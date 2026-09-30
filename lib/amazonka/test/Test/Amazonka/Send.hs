-- |
-- Module      : Test.Amazonka.Send
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Send (tests) where

import Amazonka hiding (accept, error, runResourceT)
import qualified Amazonka.Auth as Auth
import qualified Amazonka.Data as Data
import qualified Amazonka.DynamoDB as DynamoDB
import qualified Amazonka.Env.Hooks as Hooks
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import qualified Amazonka.STS as STS
import qualified Amazonka.Waiter as Waiter
import Control.Concurrent (ThreadId, forkFinally, forkIO, killThread)
import Control.DeepSeq (NFData (..))
import Control.Exception (ErrorCall (..), SomeException, bracket, displayException, try)
import Control.Monad (replicateM_, void)
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.Conduit as Conduit
import qualified Data.Conduit.List as Conduit.List
import Data.Foldable (for_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Network.HTTP.Client as Client
import Network.Socket
  ( Family (AF_INET),
    SockAddr (SockAddrInet),
    Socket,
    SocketOption (ReuseAddr),
    SocketType (Stream),
    accept,
    bind,
    close,
    defaultProtocol,
    getSocketName,
    listen,
    setSocketOption,
    socket,
    tupleToHostAddress,
    withSocketsDo,
  )
import qualified Network.Socket.ByteString as Socket
import qualified System.Timeout as Timeout
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)
import Prelude

data DeepProbe = DeepProbe

instance Data.ToPath DeepProbe where
  toPath _ = "/"

instance Data.ToQuery DeepProbe where
  toQuery _ = mempty

instance Data.ToHeaders DeepProbe where
  toHeaders _ = mempty

data DeepResponse = DeepResponse () LazyPayload

instance NFData DeepResponse where
  rnf (DeepResponse metadata payload) = rnf metadata `seq` rnf payload

newtype LazyPayload = LazyPayload Int

instance NFData LazyPayload where
  rnf (LazyPayload value) = rnf value

instance AWSRequest DeepProbe where
  type AWSResponse DeepProbe = DeepResponse

  evaluateResponse _ = rnf

  request overrides = Request.get (overrides STS.defaultService)

  response =
    Response.receiveBytes $ \_ _ _ ->
      Right (DeepResponse () (LazyPayload (error "deep response was forced")))

instance AWSPager DeepProbe where
  page _ _ = Nothing

data ShallowProbe = ShallowProbe

newtype ShallowResponse = ShallowResponse (IO ())

instance Data.ToPath ShallowProbe where
  toPath _ = "/"

instance Data.ToQuery ShallowProbe where
  toQuery _ = mempty

instance Data.ToHeaders ShallowProbe where
  toHeaders _ = mempty

instance AWSRequest ShallowProbe where
  type AWSResponse ShallowProbe = ShallowResponse

  request overrides = Request.get (overrides STS.defaultService)

  response =
    Response.receiveBytes $ \_ _ _ ->
      Right (ShallowResponse (pure ()))

data FailureProbe = FailureProbe

instance Data.ToPath FailureProbe where
  toPath _ = "/"

instance Data.ToQuery FailureProbe where
  toQuery _ = mempty

instance Data.ToHeaders FailureProbe where
  toHeaders _ = mempty

instance AWSRequest FailureProbe where
  type AWSResponse FailureProbe = ()

  evaluateResponse _ _ = error "failed response was evaluated"

  request overrides = Request.get (overrides STS.defaultService)

  response =
    Response.receiveBytes $ \_ _ _ ->
      Left "expected probe parse failure"

newtype HookProbe = HookProbe
  { forceHookResponse :: Bool
  }

instance Data.ToPath HookProbe where
  toPath _ = "/"

instance Data.ToQuery HookProbe where
  toQuery _ = mempty

instance Data.ToHeaders HookProbe where
  toHeaders _ = mempty

instance AWSRequest HookProbe where
  type AWSResponse HookProbe = DeepResponse

  evaluateResponse HookProbe {forceHookResponse} result
    | forceHookResponse = rnf result
    | otherwise = result `seq` ()

  request overrides = Request.get (overrides STS.defaultService)

  response =
    Response.receiveBytes $ \_ _ _ ->
      Right (DeepResponse () (LazyPayload (error "deep response was forced")))

data StreamingProbe = StreamingProbe

newtype StreamingResponse = StreamingResponse ResponseBody

instance Data.ToPath StreamingProbe where
  toPath _ = "/"

instance Data.ToQuery StreamingProbe where
  toQuery _ = mempty

instance Data.ToHeaders StreamingProbe where
  toHeaders _ = mempty

instance AWSRequest StreamingProbe where
  type AWSResponse StreamingProbe = StreamingResponse

  request overrides = Request.get (overrides STS.defaultService)

  response =
    Response.receiveBody $ \_ _ body ->
      Right (StreamingResponse body)

data WaiterProbe = WaiterProbe

data WaiterResponse = WaiterResponse Bool LazyPayload

instance NFData WaiterResponse where
  rnf (WaiterResponse retry payload) = rnf retry `seq` rnf payload

instance Data.ToPath WaiterProbe where
  toPath _ = "/"

instance Data.ToQuery WaiterProbe where
  toQuery _ = mempty

instance Data.ToHeaders WaiterProbe where
  toHeaders _ = mempty

instance AWSRequest WaiterProbe where
  type AWSResponse WaiterProbe = WaiterResponse

  evaluateResponse _ = rnf

  request overrides = Request.get (overrides STS.defaultService)

  response =
    Response.receiveBytes $ \_ _ body ->
      Right $
        if body == "retry"
          then WaiterResponse True (LazyPayload 0)
          else WaiterResponse False (LazyPayload (error "deep response was forced"))

tests :: TestTree
tests =
  testGroup
    "Send response evaluation"
    [ testCase "sendUnsignedEither evaluates the selected response policy" $
        withTestServer $ \port -> do
          assertDeepResponseForced $
            withEnv port $ \env ->
              runResourceT $ void (sendUnsignedEither env DeepProbe),
      testCase "sendEither evaluates the selected response policy" $
        withTestServer $ \port -> do
          assertDeepResponseForced $
            withSignedEnv port $ \env ->
              runResourceT $ void (sendEither env DeepProbe),
      testCase "discarded DynamoDB PutItem successes do not stall" $
        withTestServerResponses
          (replicate dynamoDBRequestCount dynamoDBSuccessfulResponse)
          $ \port ->
            assertCompletes "discarded successful PutItem responses stalled" $
              withDynamoDBEnv port $ \env ->
                runResourceT $
                  replicateM_ dynamoDBRequestCount $
                    void (send env (DynamoDB.newPutItem "test-table")),
      testCase "discarded DynamoDB PutItem errors do not stall" $
        withTestServerResponses
          (replicate dynamoDBRequestCount dynamoDBErrorResponse)
          $ \port ->
            assertCompletes "discarded failed PutItem responses stalled" $
              withDynamoDBEnv port $ \env ->
                replicateM_ dynamoDBRequestCount $
                  void $
                    try @Error $
                      runResourceT $
                        void (send env (DynamoDB.newPutItem "test-table")),
      testCase "sendEither uses the request returned by request hooks" $
        withTestServer $ \port -> do
          assertDeepResponseForced $
            withSignedEnv port $ \env ->
              runResourceT $
                void
                  ( sendEither
                      (withHookProbeForcing env)
                      HookProbe {forceHookResponse = False}
                  ),
      testCase "response hooks run before response evaluation" $
        withTestServer $ \port -> do
          hookRan <- newIORef False
          assertDeepResponseForced $
            withSignedEnv port $ \env ->
              runResourceT $
                void (sendEither (withResponseObservation hookRan env) DeepProbe)
          readIORef hookRan >>= assertEqual "response hook did not run" True,
      testCase "paginateEither evaluates responses before yielding pages" $
        withTestServer $ \port -> do
          assertDeepResponseForced $
            withSignedEnv port $ \env ->
              void . runResourceT $
                Conduit.runConduit
                  ( void (paginateEither env DeepProbe)
                      Conduit..| Conduit.awaitForever (const (pure ()))
                  ),
      testCase "retryRequest evaluates the eventual successful response"
        $ withTestServerResponses
          [ serverResponse
              "500 Internal Server Error"
              "<ErrorResponse><Error><Code>InternalFailure</Code><Message>retry</Message></Error><RequestId>request-id</RequestId></ErrorResponse>",
            successfulResponse
          ]
        $ \port -> do
          assertDeepResponseForced $
            withRetryingEnv port $ \env ->
              runResourceT $ void (sendEither env DeepProbe),
      testCase "awaitEither evaluates successful responses before acceptors" $
        withTestServer $ \port -> do
          assertDeepResponseForced $
            withSignedEnv port $ \env ->
              runResourceT $ void (awaitEither env successfulWait DeepProbe),
      testCase "awaitEither uses the request returned by request hooks" $
        withTestServer $ \port -> do
          assertDeepResponseForced $
            withSignedEnv port $ \env ->
              runResourceT $
                void
                  ( awaitEither
                      (withHookProbeForcing env)
                      hookWait
                      HookProbe {forceHookResponse = False}
                  ),
      testCase "awaitEither evaluates every successful retry response"
        $ withTestServerResponses
          [serverResponse "200 OK" "retry", serverResponse "200 OK" "complete"]
        $ \port -> do
          assertDeepResponseForced $
            withSignedEnv port $ \env ->
              runResourceT $ void (awaitEither env retryWait WaiterProbe),
      testCase "failed sends do not evaluate the response policy" $
        withTestServer $ \port -> do
          result <- withEnv port $ \env ->
            runResourceT $ sendUnsignedEither env FailureProbe
          case result of
            Left _ -> pure ()
            Right () -> assertFailure "expected response parsing to fail",
      testCase "failed waiter attempts do not evaluate the response policy" $
        withTestServer $ \port -> do
          result <- withSignedEnv port $ \env ->
            runResourceT $ awaitEither env failedWait FailureProbe
          case result of
            Right Waiter.AcceptSuccess -> pure ()
            Right waiterResult ->
              assertFailure $
                "expected waiter success, got "
                  <> show waiterResult
            Left exception ->
              assertFailure $
                "expected the waiter acceptor to handle the failed response: "
                  <> displayException exception,
      testCase "the default response policy does not require NFData" $
        withTestServer $ \port -> do
          result <- try @SomeException $ withEnv port $ \env ->
            runResourceT $ void (sendUnsignedEither env ShallowProbe)
          case result of
            Left exception ->
              assertFailure $
                "expected the default response policy to succeed: "
                  <> displayException exception
            Right () -> pure (),
      testCase "the default response policy preserves streaming responses" $
        withTestServer $ \port -> do
          result <- try @SomeException $ withEnv port $ \env -> runResourceT $ do
            sendUnsignedEither env StreamingProbe >>= \case
              Left exception -> pure (Left exception)
              Right (StreamingResponse body) ->
                Right . ByteString.concat <$> sinkBody body Conduit.List.consume
          case result of
            Left exception ->
              assertFailure $
                "expected the streaming response policy to succeed: "
                  <> displayException exception
            Right (Left exception) ->
              assertFailure $
                "expected the streaming request to succeed: "
                  <> displayException exception
            Right (Right body) ->
              assertEqual "unexpected streaming response body" "ok" body
    ]

successfulWait :: Waiter.Wait DeepProbe
successfulWait =
  Waiter.Wait
    { Waiter.name = "deep-probe",
      Waiter.attempts = 1,
      Waiter.delay = 0,
      Waiter.acceptors = [\_ _ -> Just Waiter.AcceptSuccess]
    }

hookWait :: Waiter.Wait HookProbe
hookWait =
  Waiter.Wait
    { Waiter.name = "hook-probe",
      Waiter.attempts = 1,
      Waiter.delay = 0,
      Waiter.acceptors = [\_ _ -> Just Waiter.AcceptSuccess]
    }

retryWait :: Waiter.Wait WaiterProbe
retryWait =
  Waiter.Wait
    { Waiter.name = "retry-probe",
      Waiter.attempts = 2,
      Waiter.delay = 0,
      Waiter.acceptors =
        [ \_ -> \case
            Right clientResponse ->
              case Client.responseBody clientResponse of
                WaiterResponse True _ -> Just Waiter.AcceptRetry
                WaiterResponse False _ -> Just Waiter.AcceptSuccess
            Left _ -> Just Waiter.AcceptFailure
        ]
    }

failedWait :: Waiter.Wait FailureProbe
failedWait =
  Waiter.Wait
    { Waiter.name = "failed-probe",
      Waiter.attempts = 1,
      Waiter.delay = 0,
      Waiter.acceptors =
        [ \_ -> \case
            Left _ -> Just Waiter.AcceptSuccess
            Right _ -> Nothing
        ]
    }

assertDeepResponseForced :: IO () -> IO ()
assertDeepResponseForced action = do
  result <- try @ErrorCall action
  case result of
    Left (ErrorCall message) ->
      assertEqual "unexpected evaluation exception" "deep response was forced" message
    Right () -> assertFailure "expected deep response evaluation to throw"

assertCompletes :: String -> IO a -> IO ()
assertCompletes failureMessage action =
  Timeout.timeout responseTimeout action >>= \case
    Nothing -> assertFailure failureMessage
    Just _ -> pure ()

withEnv :: Int -> (EnvNoAuth -> IO a) -> IO a
withEnv = withEnvUsing once

withRetryingEnv :: Int -> (Env -> IO a) -> IO a
withRetryingEnv port action =
  withEnvUsing id port $
    action
      . Auth.fromKeys
        (AccessKey "test-access-key")
        (SecretKey "test-secret-key")

withEnvUsing :: (EnvNoAuth -> EnvNoAuth) -> Int -> (EnvNoAuth -> IO a) -> IO a
withEnvUsing configure port action = do
  manager <- Client.newManager Client.defaultManagerSettings
  env <- newEnvNoAuthFromManager manager
  let service = setEndpoint False "127.0.0.1" port STS.defaultService
  action (configure (configureService service env))

withSignedEnv :: Int -> (Env -> IO a) -> IO a
withSignedEnv port action =
  withEnv port $
    action
      . Auth.fromKeys
        (AccessKey "test-access-key")
        (SecretKey "test-secret-key")

withDynamoDBEnv :: Int -> (Env -> IO a) -> IO a
withDynamoDBEnv port action = do
  manager <-
    Client.newManager
      Client.defaultManagerSettings
        { Client.managerConnCount = 1
        }
  env <- newEnvNoAuthFromManager manager
  let service = setEndpoint False "127.0.0.1" port DynamoDB.defaultService
  action $
    Auth.fromKeys
      (AccessKey "test-access-key")
      (SecretKey "test-secret-key")
      (configureService service env)

withHookProbeForcing :: Env -> Env
withHookProbeForcing env =
  env
    { hooks =
        Hooks.requestHook
          ( Hooks.addRequestHookFor @HookProbe $ \_ hookProbe ->
              pure hookProbe {forceHookResponse = True}
          )
          (hooks env)
    }

withResponseObservation :: IORef Bool -> Env -> Env
withResponseObservation observed env =
  env
    { hooks =
        Hooks.responseHook
          ( Hooks.addResponseHookFor @DeepProbe $ \_ _ ->
              writeIORef observed True
          )
          (hooks env)
    }

data TestServer = TestServer
  { serverSocket :: Socket,
    serverThread :: ThreadId,
    serverPort :: Int
  }

withTestServer :: (Int -> IO a) -> IO a
withTestServer = withTestServerResponses [successfulResponse]

withTestServerResponses :: [ByteString.ByteString] -> (Int -> IO a) -> IO a
withTestServerResponses responses action =
  withSocketsDo $
    bracket (startServer responses) stopServer (action . serverPort)

startServer :: [ByteString.ByteString] -> IO TestServer
startServer responses = do
  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
  listen sock (max 1 (Prelude.length responses))
  SockAddrInet port _ <- getSocketName sock
  thread <- forkIO $
    for_ responses $ \serverReply -> do
      (connection, _) <- accept sock
      void . forkFinally (serve serverReply connection) $ const (close connection)
  pure
    TestServer
      { serverSocket = sock,
        serverThread = thread,
        serverPort = fromIntegral port
      }

stopServer :: TestServer -> IO ()
stopServer server = do
  killThread (serverThread server)
  close (serverSocket server)

serve :: ByteString.ByteString -> Socket -> IO ()
serve serverReply connection = do
  receiveHeaders ByteString.empty
  Socket.sendAll connection serverReply
  where
    receiveHeaders buffered
      | "\r\n\r\n" `ByteString.isInfixOf` buffered = pure ()
      | otherwise = do
          chunk <- Socket.recv connection 4096
          if ByteString.null chunk
            then pure ()
            else receiveHeaders (buffered <> chunk)

successfulResponse :: ByteString.ByteString
successfulResponse = serverResponse "200 OK" "ok"

dynamoDBRequestCount :: Int
dynamoDBRequestCount = 20

responseTimeout :: Int
responseTimeout = 10 * 1000 * 1000

dynamoDBSuccessfulResponse :: ByteString.ByteString
dynamoDBSuccessfulResponse = serverResponse "200 OK" "{}"

dynamoDBErrorResponse :: ByteString.ByteString
dynamoDBErrorResponse =
  serverResponse
    "400 Bad Request"
    "{\"__type\":\"ValidationException\",\"message\":\"expected test failure\"}"

serverResponse :: ByteString.ByteString -> ByteString.ByteString -> ByteString.ByteString
serverResponse status body =
  "HTTP/1.1 "
    <> status
    <> "\r\nContent-Length: "
    <> ByteString.Char8.pack (show (ByteString.length body))
    <> "\r\nConnection: close\r\n\r\n"
    <> body
