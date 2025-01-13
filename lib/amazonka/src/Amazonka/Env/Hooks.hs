{-# LANGUAGE AllowAmbiguousTypes #-}

-- |
-- Module      : Amazonka.Env.Hooks
-- Copyright   : (c) 2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : highly experimental
-- Portability : non-portable (GHC extensions)
--
-- Hooks carried within an 'Env', allowing ad-hoc injection of
-- different behaviour during Amazonka's request/response cycle.
-- Hooks are currently experimental, but Amazonka uses the 'Hooks' API
-- to implement its default logging, and you can add your own
-- behaviour here as well. Some examples of things hooks can do:
--
-- * Log all requests Amazonka makes to a separate log, in order to
--   audit which IAM permissions your program actually needs
--   (see 'requestHook');
--
--   @
--   {-# LANGAUGE OverloadedLabels, ScopedTypeVariables, TypeApplications #-}
--   import Amazonka
--   import Amazonka.Env.Hooks
--   import Data.Generics.Labels ()
--   import Data.Typeable (typeRep)
--
--   main :: IO ()
--   main = do
--     -- Use 'Data.Typeable.typeRep' to capture a `TypeRep` of the request type,
--     -- so we know the request type to log. Note the `(req :: req)`
--     -- argument to the lambda, which captures the type of `req` as
--     -- a type variable `req` (needs `-XScopedTypeVariables`).
--     env <- newEnv discover
--       \<&\> #hooks %~ 'requestHook' ('addAWSRequestHook' $ \\_env (req :: req) -> req <$ logRequest ('typeRep' (Proxy @req)))
--     ...
--
--   logRequest :: AWSRequest a => a -> IO ()
--   logRequest = ...
--   @
--
-- * Inject a [Trace ID for AWS X-Ray](https://docs.aws.amazon.com/xray/latest/devguide/xray-concepts.html#xray-concepts-tracingheader),
--   into each request before it is signed and sent
--   (see 'configuredRequestHook'); and
--
--   @
--   {-# LANGAUGE OverloadedLabels #-}
--   import Amazonka
--   import Amazonka.Env.Hooks
--   import Data.Generics.Labels ()
--
--   main :: IO ()
--   main = do
--     env <- newEnv discover
--       \<&\> #hooks %~ 'configuredRequestHook' ('addHook' $ \\_env req -> req & #headers %~ addXRayIdHeader)
--     ...
--
--   -- The actual header would normally come from whatever calls into your program,
--   -- or you would randomly generate one yourself (hooks run in 'IO').
--   addXRayIdHeader :: ['Network.HTTP.Types.Header'] -> ['Network.HTTP.Types.Header']
--   addXRayIdHeader = ...
--   @
--
-- * Selectively silence certain expected errors, such as DynamoDB's
--   @ConditionalCheckFailedException@ ('errorHook' and 'silenceError')
--
--   @
--   {-# LANGAUGE OverloadedLabels #-}
--   import Amazonka
--   import Amazonka.Env.Hooks
--   import qualified Amazonka.DynamoDB as DynamoDB
--   import Data.Generics.Labels ()
--
--   main :: IO ()
--   main = do
--     env <- newEnv discover
--     putItemResponse <- runResourceT $
--       send
--         (env & #hooks %~ 'errorHook' ('silenceError' DynamoDB._ConditionalCheckFailedException))
--         (DynamoDB.newPutItem ...)
--     ...
--   @
--
-- Most functions with names ending in @Hook@ ('requestHook', etc.)
-- are intended for use with lenses: partially apply them to get a
-- function @'Hook' a -> 'Hook' a@ that can go on the RHS of @(%~)@
-- (the lens modify function). You then use functions like
-- 'addHookFor' to selectively extend the hooks used at any particular
-- time.
--
-- Names ending in @_@ ('Hook_', 'addHookFor_', etc.) concern hooks
-- that return @()@ instead of the hook's input type. These hooks
-- respond to some event but lack the ability to change Amazonka's
-- behaviour; either because it is unsafe to do so, or because it is
-- difficult to do anything meaningful with the updated value.
--
-- The request/response flow for a standard 'Amazonka.send' looks like
-- this:
--
-- @
--     send (req :: 'AWSRequest' a => a)
--                  |
--                  V
--         Run Hook: request
--                  |
--                  V
-- Amazonka: configure req into "Request a"
--  (Amazonka-specific HTTP request type)
--                  |
--                  V
--     Run Hook: configuredRequest
--                  |
--                  V
-- Amazonka: sign request, turn into standard
--     Network.HTTP.Client.'Network.HTTP.Client.Request'
--                  |
--                  +-<---------------------------------.
--                  V                                   |
--     Run Hook: signedRequest                          |
--                  |                                   |
--                  V                                   |
--     Run Hook: clientRequest                          |
--                  |                                   |
--                  V                                   |
--     Amazonka: send request to AWS           Run Hook: requestRetry
--                  |                                   ^
--                  V                                   |
--     Run Hook: clientResponse                         |
--                  |                                   |
--                  V                                   |
--     Run Hook: rawResponseBody                        |
--                  |                                   |
--                  V                                   |
--     Amazonka: was error? ------------------.         |
--                  |            Yes          |         |
--                  |                         V         |
--                  | No               Run Hook: error  |
--                  |                    ('NotFinal')     |
--                  |                         |         |
--                  +-<-----------------------\'         |
--                  V                                   |
--     Amazonka: should retry? -------------------------\'
--                  |            Yes
--                  | No
--                  V
--     Amazonka: was error? ------------------.
--                  |            Yes          |
--                  |                         V
--                  | No                      |
--                  |                         |
--     Run Hook: response              Run Hook: error
--                  |                     ('Final')
--                  |                         |
--                  V                         |
--     Amazonka: parse response               |
--                  |                         |
--                  +-<-----------------------\'
--                  V
--     Amazonka: return result
-- @
module Amazonka.Env.Hooks
  ( Hook,
    Hook_,
    Hooks (..),
    Finality (..),

    -- * Updating members of 'Hooks'
    requestHook,
    waitHook,
    configuredRequestHook,
    signedRequestHook,
    clientRequestHook,
    clientResponseHook,
    rawResponseBodyHook,
    requestRetryHook,
    awaitRetryHook,
    responseHook,
    errorHook,

    -- * Functions to use with the ones above
    noHook,
    noHook_,
    addHook,
    addHook_,
    addAWSRequestHook,
    addAWSRequestHook_,
    addHookFor,
    addHookFor_,
    removeHooksFor,
    removeHooksFor_,

    -- ** Specialty combinators
    silenceError,

    -- * Building 'Hooks'
    addLoggingHooks,
    noHooks,
  )
where

import Amazonka.Core.Lens.Internal (Getting, has)
import {-# SOURCE #-} Amazonka.Env (Env' (..))
import Amazonka.Logger (build, logDebug, logError, logTrace)
import Amazonka.Prelude hiding (error)
import Amazonka.Types
  ( AWSRequest,
    AWSResponse,
    ClientRequest,
    ClientResponse,
    Error,
    Request,
    Signed (..),
  )
import Amazonka.Waiter (Accept, Wait (..))
import qualified Control.Retry as Retry
import Data.List (intersperse)
import Data.Monoid (Any)
import Data.Typeable (Typeable, eqT, (:~:) (..))

-- | A hook that returns an updated version of its arguments.
type Hook a = forall withAuth. Env' withAuth -> a -> IO a

-- | A hook that cannot return an updated version of its argument.
type Hook_ a = forall withAuth. Env' withAuth -> a -> IO ()

-- | Indicates whether an error hook is potentially going to be
-- retried.
--
-- /See:/ 'error'
data Finality = NotFinal | Final
  deriving stock (Bounded, Enum, Eq, Ord, Show, Generic)

data Hooks = Hooks
  { -- | Called at the start of request processing, before the request
    -- is configured. This is always the first hook that runs, and
    -- argument is usually a request record type like @amazonka-s3@'s
    -- @GetObjectRequest@.
    request :: forall a. (AWSRequest a, Typeable a) => Hook a,
    -- | Called after the request has been configured into an abstract
    -- HTTP request, but before it is converted to a signed
    -- @Network.HTTP.Client.'Network.HTTP.Client.Request'@.
    --
    -- If you want to add additional headers (e.g., a
    -- [Trace ID for AWS X-Ray](https://docs.aws.amazon.com/xray/latest/devguide/xray-concepts.html#xray-concepts-tracingheader)),
    -- do it with this hook.
    configuredRequest :: forall a. (AWSRequest a, Typeable a) => Hook (Request a),
    -- | Called at the start of waiter processing, just after the
    -- request is configured.
    wait :: forall a. (AWSRequest a, Typeable a) => Hook (Wait a),
    -- | Called just after a request is signed, containing signature
    -- metadata and a
    -- @Network.HTTP.Client.'Network.HTTP.Client.Request'@.
    signedRequest :: forall a. (AWSRequest a, Typeable a) => Hook_ (Signed a),
    -- | Called on a
    -- @Network.HTTP.Client.'Network.HTTP.Client.Request'@, just
    -- before it is sent. While you can retrieve a 'ClientRequest'
    -- from the 'signedRequest' hook, this hook captures unsigned
    -- requests too.
    --
    -- Changing the contents of a signed request is highly likely to
    -- break its signature.
    clientRequest :: Hook ClientRequest,
    -- | Called on the raw
    -- @Network.HTTP.Client.'Network.HTTP.Client.Response'@, as soon
    -- as it comes back from the HTTP client. The body is replaced
    -- with @()@ to prevent its accidental consumption by hooks.
    clientResponse ::
      forall a.
      (AWSRequest a, Typeable a) =>
      Hook_ (Request a, ClientResponse ()),
    -- | Called on the raw response body, after it has been sunk from
    -- the @Network.HTTP.Client.'Network.HTTP.Client.Response'@.
    rawResponseBody :: Hook ByteStringLazy,
    -- | Called when Amazonka decides to retry a failed request. The
    -- 'Text' argument is an error code like @"http_error"@,
    -- @"request_throttled_exception"@. Check the retry check
    -- function for your particular 'Service', usually found somewhere
    -- like @Amazonka.S3.Types.defaultService@.
    requestRetry ::
      forall a.
      (AWSRequest a, Typeable a) =>
      Hook_ (Request a, Text, Retry.RetryStatus),
    -- | Called when Amazonka decides to retry a request while
    -- resolving an 'Amazonka.await' operation.
    awaitRetry ::
      forall a.
      (AWSRequest a, Typeable a) =>
      Hook_ (Request a, Wait a, Accept, Retry.RetryStatus),
    -- | Called when a response from AWS is successfully
    -- deserialised. Because the 'AWSResponse' type family is not
    -- injective, we include the original request.
    response ::
      forall a.
      (AWSRequest a, Typeable a) =>
      Hook_ (Request a, ClientResponse (AWSResponse a)),
    -- | Called whenever an AWS request returns an 'Error', even when
    -- the corresponding request is retried.
    --
    -- On the final error after all retries, this hook will be called
    -- twice: once with @NotFinal@ and once with @Final@. This
    -- behavior may change in a future version.
    error ::
      forall a.
      (AWSRequest a, Typeable a) =>
      Hook_ (Finality, Request a, Error)
  }

{-# INLINE requestHook #-}
requestHook ::
  (forall a. (AWSRequest a, Typeable a) => Hook a -> Hook a) ->
  Hooks ->
  Hooks
requestHook f hooks@Hooks {request} =
  hooks {request = f request}

{-# INLINE waitHook #-}
waitHook ::
  (forall a. (AWSRequest a) => Hook (Wait a) -> Hook (Wait a)) ->
  Hooks ->
  Hooks
waitHook f hooks@Hooks {wait} =
  hooks {wait = f wait}

{-# INLINE configuredRequestHook #-}
configuredRequestHook ::
  ( forall a.
    (AWSRequest a) =>
    Hook (Request a) ->
    Hook (Request a)
  ) ->
  Hooks ->
  Hooks
configuredRequestHook f hooks@Hooks {configuredRequest} =
  hooks {configuredRequest = f configuredRequest}

{-# INLINE signedRequestHook #-}
signedRequestHook ::
  ( forall a.
    (AWSRequest a) =>
    Hook_ (Signed a) ->
    Hook_ (Signed a)
  ) ->
  Hooks ->
  Hooks
signedRequestHook f hooks@Hooks {signedRequest} =
  hooks {signedRequest = f signedRequest}

{-# INLINE clientRequestHook #-}
clientRequestHook ::
  (Hook ClientRequest -> Hook ClientRequest) ->
  Hooks ->
  Hooks
clientRequestHook f hooks@Hooks {clientRequest} =
  hooks {clientRequest = f clientRequest}

{-# INLINE clientResponseHook #-}
clientResponseHook ::
  ( forall a.
    (AWSRequest a) =>
    Hook_ (Request a, ClientResponse ()) ->
    Hook_ (Request a, ClientResponse ())
  ) ->
  Hooks ->
  Hooks
clientResponseHook f hooks@Hooks {clientResponse} =
  hooks {clientResponse = f clientResponse}

{-# INLINE rawResponseBodyHook #-}
rawResponseBodyHook ::
  (Hook ByteStringLazy -> Hook ByteStringLazy) ->
  Hooks ->
  Hooks
rawResponseBodyHook f hooks@Hooks {rawResponseBody} =
  hooks {rawResponseBody = f rawResponseBody}

{-# INLINE requestRetryHook #-}
requestRetryHook ::
  ( forall a.
    (AWSRequest a) =>
    Hook_ (Request a, Text, Retry.RetryStatus) ->
    Hook_ (Request a, Text, Retry.RetryStatus)
  ) ->
  Hooks ->
  Hooks
requestRetryHook f hooks@Hooks {requestRetry} =
  hooks {requestRetry = f requestRetry}

{-# INLINE awaitRetryHook #-}
awaitRetryHook ::
  ( forall a.
    (AWSRequest a) =>
    Hook_ (Request a, Wait a, Accept, Retry.RetryStatus) ->
    Hook_ (Request a, Wait a, Accept, Retry.RetryStatus)
  ) ->
  Hooks ->
  Hooks
awaitRetryHook f hooks@Hooks {awaitRetry} =
  hooks {awaitRetry = f awaitRetry}

{-# INLINE responseHook #-}
responseHook ::
  ( forall a.
    (AWSRequest a) =>
    Hook_ (Request a, ClientResponse (AWSResponse a)) ->
    Hook_ (Request a, ClientResponse (AWSResponse a))
  ) ->
  Hooks ->
  Hooks
responseHook f hooks@Hooks {response} =
  hooks {response = f response}

{-# INLINE errorHook #-}
errorHook ::
  ( forall a.
    (AWSRequest a) =>
    Hook_ (Finality, Request a, Error) ->
    Hook_ (Finality, Request a, Error)
  ) ->
  Hooks ->
  Hooks
errorHook f hooks@Hooks {error} =
  hooks {error = f error}

-- | Turn a @'Hook' a@ into another @'Hook' a@ that does nothing.
--
-- @
-- -- Example: remove all request hooks:
-- requestHook noHook :: Hooks -> Hooks
-- @
noHook :: Hook a -> Hook a
noHook _ _ = pure

-- | Turn a @'Hook_' a@ into another @'Hook_' a@ that does nothing.
--
-- @
-- -- Example: Remove all response hooks:
-- responseHook noHook_ :: Hooks -> Hooks
-- @
noHook_ :: Hook_ a -> Hook_ a
noHook_ _ _ _ = pure ()

-- | Unconditionally add a @'Hook' a@ to the chain of hooks. If you
-- need to do something with specific request types, you want
-- 'addHookFor', instead.
addHook :: (Typeable a) => Hook a -> Hook a -> Hook a
addHook newHook oldHook env = oldHook env >=> newHook env

-- | Unconditionally add a @'Hook_' a@ to the chain of hooks. If you
-- need to do something with specific request types, you want
-- 'addHookFor_', instead.
addHook_ :: (Typeable a) => Hook_ a -> Hook_ a -> Hook_ a
addHook_ newHook oldHook env a = oldHook env a *> newHook env a

-- | Like 'addHook', adds an unconditional hook, but it also captures
-- the @'AWSRequest' a@ constraint. Useful for handling every AWS
-- request type in a generic way.
addAWSRequestHook :: (AWSRequest a, Typeable a) => Hook a -> Hook a -> Hook a
addAWSRequestHook = addHook

-- | 'addAWSRequestHook_' is 'addAWSRequestHook' but for 'Hook_'s.
addAWSRequestHook_ :: (AWSRequest a, Typeable a) => Hook_ a -> Hook_ a -> Hook_ a
addAWSRequestHook_ = addHook_

-- | @addHookFor \@a newHook oldHook@ When @a@ and @b@ are the same
-- type, run the given 'Hook a' after all others, otherwise only run
-- the existing hooks.
--
-- @
-- -- Example: Run @getObjectRequestHook@ on anything that is a @GetObjectRequest@:
-- requestHook (addHookFor @GetObjectRequest getObjectRequestHook) :: Hooks -> Hooks
-- @
addHookFor ::
  forall a b. (Typeable a, Typeable b) => Hook a -> Hook b -> Hook b
addHookFor newHook oldHook env =
  oldHook env >=> case eqT @a @b of
    Just Refl -> newHook env
    Nothing -> pure

-- | When @a@ and @b@ are the same type, run the given 'Hook_ a' after
-- all other hooks have run.
--
-- @
-- -- Example: Run @aSignedRequestHook@ on anything that is a @Signed GetObjectRequest@:
-- requestHook (addHookFor_ @(Signed GetObjectRequest) aSignedRequestHook) :: Hooks -> Hooks
-- @
addHookFor_ ::
  forall a b. (Typeable a, Typeable b) => Hook_ a -> Hook_ b -> Hook_ b
addHookFor_ newHook oldHook env a = do
  oldHook env a
  case eqT @a @b of
    Just Refl -> newHook env a
    Nothing -> pure ()

-- | When @a@ and @b@ are the same type, do not call any more hooks.
--
-- @
-- -- Example: Prevent any request hooks from running against a @PutObjectRequest@:
-- requestHook (removeHooksFor @PutObjectRequest) :: Hooks -> Hooks
-- @
removeHooksFor :: forall a b. (Typeable a, Typeable b) => Hook b -> Hook b
removeHooksFor oldHook env = case eqT @a @b of
  Just Refl -> pure
  Nothing -> oldHook env

-- | When @a@ and @b@ are the same type, do not call any more hooks.
--
-- @
-- -- Example: Prevent any error hooks from running against errors caused by a @PutObjectRequest@:
-- errorHook (removeHooksFor @(Finality, Request PutObjectRequest, Error)) :: Hooks -> Hooks
-- @
removeHooksFor_ :: forall a b. (Typeable a, Typeable b) => Hook_ b -> Hook_ b
removeHooksFor_ oldHook env a = case eqT @a @b of
  Just Refl -> pure ()
  Nothing -> oldHook env a

-- | Run the wrapped hook unless the given 'Fold' or 'Traversal'
-- matches the error. You will probably want to use this with the
-- error matchers defined by each service binding, allowing you to
-- selectively silence specific errors:
--
-- @
-- -- Assuming `env :: Amazonka.Env` and `putRequest :: DynamoDB.PutRequest`,
-- -- this silences a single type of error for a single call:
-- send (env & #hooks %~ errorHook (silenceError DynamoDB._ConditionalCheckFailedException))
-- @
--
-- @
-- 'silenceError' :: Getter Error e     -> 'Hook_' ('Finality', Request a, Error) -> 'Hook_' ('Finality', Request a, Error)
-- 'silenceError' :: Fold Error e       -> 'Hook_' ('Finality', Request a, Error) -> 'Hook_' ('Finality', Request a, Error)
-- 'silenceError' :: Iso' Error e       -> 'Hook_' ('Finality', Request a, Error) -> 'Hook_' ('Finality', Request a, Error)
-- 'silenceError' :: Lens' Error e      -> 'Hook_' ('Finality', Request a, Error) -> 'Hook_' ('Finality', Request a, Error)
-- 'silenceError' :: Traversal' Error e -> 'Hook_' ('Finality', Request a, Error) -> 'Hook_' ('Finality', Request a, Error)
-- @
silenceError ::
  Getting Any Error e ->
  Hook_ (Finality, Request a, Error) ->
  Hook_ (Finality, Request a, Error)
silenceError g oldHook env t@(_, _, err) =
  if has g err then pure () else oldHook env t

-- | Add default logging hooks. The default 'Env'' from
-- 'Amazonka.Env.newEnv' already has logging hooks installed, so you
-- probably only want this if you are building your own 'Hooks' from
-- scratch.
addLoggingHooks :: Hooks -> Hooks
addLoggingHooks
  hooks@Hooks
    { signedRequest,
      clientRequest,
      clientResponse,
      rawResponseBody,
      requestRetry,
      awaitRetry,
      error
    } =
    hooks
      { signedRequest = \env@Env {logger} s@Signed {signedMeta} -> do
          signedRequest env s
          logTrace logger signedMeta,
        clientRequest = \env@Env {logger} rq -> do
          rq' <- clientRequest env rq
          rq' <$ logDebug logger rq',
        clientResponse = \env@Env {logger} t@(_, rs) -> do
          clientResponse env t
          logDebug logger rs,
        rawResponseBody = \env@Env {logger} body -> do
          body' <- rawResponseBody env body
          body' <$ logTrace logger ("[Raw Response Body] {\n" <> body' <> "\n}"),
        requestRetry = \env@Env {logger} t@(_, name, retryStatus) -> do
          requestRetry env t
          logDebug logger
            . munwords
            $ [ "[Retry " <> build name <> "]",
                "after",
                build (Retry.rsIterNumber retryStatus + 1),
                "attempt(s)."
              ],
        awaitRetry = \env@Env {logger} t@(_, Wait {name}, accept, retryStatus) -> do
          awaitRetry env t
          logDebug logger
            . munwords
            $ [ "[Await " <> build name <> "]",
                build accept,
                "after",
                build (Retry.rsIterNumber retryStatus + 1),
                "attempts."
              ],
        error = \env@Env {logger} t@(finality, _, err) -> do
          error env t
          case finality of
            NotFinal -> logDebug logger err
            Final -> logError logger err
      }
    where
      munwords = mconcat . intersperse " "

-- | Empty 'Hooks' structure which returns everything unmodified.
noHooks :: Hooks
noHooks =
  Hooks
    { request = const pure,
      configuredRequest = const pure,
      wait = const pure,
      signedRequest = \_ _ -> pure (),
      clientRequest = const pure,
      clientResponse = \_ _ -> pure (),
      rawResponseBody = const pure,
      requestRetry = \_ _ -> pure (),
      awaitRetry = \_ _ -> pure (),
      response = \_ _ -> pure (),
      error = \_ _ -> pure ()
    }
