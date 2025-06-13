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
--       \<&\> #hooks %~ 'requestHook'
--         ('addRequestHook' $ \\_env (req :: req) -> req <$ logRequest ('typeRep' (Proxy @req)))
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
--       \<&\> #hooks %~ 'configuredRequestHook'
--         ('addConfiguredRequestHook' $ \\_env req -> req & #headers %~ addXRayIdHeader)
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
--         (env & #hooks %~ 'errorHook'
--           ('silenceError' DynamoDB._ConditionalCheckFailedException))
--         (DynamoDB.newPutItem ...)
--     ...
--   @
--
-- =Function Conventions
--
-- * Functions named @...Hook@ ('requestHook', etc.) are intended for
--   use with lens operators: partially apply them to get a function
--   @'Hooks' -> 'Hooks'@ that can go on the RHS of @(%~)@ (the lens
--   modify function). Use functions like 'addRequestHookFor' to
--   selectively adjust the hook at a particular field.
--
-- * Function names ending in @For@ ('addRequestHookFor',
--   'addConfiguredRequestHookFor', 'removeRequestHooksFor', ...) are
--   designed to work on a single AWS request type. You will need to
--   pin this type with a type application in @remove...HooksFor@
--   functions, and you may want to do so in @add...HooksFor@
--   functions for consistency.
--
-- =Request/Response Flow
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

    -- * Composing hooks

    -- ** Functions for specific field hooks

    -- *** @requestHook@
    addRequestHook,
    addRequestHookFor,
    removeRequestHooksFor,

    -- *** @configuredRequestHook@
    addConfiguredRequestHook,
    addConfiguredRequestHookFor,
    removeConfiguredRequestHooksFor,

    -- *** @waitHook@
    addWaitHook,
    addWaitHookFor,
    removeWaitHooksFor,

    -- *** @signedRequestHook@
    addSignedRequestHook,
    addSignedRequestHookFor,
    removeSignedRequestHooksFor,

    -- *** @clientRequestHook@
    addClientRequestHook,
    removeClientRequestHooks,

    -- *** @clientResponseHook@
    addClientResponseHook,
    addClientResponseHookFor,
    removeClientResponseHooksFor,

    -- *** @rawResponseBodyHook@
    addRawResponseBodyHook,
    removeRawResponseBodyHooks,

    -- *** @requestRetryHook@
    addRequestRetryHook,
    addRequestRetryHookFor,
    removeRequestRetryHooksFor,

    -- *** @awaitRetryHook@
    addAwaitRetryHook,
    addAwaitRetryHookFor,
    removeAwaitRetryHooksFor,

    -- *** @responseHook@
    addResponseHook,
    addResponseHookFor,
    removeResponseHooksFor,

    -- *** @errorHook@
    addErrorHook,
    addErrorHookFor,
    silenceError,
    removeErrorHooksFor,

    -- ** Functions for any hook
    noHook,
    noHook_,

    -- * Building 'Hooks'
    addLoggingHooks,
    noHooks,

    -- * Deprecated functions
    addHook,
    addHook_,
    addAWSRequestHook,
    addAWSRequestHook_,
    addHookFor,
    addHookFor_,
    removeHooksFor,
    removeHooksFor_,
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
--
-- @since 2.0
type Hook a = forall withAuth. Env' withAuth -> a -> IO a

-- | A hook that cannot return an updated version of its argument.
--
-- These hooks respond to some event but lack the ability to change
-- Amazonka's behaviour; either because it is unsafe to do so, or
-- because it is difficult to do anything meaningful with the updated
-- value.
--
-- @since 2.0
type Hook_ a = forall withAuth. Env' withAuth -> a -> IO ()

-- | Indicates whether an error hook is potentially going to be
-- retried.
--
-- /See:/ 'error'
--
-- @since 2.0
data Finality = NotFinal | Final
  deriving stock (Bounded, Enum, Eq, Ord, Show, Generic)

-- | The collection of lifecycle hooks stored in an 'Amazonka.Env.Env'.
--
-- @since 2.0
data Hooks = Hooks
  { -- | Called at the start of request processing, before the request
    -- is configured. This is always the first hook that runs, and its
    -- argument is usually a request record type like @amazonka-s3@'s
    -- @GetObjectRequest@.
    request :: forall a. (AWSRequest a) => Hook a,
    -- | Called after the request has been configured into an abstract
    -- HTTP request, but before it is converted to a signed
    -- @Network.HTTP.Client.'Network.HTTP.Client.Request'@.
    --
    -- If you want to add additional headers (e.g., a
    -- [Trace ID for AWS X-Ray](https://docs.aws.amazon.com/xray/latest/devguide/xray-concepts.html#xray-concepts-tracingheader)),
    -- do it with this hook.
    configuredRequest :: forall a. (AWSRequest a) => Hook (Request a),
    -- | Called at the start of waiter processing, just after the
    -- request is configured.
    wait :: forall a. (AWSRequest a) => Hook (Wait a),
    -- | Called just after a request is signed, containing signature
    -- metadata and a
    -- @Network.HTTP.Client.'Network.HTTP.Client.Request'@.
    signedRequest :: forall a. (AWSRequest a) => Hook_ (Signed a),
    -- | Called on a
    -- @Network.HTTP.Client.'Network.HTTP.Client.Request'@, just
    -- before it is sent. While you can retrieve a 'ClientRequest'
    -- from the @signedRequest@ hook, this hook captures unsigned
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
      (AWSRequest a) =>
      Hook_ (Request a, ClientResponse ()),
    -- | Called on the raw response body, after it has been sunk from
    -- the @Network.HTTP.Client.'Network.HTTP.Client.Response'@.
    rawResponseBody :: Hook ByteStringLazy,
    -- | Called when Amazonka decides to retry a failed request. The
    -- 'Text' argument is an error code like @"http_error"@ or
    -- @"request_throttled_exception"@. Check the retry check function
    -- for your particular 'Service', usually found somewhere like
    -- @Amazonka.S3.Types.defaultService@.
    requestRetry ::
      forall a.
      (AWSRequest a) =>
      Hook_ (Request a, Text, Retry.RetryStatus),
    -- | Called when Amazonka decides to retry a request while
    -- resolving an 'Amazonka.await' operation.
    awaitRetry ::
      forall a.
      (AWSRequest a) =>
      Hook_ (Request a, Wait a, Accept, Retry.RetryStatus),
    -- | Called when a response from AWS is successfully
    -- deserialised. Because the 'AWSResponse' type family is not
    -- injective, we include the original request.
    response ::
      forall a.
      (AWSRequest a) =>
      Hook_ (Request a, ClientResponse (AWSResponse a)),
    -- | Called whenever an AWS request returns an 'Error', even when
    -- the corresponding request is retried.
    --
    -- On the final error after all retries, this hook will be called
    -- twice: once with @NotFinal@ and once with @Final@. This
    -- behavior may change in a future version.
    error ::
      forall a.
      (AWSRequest a) =>
      Hook_ (Finality, Request a, Error)
  }

-- | @since 2.0
requestHook ::
  (forall a. (AWSRequest a) => Hook a -> Hook a) ->
  Hooks ->
  Hooks
requestHook f hooks@Hooks {request} =
  hooks {request = f request}
{-# INLINE requestHook #-}

-- | @since 2.0
waitHook ::
  (forall a. (AWSRequest a) => Hook (Wait a) -> Hook (Wait a)) ->
  Hooks ->
  Hooks
waitHook f hooks@Hooks {wait} =
  hooks {wait = f wait}
{-# INLINE waitHook #-}

-- | @since 2.0
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
{-# INLINE configuredRequestHook #-}

-- | @since 2.0
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
{-# INLINE signedRequestHook #-}

-- | @since 2.0
clientRequestHook ::
  (Hook ClientRequest -> Hook ClientRequest) ->
  Hooks ->
  Hooks
clientRequestHook f hooks@Hooks {clientRequest} =
  hooks {clientRequest = f clientRequest}
{-# INLINE clientRequestHook #-}

-- | @since 2.0
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
{-# INLINE clientResponseHook #-}

-- | @since 2.0
rawResponseBodyHook ::
  (Hook ByteStringLazy -> Hook ByteStringLazy) ->
  Hooks ->
  Hooks
rawResponseBodyHook f hooks@Hooks {rawResponseBody} =
  hooks {rawResponseBody = f rawResponseBody}
{-# INLINE rawResponseBodyHook #-}

-- | @since 2.0
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
{-# INLINE requestRetryHook #-}

-- | @since 2.0
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
{-# INLINE awaitRetryHook #-}

-- | @since 2.0
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
{-# INLINE responseHook #-}

-- | @since 2.0
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
{-# INLINE errorHook #-}

-- | Add a hook for every AWS request type. Designed to be used with
-- 'requestHook'.
--
-- @
-- -- Example: A hook modification that logs every AWS request,
-- -- where `logRequest :: AWSRequest a => Hook a`.
-- requestHook (addRequestHook logRequest) :: Hooks -> Hooks
-- @
--
-- @since 2.1
addRequestHook :: (AWSRequest a) => Hook a -> Hook a -> Hook a
addRequestHook = addHook

-- | Add a hook for one specific AWS request type. Designed to be used
-- with 'requestHook'.
--
-- @
-- -- Example: Run @getObjectRequestHook@ on anything that is a @GetObjectRequest@
-- -- Assumes getObjectRequestHook :: Hook GetObjectRequest
-- requestHook (addRequestHookFor getObjectRequestHook) :: Hooks -> Hooks
-- @
--
-- @since 2.1
addRequestHookFor ::
  forall a b. (AWSRequest a, AWSRequest b) => Hook a -> Hook b -> Hook b
addRequestHookFor = addHookFor

-- | Remove all hooks for one AWS request type. Designed to be used
-- with 'requestHook'.
--
-- @
-- -- Example: Don't run 'request' hooks on a @GetObjectRequest@:
-- requestHook (removeRequestHooksFor @GetObjectRequest) :: Hooks -> Hooks
-- @
--
-- @since 2.1
removeRequestHooksFor ::
  forall a b. (AWSRequest a, AWSRequest b) => Hook b -> Hook b
removeRequestHooksFor = removeHooksFor @a

-- | Add a hook for every configured AWS request type. Designed
-- to be used with 'configuredRequestHook'.
--
-- @since 2.1
addConfiguredRequestHook ::
  (AWSRequest a) => Hook (Request a) -> Hook (Request a) -> Hook (Request a)
addConfiguredRequestHook = addHook

-- | Add a hook for one specific configured AWS request type. Designed
-- to be used with 'configuredRequestHook'.
--
-- @since 2.1
addConfiguredRequestHookFor ::
  (AWSRequest a, AWSRequest b) =>
  Hook (Request a) ->
  Hook (Request b) ->
  Hook (Request b)
addConfiguredRequestHookFor = addHookFor

-- | Remove all hooks for one specific type of configured
-- request. Designed to be used with 'configuredRequestHook'.
--
-- @since 2.1
removeConfiguredRequestHooksFor ::
  forall a b.
  (AWSRequest a, AWSRequest b) =>
  Hook (Request b) ->
  Hook (Request b)
removeConfiguredRequestHooksFor = removeHooksFor @(Request a)

-- | Add a hook for every @'Wait' a@ configuration. Designed to
-- be used with 'waitHook'.
--
-- @since 2.1
addWaitHook ::
  (AWSRequest a) => Hook (Wait a) -> Hook (Wait a) -> Hook (Wait a)
addWaitHook = addHook

-- | Add a hook for one specific @'Wait' a@ configuration. Designed to
-- be used with 'waitHook'.
--
-- @since 2.1
addWaitHookFor ::
  (AWSRequest a, AWSRequest b) =>
  Hook (Wait a) ->
  Hook (Wait b) ->
  Hook (Wait b)
addWaitHookFor = addHookFor

-- | Remove all hooks for one specific type of @'Wait' a@. Designed to
-- be used with 'waitHook'.
--
-- @since 2.1
removeWaitHooksFor ::
  forall a b.
  (AWSRequest a, AWSRequest b) =>
  Hook (Wait b) ->
  Hook (Wait b)
removeWaitHooksFor = removeHooksFor @(Wait a)

-- | Add a hook for every @'Signed' a@ request. Designed to be used
-- with 'signedRequestHook'.
--
-- @since 2.1
addSignedRequestHook ::
  (AWSRequest a) => Hook_ (Signed a) -> Hook_ (Signed a) -> Hook_ (Signed a)
addSignedRequestHook = addHook_

-- | Add a hook for one specific @'Signed' a@ request type. Designed
-- to be used with 'signedRequestHook'.
--
-- @since 2.1
addSignedRequestHookFor ::
  (AWSRequest a, AWSRequest b) =>
  Hook_ (Signed a) ->
  Hook_ (Signed b) ->
  Hook_ (Signed b)
addSignedRequestHookFor = addHookFor_

-- | Remove all hooks for one specific type of @'Signed' a@
-- request. Designed to be used with 'signedRequestHook'.
--
-- @since 2.1
removeSignedRequestHooksFor ::
  forall a b.
  (AWSRequest a, AWSRequest b) =>
  Hook_ (Signed b) ->
  Hook_ (Signed b)
removeSignedRequestHooksFor = removeHooksFor_ @(Signed a)

-- | Add a hook for 'ClientRequest'. This is an alias for 'addHook',
-- provided for consistency and designed to be used with
-- 'clientRequestHook'.
--
-- @since 2.1
addClientRequestHook ::
  Hook ClientRequest -> Hook ClientRequest -> Hook ClientRequest
addClientRequestHook = addHook

-- | Remove all hooks for 'ClientRequest'. This is an alias for
-- 'noHook', provided for consistency and designed to be used with
-- 'clientRequestHook'.
--
-- @since 2.1
removeClientRequestHooks :: Hook ClientRequest -> Hook ClientRequest
removeClientRequestHooks = noHook

-- | Add a hook for every type of client response. Designed to
-- be used with 'clientResponseHook'.
--
-- @since 2.1
addClientResponseHook ::
  (AWSRequest a) =>
  Hook_ (Request a, ClientResponse ()) ->
  Hook_ (Request a, ClientResponse ()) ->
  Hook_ (Request a, ClientResponse ())
addClientResponseHook = addHook_

-- | Add a hook for one specific type of client response. Designed to
-- be used with 'clientResponseHook'.
--
-- @since 2.1
addClientResponseHookFor ::
  (AWSRequest a, AWSRequest b) =>
  Hook_ (Request a, ClientResponse ()) ->
  Hook_ (Request b, ClientResponse ()) ->
  Hook_ (Request b, ClientResponse ())
addClientResponseHookFor = addHookFor_

-- | Remove all hooks for one specific type of client
-- response. Designed to be used with 'clientResponseHook'.
--
-- @since 2.1
removeClientResponseHooksFor ::
  forall a b.
  (AWSRequest a, AWSRequest b) =>
  Hook_ (Request b, ClientResponse ()) ->
  Hook_ (Request b, ClientResponse ())
removeClientResponseHooksFor = removeHooksFor_ @(Request a, ClientResponse ())

-- | Add a hook for the raw response body. This is an alias for
-- 'addHook', provided for consistency and designed to be used with
-- 'rawResponseBodyHook'.
--
-- @since 2.1
addRawResponseBodyHook ::
  Hook ByteStringLazy -> Hook ByteStringLazy -> Hook ByteStringLazy
addRawResponseBodyHook = addHook

-- | Remove all hooks for the raw response body. This is an alias for
-- 'noHook', provided for consistency and designed to be used with
-- 'rawResponseBodyHook'.
--
-- @since 2.1
removeRawResponseBodyHooks :: Hook ByteStringLazy -> Hook ByteStringLazy
removeRawResponseBodyHooks = noHook

-- | Add a request retry hook for every AWS request type. Designed to
-- be used with 'requestRetryHook'.
--
-- @since 2.1
addRequestRetryHook ::
  (AWSRequest a) =>
  Hook_ (Request a, Text, Retry.RetryStatus) ->
  Hook_ (Request a, Text, Retry.RetryStatus) ->
  Hook_ (Request a, Text, Retry.RetryStatus)
addRequestRetryHook = addHook_

-- | Add a request retry hook for one specific AWS request
-- type. Designed to be used with 'requestRetryHook'.
--
-- @since 2.1
addRequestRetryHookFor ::
  forall a b.
  (AWSRequest a, AWSRequest b) =>
  Hook_ (Request a, Text, Retry.RetryStatus) ->
  Hook_ (Request b, Text, Retry.RetryStatus) ->
  Hook_ (Request b, Text, Retry.RetryStatus)
addRequestRetryHookFor = addHookFor_

-- | Remove all request retry hooks for one specific AWS request
-- type. Designed to be used with 'requestRetryHook'.
--
-- @since 2.1
removeRequestRetryHooksFor ::
  forall a b.
  (AWSRequest a, AWSRequest b) =>
  Hook_ (Request b, Text, Retry.RetryStatus) ->
  Hook_ (Request b, Text, Retry.RetryStatus)
removeRequestRetryHooksFor =
  removeHooksFor_ @(Request a, Text, Retry.RetryStatus)

-- | Add an await retry hook for every AWS request type. Designed to
-- be used with 'awaitRetryHook'.
--
-- @since 2.1
addAwaitRetryHook ::
  (AWSRequest a) =>
  Hook_ (Request a, Wait a, Accept, Retry.RetryStatus) ->
  Hook_ (Request a, Wait a, Accept, Retry.RetryStatus) ->
  Hook_ (Request a, Wait a, Accept, Retry.RetryStatus)
addAwaitRetryHook = addHook_

-- | Add an await retry hook for a specific AWS request type. Designed
-- to be used with 'awaitRetryHook'.
--
-- @since 2.1
addAwaitRetryHookFor ::
  (AWSRequest a, AWSRequest b) =>
  Hook_ (Request a, Wait a, Accept, Retry.RetryStatus) ->
  Hook_ (Request b, Wait b, Accept, Retry.RetryStatus) ->
  Hook_ (Request b, Wait b, Accept, Retry.RetryStatus)
addAwaitRetryHookFor = addHookFor_

-- | Remove all await retry hooks for one specific AWS request
-- type. Designed to be used with 'awaitRetryHook'.
--
-- @since 2.1
removeAwaitRetryHooksFor ::
  forall a b.
  (AWSRequest a, AWSRequest b) =>
  Hook_ (Request b, Wait b, Accept, Retry.RetryStatus) ->
  Hook_ (Request b, Wait b, Accept, Retry.RetryStatus)
removeAwaitRetryHooksFor =
  removeHooksFor_ @(Request a, Wait a, Accept, Retry.RetryStatus)

-- | Add a repsonse hook for every AWS request type. Designed to be
-- used with 'responseHook'.
--
-- @since 2.1
addResponseHook ::
  (AWSRequest a) =>
  Hook_ (Request a, ClientResponse (AWSResponse a)) ->
  Hook_ (Request a, ClientResponse (AWSResponse a)) ->
  Hook_ (Request a, ClientResponse (AWSResponse a))
addResponseHook = addHook_

-- | Add a repsonse hook for one specific AWS request type. Designed
-- to be used with 'responseHook'.
--
-- @since 2.1
addResponseHookFor ::
  (AWSRequest a, AWSRequest b) =>
  Hook_ (Request a, ClientResponse (AWSResponse a)) ->
  Hook_ (Request b, ClientResponse (AWSResponse b)) ->
  Hook_ (Request b, ClientResponse (AWSResponse b))
addResponseHookFor = addHookFor_

-- | Remove all response hooks for one specific AWS request
-- type. Designed to be used with 'responseHook'.
--
-- @since 2.1
removeResponseHooksFor ::
  forall a b.
  (AWSRequest a, AWSRequest b) =>
  Hook_ (Request b, ClientResponse (AWSResponse b)) ->
  Hook_ (Request b, ClientResponse (AWSResponse b))
removeResponseHooksFor =
  removeHooksFor_ @(Request a, ClientResponse (AWSResponse a))

-- | Add an error hook for every AWS request type. Designed to be used
-- with 'errorHook'.
--
-- @since 2.1
addErrorHook ::
  (AWSRequest a) =>
  Hook_ (Finality, Request a, Error) ->
  Hook_ (Finality, Request a, Error) ->
  Hook_ (Finality, Request a, Error)
addErrorHook = addHook_

-- | Add an error hook for one specific AWS request type. Designed to
-- be used with 'errorHook'.
--
-- @since 2.1
addErrorHookFor ::
  (AWSRequest a, AWSRequest b) =>
  Hook_ (Finality, Request a, Error) ->
  Hook_ (Finality, Request b, Error) ->
  Hook_ (Finality, Request b, Error)
addErrorHookFor = addHookFor_

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
--
-- @since 2.0
silenceError ::
  Getting Any Error e ->
  Hook_ (Finality, Request a, Error) ->
  Hook_ (Finality, Request a, Error)
silenceError g oldHook env t@(_, _, err) =
  if has g err then pure () else oldHook env t

-- | Remove all error hooks for one specific AWS request
-- type. Designed to be used with 'errorHook'.
--
-- @since 2.1
removeErrorHooksFor ::
  forall a b.
  (AWSRequest a, AWSRequest b) =>
  Hook_ (Finality, Request b, Error) ->
  Hook_ (Finality, Request b, Error)
removeErrorHooksFor = removeHooksFor_ @(Finality, Request a, Error)

-- | Turn a @'Hook' a@ into another @'Hook' a@ that does nothing.
--
-- @
-- -- Example: remove all request hooks:
-- requestHook noHook :: Hooks -> Hooks
-- @
--
-- @since 2.0
noHook :: Hook a -> Hook a
noHook _ _ = pure

-- | Turn a @'Hook_' a@ into another @'Hook_' a@ that does nothing.
--
-- @
-- -- Example: Remove all response hooks:
-- responseHook noHook_ :: Hooks -> Hooks
-- @
--
-- @since 2.0
noHook_ :: Hook_ a -> Hook_ a
noHook_ _ _ _ = pure ()

-- | Add default logging hooks. The default 'Env'' from
-- 'Amazonka.Env.newEnv' already has logging hooks installed, so you
-- probably only want this if you are building your own 'Hooks' from
-- scratch.
--
-- @since 2.0
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
--
-- @since 2.0
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

-- | Unconditionally add a @'Hook' a@ to the chain of hooks. If you
-- need to do something with specific request types, you want
-- 'addHookFor', instead.
--
-- __NOTE__: This function has been deprecated because it's almost
-- impossible to do anything useful with. Instead, you should use one
-- of the hook-specific functions in this module.
--
-- @since 2.0
addHook :: Hook a -> Hook a -> Hook a
addHook newHook oldHook env = oldHook env >=> newHook env
{-# DEPRECATED addHook "this function will be internal in Amazonka 2.2" #-}

-- | Unconditionally add a @'Hook_' a@ to the chain of hooks. If you
-- need to do something with specific request types, you want
-- 'addHookFor_', instead.
--
-- __NOTE__: This function has been deprecated because it's almost
-- impossible to do anything useful with. Instead, you should use one
-- of the hook-specific functions in this module.
--
-- @since 2.0
addHook_ :: Hook_ a -> Hook_ a -> Hook_ a
addHook_ newHook oldHook env a = oldHook env a *> newHook env a
{-# DEPRECATED addHook_ "this function will be internal in Amazonka 2.2" #-}

-- | Like 'addHook', adds an unconditional hook, but it also captures
-- the @'AWSRequest' a@ constraint.
--
-- __NOTE__: This function has been deprecated because it's very easy
-- to accidentally write code which typechecks but never fires the new
-- hook. Instead, you should use one of the hook-specific functions in
-- this module.
--
-- @since 2.0
addAWSRequestHook :: (AWSRequest a) => Hook a -> Hook a -> Hook a
addAWSRequestHook = addHook
{-# DEPRECATED addAWSRequestHook "this function will be removed in Amazonka 2.2" #-}

-- | 'addAWSRequestHook_' is 'addAWSRequestHook' but for 'Hook_'s.
--
-- __NOTE__: This function has been deprecated because it's very easy
-- to accidentally write code which typechecks but never fires the new
-- hook. Instead, you should use one of the hook-specific functions in
-- this module.
--
-- @since 2.0
addAWSRequestHook_ :: (AWSRequest a) => Hook_ a -> Hook_ a -> Hook_ a
addAWSRequestHook_ = addHookFor_
{-# DEPRECATED addAWSRequestHook_ "this function will be removed in Amazonka 2.2" #-}

-- | @addHookFor \@a newHook oldHook@ When @a@ and @b@ are the same
-- type, run the given 'Hook a' after all others, otherwise only run
-- the existing hooks.
--
-- @
-- -- Example: Run @getObjectRequestHook@ on anything that is a @GetObjectRequest@:
-- requestHook (addHookFor @GetObjectRequest getObjectRequestHook) :: Hooks -> Hooks
-- @
--
-- __NOTE__: This function has been deprecated because it's very easy
-- to accidentally write code which typechecks but never fires the new
-- hook. Instead, you should use one of the hook-specific functions in
-- this module.
--
-- @since 2.0
addHookFor ::
  forall a b. (Typeable a, Typeable b) => Hook a -> Hook b -> Hook b
addHookFor newHook oldHook env =
  oldHook env >=> case eqT @a @b of
    Just Refl -> newHook env
    Nothing -> pure
{-# DEPRECATED addHookFor "this function will be internal in Amazonka 2.2" #-}

-- | When @a@ and @b@ are the same type, run the given 'Hook_ a' after
-- all other hooks have run.
--
-- @
-- -- Example: Run @aSignedRequestHook@ on anything that is a @Signed GetObjectRequest@:
-- requestHook (addHookFor_ @(Signed GetObjectRequest) aSignedRequestHook) :: Hooks -> Hooks
-- @
--
-- __NOTE__: This function has been deprecated because it's very easy
-- to accidentally write code which typechecks but never fires the new
-- hook. Instead, you should use one of the hook-specific functions in
-- this module.
--
-- @since 2.0
addHookFor_ ::
  forall a b. (Typeable a, Typeable b) => Hook_ a -> Hook_ b -> Hook_ b
addHookFor_ newHook oldHook env a = do
  oldHook env a
  case eqT @a @b of
    Just Refl -> newHook env a
    Nothing -> pure ()
{-# DEPRECATED addHookFor_ "this function will be internal in Amazonka 2.2" #-}

-- | When @a@ and @b@ are the same type, do not call any more hooks.
--
-- @
-- -- Example: Prevent any request hooks from running against a @PutObjectRequest@:
-- requestHook (removeHooksFor @PutObjectRequest) :: Hooks -> Hooks
-- @
--
-- __NOTE__: This function has been deprecated because it's very easy
-- to accidentally write code which typechecks but never suppresses
-- the hook. Instead, you should use one of the hook-specific
-- functions in this module.
--
-- @since 2.0
removeHooksFor :: forall a b. (Typeable a, Typeable b) => Hook b -> Hook b
removeHooksFor oldHook env = case eqT @a @b of
  Just Refl -> pure
  Nothing -> oldHook env
{-# DEPRECATED removeHooksFor "this function will be internal in Amazonka 2.2" #-}

-- | When @a@ and @b@ are the same type, do not call any more hooks.
--
-- @
-- -- Example: Prevent any error hooks from running against errors caused by a @PutObjectRequest@:
-- errorHook (removeHooksFor @(Finality, Request PutObjectRequest, Error)) :: Hooks -> Hooks
-- @
--
-- __NOTE__: This function has been deprecated because it's very easy
-- to accidentally write code which typechecks but never suppresses
-- the hook. Instead, you should use one of the hook-specific
-- functions in this module.
--
-- @since 2.0
removeHooksFor_ :: forall a b. (Typeable a, Typeable b) => Hook_ b -> Hook_ b
removeHooksFor_ oldHook env a = case eqT @a @b of
  Just Refl -> pure ()
  Nothing -> oldHook env a
{-# DEPRECATED removeHooksFor_ "this function will be internal in Amazonka 2.2" #-}
