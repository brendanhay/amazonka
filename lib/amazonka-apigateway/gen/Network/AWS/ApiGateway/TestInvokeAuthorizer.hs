{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.TestInvokeAuthorizer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Simulate the execution of an 'Authorizer' in your 'RestApi' with headers, parameters, and an incoming request body.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-use-lambda-authorizer.html Use Lambda Function as Authorizer> <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-integrate-with-cognito.html Use Cognito User Pool as Authorizer> 
module Network.AWS.ApiGateway.TestInvokeAuthorizer
    (
    -- * Creating a request
      TestInvokeAuthorizer (..)
    , mkTestInvokeAuthorizer
    -- ** Request lenses
    , tiaRestApiId
    , tiaAuthorizerId
    , tiaAdditionalContext
    , tiaBody
    , tiaHeaders
    , tiaMultiValueHeaders
    , tiaPathWithQueryString
    , tiaStageVariables

    -- * Destructuring the response
    , TestInvokeAuthorizerResponse (..)
    , mkTestInvokeAuthorizerResponse
    -- ** Response lenses
    , tiarrsAuthorization
    , tiarrsClaims
    , tiarrsClientStatus
    , tiarrsLatency
    , tiarrsLog
    , tiarrsPolicy
    , tiarrsPrincipalId
    , tiarrsResponseStatus
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Make a request to simulate the execution of an 'Authorizer' .
--
-- /See:/ 'mkTestInvokeAuthorizer' smart constructor.
data TestInvokeAuthorizer = TestInvokeAuthorizer'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , authorizerId :: Core.Text
    -- ^ [Required] Specifies a test invoke authorizer request's 'Authorizer' ID.
  , additionalContext :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ [Optional] A key-value map of additional context variables.
  , body :: Core.Maybe Core.Text
    -- ^ [Optional] The simulated request body of an incoming invocation request.
  , headers :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ [Required] A key-value map of headers to simulate an incoming invocation request. This is where the incoming authorization token, or identity source, should be specified.
  , multiValueHeaders :: Core.Maybe (Core.HashMap Core.Text [Core.Text])
    -- ^ [Optional] The headers as a map from string to list of values to simulate an incoming invocation request. This is where the incoming authorization token, or identity source, may be specified.
  , pathWithQueryString :: Core.Maybe Core.Text
    -- ^ [Optional] The URI path, including query string, of the simulated invocation request. Use this to specify path parameters and query string parameters.
  , stageVariables :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ A key-value map of stage variables to simulate an invocation on a deployed 'Stage' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TestInvokeAuthorizer' value with any optional fields omitted.
mkTestInvokeAuthorizer
    :: Core.Text -- ^ 'restApiId'
    -> Core.Text -- ^ 'authorizerId'
    -> TestInvokeAuthorizer
mkTestInvokeAuthorizer restApiId authorizerId
  = TestInvokeAuthorizer'{restApiId, authorizerId,
                          additionalContext = Core.Nothing, body = Core.Nothing,
                          headers = Core.Nothing, multiValueHeaders = Core.Nothing,
                          pathWithQueryString = Core.Nothing, stageVariables = Core.Nothing}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiaRestApiId :: Lens.Lens' TestInvokeAuthorizer Core.Text
tiaRestApiId = Lens.field @"restApiId"
{-# INLINEABLE tiaRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | [Required] Specifies a test invoke authorizer request's 'Authorizer' ID.
--
-- /Note:/ Consider using 'authorizerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiaAuthorizerId :: Lens.Lens' TestInvokeAuthorizer Core.Text
tiaAuthorizerId = Lens.field @"authorizerId"
{-# INLINEABLE tiaAuthorizerId #-}
{-# DEPRECATED authorizerId "Use generic-lens or generic-optics with 'authorizerId' instead"  #-}

-- | [Optional] A key-value map of additional context variables.
--
-- /Note:/ Consider using 'additionalContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiaAdditionalContext :: Lens.Lens' TestInvokeAuthorizer (Core.Maybe (Core.HashMap Core.Text Core.Text))
tiaAdditionalContext = Lens.field @"additionalContext"
{-# INLINEABLE tiaAdditionalContext #-}
{-# DEPRECATED additionalContext "Use generic-lens or generic-optics with 'additionalContext' instead"  #-}

-- | [Optional] The simulated request body of an incoming invocation request.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiaBody :: Lens.Lens' TestInvokeAuthorizer (Core.Maybe Core.Text)
tiaBody = Lens.field @"body"
{-# INLINEABLE tiaBody #-}
{-# DEPRECATED body "Use generic-lens or generic-optics with 'body' instead"  #-}

-- | [Required] A key-value map of headers to simulate an incoming invocation request. This is where the incoming authorization token, or identity source, should be specified.
--
-- /Note:/ Consider using 'headers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiaHeaders :: Lens.Lens' TestInvokeAuthorizer (Core.Maybe (Core.HashMap Core.Text Core.Text))
tiaHeaders = Lens.field @"headers"
{-# INLINEABLE tiaHeaders #-}
{-# DEPRECATED headers "Use generic-lens or generic-optics with 'headers' instead"  #-}

-- | [Optional] The headers as a map from string to list of values to simulate an incoming invocation request. This is where the incoming authorization token, or identity source, may be specified.
--
-- /Note:/ Consider using 'multiValueHeaders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiaMultiValueHeaders :: Lens.Lens' TestInvokeAuthorizer (Core.Maybe (Core.HashMap Core.Text [Core.Text]))
tiaMultiValueHeaders = Lens.field @"multiValueHeaders"
{-# INLINEABLE tiaMultiValueHeaders #-}
{-# DEPRECATED multiValueHeaders "Use generic-lens or generic-optics with 'multiValueHeaders' instead"  #-}

-- | [Optional] The URI path, including query string, of the simulated invocation request. Use this to specify path parameters and query string parameters.
--
-- /Note:/ Consider using 'pathWithQueryString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiaPathWithQueryString :: Lens.Lens' TestInvokeAuthorizer (Core.Maybe Core.Text)
tiaPathWithQueryString = Lens.field @"pathWithQueryString"
{-# INLINEABLE tiaPathWithQueryString #-}
{-# DEPRECATED pathWithQueryString "Use generic-lens or generic-optics with 'pathWithQueryString' instead"  #-}

-- | A key-value map of stage variables to simulate an invocation on a deployed 'Stage' .
--
-- /Note:/ Consider using 'stageVariables' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiaStageVariables :: Lens.Lens' TestInvokeAuthorizer (Core.Maybe (Core.HashMap Core.Text Core.Text))
tiaStageVariables = Lens.field @"stageVariables"
{-# INLINEABLE tiaStageVariables #-}
{-# DEPRECATED stageVariables "Use generic-lens or generic-optics with 'stageVariables' instead"  #-}

instance Core.ToQuery TestInvokeAuthorizer where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders TestInvokeAuthorizer where
        toHeaders TestInvokeAuthorizer{..}
          = Core.pure ("Accept", "application/json")

instance Core.FromJSON TestInvokeAuthorizer where
        toJSON TestInvokeAuthorizer{..}
          = Core.object
              (Core.catMaybes
                 [("additionalContext" Core..=) Core.<$> additionalContext,
                  ("body" Core..=) Core.<$> body,
                  ("headers" Core..=) Core.<$> headers,
                  ("multiValueHeaders" Core..=) Core.<$> multiValueHeaders,
                  ("pathWithQueryString" Core..=) Core.<$> pathWithQueryString,
                  ("stageVariables" Core..=) Core.<$> stageVariables])

instance Core.AWSRequest TestInvokeAuthorizer where
        type Rs TestInvokeAuthorizer = TestInvokeAuthorizerResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<> "/authorizers/"
                             Core.<> Core.toText authorizerId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 TestInvokeAuthorizerResponse' Core.<$>
                   (x Core..:? "authorization") Core.<*> x Core..:? "claims" Core.<*>
                     x Core..:? "clientStatus"
                     Core.<*> x Core..:? "latency"
                     Core.<*> x Core..:? "log"
                     Core.<*> x Core..:? "policy"
                     Core.<*> x Core..:? "principalId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the response of the test invoke request for a custom 'Authorizer' 
--
-- /See:/ 'mkTestInvokeAuthorizerResponse' smart constructor.
data TestInvokeAuthorizerResponse = TestInvokeAuthorizerResponse'
  { authorization :: Core.Maybe (Core.HashMap Core.Text [Core.Text])
  , claims :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ The <https://openid.net/specs/openid-connect-core-1_0.html#StandardClaims open identity claims> , with any supported custom attributes, returned from the Cognito Your User Pool configured for the API.
  , clientStatus :: Core.Maybe Core.Int
    -- ^ The HTTP status code that the client would have received. Value is 0 if the authorizer succeeded.
  , latency :: Core.Maybe Core.Integer
    -- ^ The execution latency of the test authorizer request.
  , log :: Core.Maybe Core.Text
    -- ^ The API Gateway execution log for the test authorizer request.
  , policy :: Core.Maybe Core.Text
    -- ^ The JSON policy document returned by the 'Authorizer' 
  , principalId :: Core.Maybe Core.Text
    -- ^ The principal identity returned by the 'Authorizer' 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TestInvokeAuthorizerResponse' value with any optional fields omitted.
mkTestInvokeAuthorizerResponse
    :: Core.Int -- ^ 'responseStatus'
    -> TestInvokeAuthorizerResponse
mkTestInvokeAuthorizerResponse responseStatus
  = TestInvokeAuthorizerResponse'{authorization = Core.Nothing,
                                  claims = Core.Nothing, clientStatus = Core.Nothing,
                                  latency = Core.Nothing, log = Core.Nothing, policy = Core.Nothing,
                                  principalId = Core.Nothing, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'authorization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiarrsAuthorization :: Lens.Lens' TestInvokeAuthorizerResponse (Core.Maybe (Core.HashMap Core.Text [Core.Text]))
tiarrsAuthorization = Lens.field @"authorization"
{-# INLINEABLE tiarrsAuthorization #-}
{-# DEPRECATED authorization "Use generic-lens or generic-optics with 'authorization' instead"  #-}

-- | The <https://openid.net/specs/openid-connect-core-1_0.html#StandardClaims open identity claims> , with any supported custom attributes, returned from the Cognito Your User Pool configured for the API.
--
-- /Note:/ Consider using 'claims' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiarrsClaims :: Lens.Lens' TestInvokeAuthorizerResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
tiarrsClaims = Lens.field @"claims"
{-# INLINEABLE tiarrsClaims #-}
{-# DEPRECATED claims "Use generic-lens or generic-optics with 'claims' instead"  #-}

-- | The HTTP status code that the client would have received. Value is 0 if the authorizer succeeded.
--
-- /Note:/ Consider using 'clientStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiarrsClientStatus :: Lens.Lens' TestInvokeAuthorizerResponse (Core.Maybe Core.Int)
tiarrsClientStatus = Lens.field @"clientStatus"
{-# INLINEABLE tiarrsClientStatus #-}
{-# DEPRECATED clientStatus "Use generic-lens or generic-optics with 'clientStatus' instead"  #-}

-- | The execution latency of the test authorizer request.
--
-- /Note:/ Consider using 'latency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiarrsLatency :: Lens.Lens' TestInvokeAuthorizerResponse (Core.Maybe Core.Integer)
tiarrsLatency = Lens.field @"latency"
{-# INLINEABLE tiarrsLatency #-}
{-# DEPRECATED latency "Use generic-lens or generic-optics with 'latency' instead"  #-}

-- | The API Gateway execution log for the test authorizer request.
--
-- /Note:/ Consider using 'log' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiarrsLog :: Lens.Lens' TestInvokeAuthorizerResponse (Core.Maybe Core.Text)
tiarrsLog = Lens.field @"log"
{-# INLINEABLE tiarrsLog #-}
{-# DEPRECATED log "Use generic-lens or generic-optics with 'log' instead"  #-}

-- | The JSON policy document returned by the 'Authorizer' 
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiarrsPolicy :: Lens.Lens' TestInvokeAuthorizerResponse (Core.Maybe Core.Text)
tiarrsPolicy = Lens.field @"policy"
{-# INLINEABLE tiarrsPolicy #-}
{-# DEPRECATED policy "Use generic-lens or generic-optics with 'policy' instead"  #-}

-- | The principal identity returned by the 'Authorizer' 
--
-- /Note:/ Consider using 'principalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiarrsPrincipalId :: Lens.Lens' TestInvokeAuthorizerResponse (Core.Maybe Core.Text)
tiarrsPrincipalId = Lens.field @"principalId"
{-# INLINEABLE tiarrsPrincipalId #-}
{-# DEPRECATED principalId "Use generic-lens or generic-optics with 'principalId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiarrsResponseStatus :: Lens.Lens' TestInvokeAuthorizerResponse Core.Int
tiarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE tiarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
