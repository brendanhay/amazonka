{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.TestInvokeMethod
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Simulate the execution of a 'Method' in your 'RestApi' with headers, parameters, and an incoming request body.
module Network.AWS.ApiGateway.TestInvokeMethod
    (
    -- * Creating a request
      TestInvokeMethod (..)
    , mkTestInvokeMethod
    -- ** Request lenses
    , timRestApiId
    , timResourceId
    , timHttpMethod
    , timBody
    , timClientCertificateId
    , timHeaders
    , timMultiValueHeaders
    , timPathWithQueryString
    , timStageVariables

    -- * Destructuring the response
    , TestInvokeMethodResponse (..)
    , mkTestInvokeMethodResponse
    -- ** Response lenses
    , timrrsBody
    , timrrsHeaders
    , timrrsLatency
    , timrrsLog
    , timrrsMultiValueHeaders
    , timrrsStatus
    , timrrsResponseStatus
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Make a request to simulate the execution of a 'Method' .
--
-- /See:/ 'mkTestInvokeMethod' smart constructor.
data TestInvokeMethod = TestInvokeMethod'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , resourceId :: Core.Text
    -- ^ [Required] Specifies a test invoke method request's resource ID.
  , httpMethod :: Core.Text
    -- ^ [Required] Specifies a test invoke method request's HTTP method.
  , body :: Core.Maybe Core.Text
    -- ^ The simulated request body of an incoming invocation request.
  , clientCertificateId :: Core.Maybe Core.Text
    -- ^ A 'ClientCertificate' identifier to use in the test invocation. API Gateway will use the certificate when making the HTTPS request to the defined back-end endpoint.
  , headers :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ A key-value map of headers to simulate an incoming invocation request.
  , multiValueHeaders :: Core.Maybe (Core.HashMap Core.Text [Core.Text])
    -- ^ The headers as a map from string to list of values to simulate an incoming invocation request.
  , pathWithQueryString :: Core.Maybe Core.Text
    -- ^ The URI path, including query string, of the simulated invocation request. Use this to specify path parameters and query string parameters.
  , stageVariables :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ A key-value map of stage variables to simulate an invocation on a deployed 'Stage' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TestInvokeMethod' value with any optional fields omitted.
mkTestInvokeMethod
    :: Core.Text -- ^ 'restApiId'
    -> Core.Text -- ^ 'resourceId'
    -> Core.Text -- ^ 'httpMethod'
    -> TestInvokeMethod
mkTestInvokeMethod restApiId resourceId httpMethod
  = TestInvokeMethod'{restApiId, resourceId, httpMethod,
                      body = Core.Nothing, clientCertificateId = Core.Nothing,
                      headers = Core.Nothing, multiValueHeaders = Core.Nothing,
                      pathWithQueryString = Core.Nothing, stageVariables = Core.Nothing}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
timRestApiId :: Lens.Lens' TestInvokeMethod Core.Text
timRestApiId = Lens.field @"restApiId"
{-# INLINEABLE timRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | [Required] Specifies a test invoke method request's resource ID.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
timResourceId :: Lens.Lens' TestInvokeMethod Core.Text
timResourceId = Lens.field @"resourceId"
{-# INLINEABLE timResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | [Required] Specifies a test invoke method request's HTTP method.
--
-- /Note:/ Consider using 'httpMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
timHttpMethod :: Lens.Lens' TestInvokeMethod Core.Text
timHttpMethod = Lens.field @"httpMethod"
{-# INLINEABLE timHttpMethod #-}
{-# DEPRECATED httpMethod "Use generic-lens or generic-optics with 'httpMethod' instead"  #-}

-- | The simulated request body of an incoming invocation request.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
timBody :: Lens.Lens' TestInvokeMethod (Core.Maybe Core.Text)
timBody = Lens.field @"body"
{-# INLINEABLE timBody #-}
{-# DEPRECATED body "Use generic-lens or generic-optics with 'body' instead"  #-}

-- | A 'ClientCertificate' identifier to use in the test invocation. API Gateway will use the certificate when making the HTTPS request to the defined back-end endpoint.
--
-- /Note:/ Consider using 'clientCertificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
timClientCertificateId :: Lens.Lens' TestInvokeMethod (Core.Maybe Core.Text)
timClientCertificateId = Lens.field @"clientCertificateId"
{-# INLINEABLE timClientCertificateId #-}
{-# DEPRECATED clientCertificateId "Use generic-lens or generic-optics with 'clientCertificateId' instead"  #-}

-- | A key-value map of headers to simulate an incoming invocation request.
--
-- /Note:/ Consider using 'headers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
timHeaders :: Lens.Lens' TestInvokeMethod (Core.Maybe (Core.HashMap Core.Text Core.Text))
timHeaders = Lens.field @"headers"
{-# INLINEABLE timHeaders #-}
{-# DEPRECATED headers "Use generic-lens or generic-optics with 'headers' instead"  #-}

-- | The headers as a map from string to list of values to simulate an incoming invocation request.
--
-- /Note:/ Consider using 'multiValueHeaders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
timMultiValueHeaders :: Lens.Lens' TestInvokeMethod (Core.Maybe (Core.HashMap Core.Text [Core.Text]))
timMultiValueHeaders = Lens.field @"multiValueHeaders"
{-# INLINEABLE timMultiValueHeaders #-}
{-# DEPRECATED multiValueHeaders "Use generic-lens or generic-optics with 'multiValueHeaders' instead"  #-}

-- | The URI path, including query string, of the simulated invocation request. Use this to specify path parameters and query string parameters.
--
-- /Note:/ Consider using 'pathWithQueryString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
timPathWithQueryString :: Lens.Lens' TestInvokeMethod (Core.Maybe Core.Text)
timPathWithQueryString = Lens.field @"pathWithQueryString"
{-# INLINEABLE timPathWithQueryString #-}
{-# DEPRECATED pathWithQueryString "Use generic-lens or generic-optics with 'pathWithQueryString' instead"  #-}

-- | A key-value map of stage variables to simulate an invocation on a deployed 'Stage' .
--
-- /Note:/ Consider using 'stageVariables' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
timStageVariables :: Lens.Lens' TestInvokeMethod (Core.Maybe (Core.HashMap Core.Text Core.Text))
timStageVariables = Lens.field @"stageVariables"
{-# INLINEABLE timStageVariables #-}
{-# DEPRECATED stageVariables "Use generic-lens or generic-optics with 'stageVariables' instead"  #-}

instance Core.ToQuery TestInvokeMethod where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders TestInvokeMethod where
        toHeaders TestInvokeMethod{..}
          = Core.pure ("Accept", "application/json")

instance Core.FromJSON TestInvokeMethod where
        toJSON TestInvokeMethod{..}
          = Core.object
              (Core.catMaybes
                 [("body" Core..=) Core.<$> body,
                  ("clientCertificateId" Core..=) Core.<$> clientCertificateId,
                  ("headers" Core..=) Core.<$> headers,
                  ("multiValueHeaders" Core..=) Core.<$> multiValueHeaders,
                  ("pathWithQueryString" Core..=) Core.<$> pathWithQueryString,
                  ("stageVariables" Core..=) Core.<$> stageVariables])

instance Core.AWSRequest TestInvokeMethod where
        type Rs TestInvokeMethod = TestInvokeMethodResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<> "/resources/"
                             Core.<> Core.toText resourceId
                             Core.<> "/methods/"
                             Core.<> Core.toText httpMethod,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 TestInvokeMethodResponse' Core.<$>
                   (x Core..:? "body") Core.<*> x Core..:? "headers" Core.<*>
                     x Core..:? "latency"
                     Core.<*> x Core..:? "log"
                     Core.<*> x Core..:? "multiValueHeaders"
                     Core.<*> x Core..:? "status"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the response of the test invoke request in the HTTP method.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-test-method.html#how-to-test-method-console Test API using the API Gateway console> 
--
-- /See:/ 'mkTestInvokeMethodResponse' smart constructor.
data TestInvokeMethodResponse = TestInvokeMethodResponse'
  { body :: Core.Maybe Core.Text
    -- ^ The body of the HTTP response.
  , headers :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ The headers of the HTTP response.
  , latency :: Core.Maybe Core.Integer
    -- ^ The execution latency of the test invoke request.
  , log :: Core.Maybe Core.Text
    -- ^ The API Gateway execution log for the test invoke request.
  , multiValueHeaders :: Core.Maybe (Core.HashMap Core.Text [Core.Text])
    -- ^ The headers of the HTTP response as a map from string to list of values.
  , status :: Core.Maybe Core.Int
    -- ^ The HTTP status code.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TestInvokeMethodResponse' value with any optional fields omitted.
mkTestInvokeMethodResponse
    :: Core.Int -- ^ 'responseStatus'
    -> TestInvokeMethodResponse
mkTestInvokeMethodResponse responseStatus
  = TestInvokeMethodResponse'{body = Core.Nothing,
                              headers = Core.Nothing, latency = Core.Nothing, log = Core.Nothing,
                              multiValueHeaders = Core.Nothing, status = Core.Nothing,
                              responseStatus}

-- | The body of the HTTP response.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
timrrsBody :: Lens.Lens' TestInvokeMethodResponse (Core.Maybe Core.Text)
timrrsBody = Lens.field @"body"
{-# INLINEABLE timrrsBody #-}
{-# DEPRECATED body "Use generic-lens or generic-optics with 'body' instead"  #-}

-- | The headers of the HTTP response.
--
-- /Note:/ Consider using 'headers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
timrrsHeaders :: Lens.Lens' TestInvokeMethodResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
timrrsHeaders = Lens.field @"headers"
{-# INLINEABLE timrrsHeaders #-}
{-# DEPRECATED headers "Use generic-lens or generic-optics with 'headers' instead"  #-}

-- | The execution latency of the test invoke request.
--
-- /Note:/ Consider using 'latency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
timrrsLatency :: Lens.Lens' TestInvokeMethodResponse (Core.Maybe Core.Integer)
timrrsLatency = Lens.field @"latency"
{-# INLINEABLE timrrsLatency #-}
{-# DEPRECATED latency "Use generic-lens or generic-optics with 'latency' instead"  #-}

-- | The API Gateway execution log for the test invoke request.
--
-- /Note:/ Consider using 'log' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
timrrsLog :: Lens.Lens' TestInvokeMethodResponse (Core.Maybe Core.Text)
timrrsLog = Lens.field @"log"
{-# INLINEABLE timrrsLog #-}
{-# DEPRECATED log "Use generic-lens or generic-optics with 'log' instead"  #-}

-- | The headers of the HTTP response as a map from string to list of values.
--
-- /Note:/ Consider using 'multiValueHeaders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
timrrsMultiValueHeaders :: Lens.Lens' TestInvokeMethodResponse (Core.Maybe (Core.HashMap Core.Text [Core.Text]))
timrrsMultiValueHeaders = Lens.field @"multiValueHeaders"
{-# INLINEABLE timrrsMultiValueHeaders #-}
{-# DEPRECATED multiValueHeaders "Use generic-lens or generic-optics with 'multiValueHeaders' instead"  #-}

-- | The HTTP status code.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
timrrsStatus :: Lens.Lens' TestInvokeMethodResponse (Core.Maybe Core.Int)
timrrsStatus = Lens.field @"status"
{-# INLINEABLE timrrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
timrrsResponseStatus :: Lens.Lens' TestInvokeMethodResponse Core.Int
timrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE timrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
