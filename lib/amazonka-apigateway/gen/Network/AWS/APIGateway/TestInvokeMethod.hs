{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.TestInvokeMethod
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Simulate the execution of a 'Method' in your 'RestApi' with headers, parameters, and an incoming request body.
module Network.AWS.APIGateway.TestInvokeMethod
  ( -- * Creating a request
    TestInvokeMethod (..),
    mkTestInvokeMethod,

    -- ** Request lenses
    timPathWithQueryString,
    timBody,
    timClientCertificateId,
    timStageVariables,
    timHeaders,
    timMultiValueHeaders,
    timRestAPIId,
    timResourceId,
    timHttpMethod,

    -- * Destructuring the response
    TestInvokeMethodResponse (..),
    mkTestInvokeMethodResponse,

    -- ** Response lenses
    timrsLog,
    timrsStatus,
    timrsBody,
    timrsLatency,
    timrsHeaders,
    timrsMultiValueHeaders,
    timrsResponseStatus,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Make a request to simulate the execution of a 'Method' .
--
-- /See:/ 'mkTestInvokeMethod' smart constructor.
data TestInvokeMethod = TestInvokeMethod'
  { pathWithQueryString ::
      Lude.Maybe Lude.Text,
    body :: Lude.Maybe Lude.Text,
    clientCertificateId :: Lude.Maybe Lude.Text,
    stageVariables ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    headers ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    multiValueHeaders ::
      Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])),
    restAPIId :: Lude.Text,
    resourceId :: Lude.Text,
    httpMethod :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TestInvokeMethod' with the minimum fields required to make a request.
--
-- * 'body' - The simulated request body of an incoming invocation request.
-- * 'clientCertificateId' - A 'ClientCertificate' identifier to use in the test invocation. API Gateway will use the certificate when making the HTTPS request to the defined back-end endpoint.
-- * 'headers' - A key-value map of headers to simulate an incoming invocation request.
-- * 'httpMethod' - [Required] Specifies a test invoke method request's HTTP method.
-- * 'multiValueHeaders' - The headers as a map from string to list of values to simulate an incoming invocation request.
-- * 'pathWithQueryString' - The URI path, including query string, of the simulated invocation request. Use this to specify path parameters and query string parameters.
-- * 'resourceId' - [Required] Specifies a test invoke method request's resource ID.
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
-- * 'stageVariables' - A key-value map of stage variables to simulate an invocation on a deployed 'Stage' .
mkTestInvokeMethod ::
  -- | 'restAPIId'
  Lude.Text ->
  -- | 'resourceId'
  Lude.Text ->
  -- | 'httpMethod'
  Lude.Text ->
  TestInvokeMethod
mkTestInvokeMethod pRestAPIId_ pResourceId_ pHttpMethod_ =
  TestInvokeMethod'
    { pathWithQueryString = Lude.Nothing,
      body = Lude.Nothing,
      clientCertificateId = Lude.Nothing,
      stageVariables = Lude.Nothing,
      headers = Lude.Nothing,
      multiValueHeaders = Lude.Nothing,
      restAPIId = pRestAPIId_,
      resourceId = pResourceId_,
      httpMethod = pHttpMethod_
    }

-- | The URI path, including query string, of the simulated invocation request. Use this to specify path parameters and query string parameters.
--
-- /Note:/ Consider using 'pathWithQueryString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
timPathWithQueryString :: Lens.Lens' TestInvokeMethod (Lude.Maybe Lude.Text)
timPathWithQueryString = Lens.lens (pathWithQueryString :: TestInvokeMethod -> Lude.Maybe Lude.Text) (\s a -> s {pathWithQueryString = a} :: TestInvokeMethod)
{-# DEPRECATED timPathWithQueryString "Use generic-lens or generic-optics with 'pathWithQueryString' instead." #-}

-- | The simulated request body of an incoming invocation request.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
timBody :: Lens.Lens' TestInvokeMethod (Lude.Maybe Lude.Text)
timBody = Lens.lens (body :: TestInvokeMethod -> Lude.Maybe Lude.Text) (\s a -> s {body = a} :: TestInvokeMethod)
{-# DEPRECATED timBody "Use generic-lens or generic-optics with 'body' instead." #-}

-- | A 'ClientCertificate' identifier to use in the test invocation. API Gateway will use the certificate when making the HTTPS request to the defined back-end endpoint.
--
-- /Note:/ Consider using 'clientCertificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
timClientCertificateId :: Lens.Lens' TestInvokeMethod (Lude.Maybe Lude.Text)
timClientCertificateId = Lens.lens (clientCertificateId :: TestInvokeMethod -> Lude.Maybe Lude.Text) (\s a -> s {clientCertificateId = a} :: TestInvokeMethod)
{-# DEPRECATED timClientCertificateId "Use generic-lens or generic-optics with 'clientCertificateId' instead." #-}

-- | A key-value map of stage variables to simulate an invocation on a deployed 'Stage' .
--
-- /Note:/ Consider using 'stageVariables' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
timStageVariables :: Lens.Lens' TestInvokeMethod (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
timStageVariables = Lens.lens (stageVariables :: TestInvokeMethod -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {stageVariables = a} :: TestInvokeMethod)
{-# DEPRECATED timStageVariables "Use generic-lens or generic-optics with 'stageVariables' instead." #-}

-- | A key-value map of headers to simulate an incoming invocation request.
--
-- /Note:/ Consider using 'headers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
timHeaders :: Lens.Lens' TestInvokeMethod (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
timHeaders = Lens.lens (headers :: TestInvokeMethod -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {headers = a} :: TestInvokeMethod)
{-# DEPRECATED timHeaders "Use generic-lens or generic-optics with 'headers' instead." #-}

-- | The headers as a map from string to list of values to simulate an incoming invocation request.
--
-- /Note:/ Consider using 'multiValueHeaders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
timMultiValueHeaders :: Lens.Lens' TestInvokeMethod (Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])))
timMultiValueHeaders = Lens.lens (multiValueHeaders :: TestInvokeMethod -> Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text]))) (\s a -> s {multiValueHeaders = a} :: TestInvokeMethod)
{-# DEPRECATED timMultiValueHeaders "Use generic-lens or generic-optics with 'multiValueHeaders' instead." #-}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
timRestAPIId :: Lens.Lens' TestInvokeMethod Lude.Text
timRestAPIId = Lens.lens (restAPIId :: TestInvokeMethod -> Lude.Text) (\s a -> s {restAPIId = a} :: TestInvokeMethod)
{-# DEPRECATED timRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

-- | [Required] Specifies a test invoke method request's resource ID.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
timResourceId :: Lens.Lens' TestInvokeMethod Lude.Text
timResourceId = Lens.lens (resourceId :: TestInvokeMethod -> Lude.Text) (\s a -> s {resourceId = a} :: TestInvokeMethod)
{-# DEPRECATED timResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | [Required] Specifies a test invoke method request's HTTP method.
--
-- /Note:/ Consider using 'httpMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
timHttpMethod :: Lens.Lens' TestInvokeMethod Lude.Text
timHttpMethod = Lens.lens (httpMethod :: TestInvokeMethod -> Lude.Text) (\s a -> s {httpMethod = a} :: TestInvokeMethod)
{-# DEPRECATED timHttpMethod "Use generic-lens or generic-optics with 'httpMethod' instead." #-}

instance Lude.AWSRequest TestInvokeMethod where
  type Rs TestInvokeMethod = TestInvokeMethodResponse
  request = Req.postJSON apiGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          TestInvokeMethodResponse'
            Lude.<$> (x Lude..?> "log")
            Lude.<*> (x Lude..?> "status")
            Lude.<*> (x Lude..?> "body")
            Lude.<*> (x Lude..?> "latency")
            Lude.<*> (x Lude..?> "headers" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "multiValueHeaders" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders TestInvokeMethod where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToJSON TestInvokeMethod where
  toJSON TestInvokeMethod' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("pathWithQueryString" Lude..=) Lude.<$> pathWithQueryString,
            ("body" Lude..=) Lude.<$> body,
            ("clientCertificateId" Lude..=) Lude.<$> clientCertificateId,
            ("stageVariables" Lude..=) Lude.<$> stageVariables,
            ("headers" Lude..=) Lude.<$> headers,
            ("multiValueHeaders" Lude..=) Lude.<$> multiValueHeaders
          ]
      )

instance Lude.ToPath TestInvokeMethod where
  toPath TestInvokeMethod' {..} =
    Lude.mconcat
      [ "/restapis/",
        Lude.toBS restAPIId,
        "/resources/",
        Lude.toBS resourceId,
        "/methods/",
        Lude.toBS httpMethod
      ]

instance Lude.ToQuery TestInvokeMethod where
  toQuery = Lude.const Lude.mempty

-- | Represents the response of the test invoke request in the HTTP method.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-test-method.html#how-to-test-method-console Test API using the API Gateway console>
--
-- /See:/ 'mkTestInvokeMethodResponse' smart constructor.
data TestInvokeMethodResponse = TestInvokeMethodResponse'
  { log ::
      Lude.Maybe Lude.Text,
    status :: Lude.Maybe Lude.Int,
    body :: Lude.Maybe Lude.Text,
    latency :: Lude.Maybe Lude.Integer,
    headers ::
      Lude.Maybe
        (Lude.HashMap Lude.Text (Lude.Text)),
    multiValueHeaders ::
      Lude.Maybe
        (Lude.HashMap Lude.Text ([Lude.Text])),
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TestInvokeMethodResponse' with the minimum fields required to make a request.
--
-- * 'body' - The body of the HTTP response.
-- * 'headers' - The headers of the HTTP response.
-- * 'latency' - The execution latency of the test invoke request.
-- * 'log' - The API Gateway execution log for the test invoke request.
-- * 'multiValueHeaders' - The headers of the HTTP response as a map from string to list of values.
-- * 'responseStatus' - The response status code.
-- * 'status' - The HTTP status code.
mkTestInvokeMethodResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  TestInvokeMethodResponse
mkTestInvokeMethodResponse pResponseStatus_ =
  TestInvokeMethodResponse'
    { log = Lude.Nothing,
      status = Lude.Nothing,
      body = Lude.Nothing,
      latency = Lude.Nothing,
      headers = Lude.Nothing,
      multiValueHeaders = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The API Gateway execution log for the test invoke request.
--
-- /Note:/ Consider using 'log' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
timrsLog :: Lens.Lens' TestInvokeMethodResponse (Lude.Maybe Lude.Text)
timrsLog = Lens.lens (log :: TestInvokeMethodResponse -> Lude.Maybe Lude.Text) (\s a -> s {log = a} :: TestInvokeMethodResponse)
{-# DEPRECATED timrsLog "Use generic-lens or generic-optics with 'log' instead." #-}

-- | The HTTP status code.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
timrsStatus :: Lens.Lens' TestInvokeMethodResponse (Lude.Maybe Lude.Int)
timrsStatus = Lens.lens (status :: TestInvokeMethodResponse -> Lude.Maybe Lude.Int) (\s a -> s {status = a} :: TestInvokeMethodResponse)
{-# DEPRECATED timrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The body of the HTTP response.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
timrsBody :: Lens.Lens' TestInvokeMethodResponse (Lude.Maybe Lude.Text)
timrsBody = Lens.lens (body :: TestInvokeMethodResponse -> Lude.Maybe Lude.Text) (\s a -> s {body = a} :: TestInvokeMethodResponse)
{-# DEPRECATED timrsBody "Use generic-lens or generic-optics with 'body' instead." #-}

-- | The execution latency of the test invoke request.
--
-- /Note:/ Consider using 'latency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
timrsLatency :: Lens.Lens' TestInvokeMethodResponse (Lude.Maybe Lude.Integer)
timrsLatency = Lens.lens (latency :: TestInvokeMethodResponse -> Lude.Maybe Lude.Integer) (\s a -> s {latency = a} :: TestInvokeMethodResponse)
{-# DEPRECATED timrsLatency "Use generic-lens or generic-optics with 'latency' instead." #-}

-- | The headers of the HTTP response.
--
-- /Note:/ Consider using 'headers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
timrsHeaders :: Lens.Lens' TestInvokeMethodResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
timrsHeaders = Lens.lens (headers :: TestInvokeMethodResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {headers = a} :: TestInvokeMethodResponse)
{-# DEPRECATED timrsHeaders "Use generic-lens or generic-optics with 'headers' instead." #-}

-- | The headers of the HTTP response as a map from string to list of values.
--
-- /Note:/ Consider using 'multiValueHeaders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
timrsMultiValueHeaders :: Lens.Lens' TestInvokeMethodResponse (Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])))
timrsMultiValueHeaders = Lens.lens (multiValueHeaders :: TestInvokeMethodResponse -> Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text]))) (\s a -> s {multiValueHeaders = a} :: TestInvokeMethodResponse)
{-# DEPRECATED timrsMultiValueHeaders "Use generic-lens or generic-optics with 'multiValueHeaders' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
timrsResponseStatus :: Lens.Lens' TestInvokeMethodResponse Lude.Int
timrsResponseStatus = Lens.lens (responseStatus :: TestInvokeMethodResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: TestInvokeMethodResponse)
{-# DEPRECATED timrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
