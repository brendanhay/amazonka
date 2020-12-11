{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.TestInvokeAuthorizer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Simulate the execution of an 'Authorizer' in your 'RestApi' with headers, parameters, and an incoming request body.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-use-lambda-authorizer.html Use Lambda Function as Authorizer> <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-integrate-with-cognito.html Use Cognito User Pool as Authorizer>
module Network.AWS.APIGateway.TestInvokeAuthorizer
  ( -- * Creating a request
    TestInvokeAuthorizer (..),
    mkTestInvokeAuthorizer,

    -- ** Request lenses
    tiaPathWithQueryString,
    tiaBody,
    tiaAdditionalContext,
    tiaStageVariables,
    tiaHeaders,
    tiaMultiValueHeaders,
    tiaRestAPIId,
    tiaAuthorizerId,

    -- * Destructuring the response
    TestInvokeAuthorizerResponse (..),
    mkTestInvokeAuthorizerResponse,

    -- ** Response lenses
    tiarsLog,
    tiarsPrincipalId,
    tiarsLatency,
    tiarsAuthorization,
    tiarsClaims,
    tiarsClientStatus,
    tiarsPolicy,
    tiarsResponseStatus,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Make a request to simulate the execution of an 'Authorizer' .
--
-- /See:/ 'mkTestInvokeAuthorizer' smart constructor.
data TestInvokeAuthorizer = TestInvokeAuthorizer'
  { pathWithQueryString ::
      Lude.Maybe Lude.Text,
    body :: Lude.Maybe Lude.Text,
    additionalContext ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    stageVariables ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    headers ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    multiValueHeaders ::
      Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])),
    restAPIId :: Lude.Text,
    authorizerId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TestInvokeAuthorizer' with the minimum fields required to make a request.
--
-- * 'additionalContext' - [Optional] A key-value map of additional context variables.
-- * 'authorizerId' - [Required] Specifies a test invoke authorizer request's 'Authorizer' ID.
-- * 'body' - [Optional] The simulated request body of an incoming invocation request.
-- * 'headers' - [Required] A key-value map of headers to simulate an incoming invocation request. This is where the incoming authorization token, or identity source, should be specified.
-- * 'multiValueHeaders' - [Optional] The headers as a map from string to list of values to simulate an incoming invocation request. This is where the incoming authorization token, or identity source, may be specified.
-- * 'pathWithQueryString' - [Optional] The URI path, including query string, of the simulated invocation request. Use this to specify path parameters and query string parameters.
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
-- * 'stageVariables' - A key-value map of stage variables to simulate an invocation on a deployed 'Stage' .
mkTestInvokeAuthorizer ::
  -- | 'restAPIId'
  Lude.Text ->
  -- | 'authorizerId'
  Lude.Text ->
  TestInvokeAuthorizer
mkTestInvokeAuthorizer pRestAPIId_ pAuthorizerId_ =
  TestInvokeAuthorizer'
    { pathWithQueryString = Lude.Nothing,
      body = Lude.Nothing,
      additionalContext = Lude.Nothing,
      stageVariables = Lude.Nothing,
      headers = Lude.Nothing,
      multiValueHeaders = Lude.Nothing,
      restAPIId = pRestAPIId_,
      authorizerId = pAuthorizerId_
    }

-- | [Optional] The URI path, including query string, of the simulated invocation request. Use this to specify path parameters and query string parameters.
--
-- /Note:/ Consider using 'pathWithQueryString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiaPathWithQueryString :: Lens.Lens' TestInvokeAuthorizer (Lude.Maybe Lude.Text)
tiaPathWithQueryString = Lens.lens (pathWithQueryString :: TestInvokeAuthorizer -> Lude.Maybe Lude.Text) (\s a -> s {pathWithQueryString = a} :: TestInvokeAuthorizer)
{-# DEPRECATED tiaPathWithQueryString "Use generic-lens or generic-optics with 'pathWithQueryString' instead." #-}

-- | [Optional] The simulated request body of an incoming invocation request.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiaBody :: Lens.Lens' TestInvokeAuthorizer (Lude.Maybe Lude.Text)
tiaBody = Lens.lens (body :: TestInvokeAuthorizer -> Lude.Maybe Lude.Text) (\s a -> s {body = a} :: TestInvokeAuthorizer)
{-# DEPRECATED tiaBody "Use generic-lens or generic-optics with 'body' instead." #-}

-- | [Optional] A key-value map of additional context variables.
--
-- /Note:/ Consider using 'additionalContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiaAdditionalContext :: Lens.Lens' TestInvokeAuthorizer (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
tiaAdditionalContext = Lens.lens (additionalContext :: TestInvokeAuthorizer -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {additionalContext = a} :: TestInvokeAuthorizer)
{-# DEPRECATED tiaAdditionalContext "Use generic-lens or generic-optics with 'additionalContext' instead." #-}

-- | A key-value map of stage variables to simulate an invocation on a deployed 'Stage' .
--
-- /Note:/ Consider using 'stageVariables' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiaStageVariables :: Lens.Lens' TestInvokeAuthorizer (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
tiaStageVariables = Lens.lens (stageVariables :: TestInvokeAuthorizer -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {stageVariables = a} :: TestInvokeAuthorizer)
{-# DEPRECATED tiaStageVariables "Use generic-lens or generic-optics with 'stageVariables' instead." #-}

-- | [Required] A key-value map of headers to simulate an incoming invocation request. This is where the incoming authorization token, or identity source, should be specified.
--
-- /Note:/ Consider using 'headers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiaHeaders :: Lens.Lens' TestInvokeAuthorizer (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
tiaHeaders = Lens.lens (headers :: TestInvokeAuthorizer -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {headers = a} :: TestInvokeAuthorizer)
{-# DEPRECATED tiaHeaders "Use generic-lens or generic-optics with 'headers' instead." #-}

-- | [Optional] The headers as a map from string to list of values to simulate an incoming invocation request. This is where the incoming authorization token, or identity source, may be specified.
--
-- /Note:/ Consider using 'multiValueHeaders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiaMultiValueHeaders :: Lens.Lens' TestInvokeAuthorizer (Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])))
tiaMultiValueHeaders = Lens.lens (multiValueHeaders :: TestInvokeAuthorizer -> Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text]))) (\s a -> s {multiValueHeaders = a} :: TestInvokeAuthorizer)
{-# DEPRECATED tiaMultiValueHeaders "Use generic-lens or generic-optics with 'multiValueHeaders' instead." #-}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiaRestAPIId :: Lens.Lens' TestInvokeAuthorizer Lude.Text
tiaRestAPIId = Lens.lens (restAPIId :: TestInvokeAuthorizer -> Lude.Text) (\s a -> s {restAPIId = a} :: TestInvokeAuthorizer)
{-# DEPRECATED tiaRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

-- | [Required] Specifies a test invoke authorizer request's 'Authorizer' ID.
--
-- /Note:/ Consider using 'authorizerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiaAuthorizerId :: Lens.Lens' TestInvokeAuthorizer Lude.Text
tiaAuthorizerId = Lens.lens (authorizerId :: TestInvokeAuthorizer -> Lude.Text) (\s a -> s {authorizerId = a} :: TestInvokeAuthorizer)
{-# DEPRECATED tiaAuthorizerId "Use generic-lens or generic-optics with 'authorizerId' instead." #-}

instance Lude.AWSRequest TestInvokeAuthorizer where
  type Rs TestInvokeAuthorizer = TestInvokeAuthorizerResponse
  request = Req.postJSON apiGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          TestInvokeAuthorizerResponse'
            Lude.<$> (x Lude..?> "log")
            Lude.<*> (x Lude..?> "principalId")
            Lude.<*> (x Lude..?> "latency")
            Lude.<*> (x Lude..?> "authorization" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "claims" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "clientStatus")
            Lude.<*> (x Lude..?> "policy")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders TestInvokeAuthorizer where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToJSON TestInvokeAuthorizer where
  toJSON TestInvokeAuthorizer' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("pathWithQueryString" Lude..=) Lude.<$> pathWithQueryString,
            ("body" Lude..=) Lude.<$> body,
            ("additionalContext" Lude..=) Lude.<$> additionalContext,
            ("stageVariables" Lude..=) Lude.<$> stageVariables,
            ("headers" Lude..=) Lude.<$> headers,
            ("multiValueHeaders" Lude..=) Lude.<$> multiValueHeaders
          ]
      )

instance Lude.ToPath TestInvokeAuthorizer where
  toPath TestInvokeAuthorizer' {..} =
    Lude.mconcat
      [ "/restapis/",
        Lude.toBS restAPIId,
        "/authorizers/",
        Lude.toBS authorizerId
      ]

instance Lude.ToQuery TestInvokeAuthorizer where
  toQuery = Lude.const Lude.mempty

-- | Represents the response of the test invoke request for a custom 'Authorizer'
--
-- /See:/ 'mkTestInvokeAuthorizerResponse' smart constructor.
data TestInvokeAuthorizerResponse = TestInvokeAuthorizerResponse'
  { log ::
      Lude.Maybe Lude.Text,
    principalId ::
      Lude.Maybe Lude.Text,
    latency ::
      Lude.Maybe Lude.Integer,
    authorization ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            ([Lude.Text])
        ),
    claims ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            (Lude.Text)
        ),
    clientStatus ::
      Lude.Maybe Lude.Int,
    policy :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'TestInvokeAuthorizerResponse' with the minimum fields required to make a request.
--
-- * 'authorization' - Undocumented field.
-- * 'claims' - The <https://openid.net/specs/openid-connect-core-1_0.html#StandardClaims open identity claims> , with any supported custom attributes, returned from the Cognito Your User Pool configured for the API.
-- * 'clientStatus' - The HTTP status code that the client would have received. Value is 0 if the authorizer succeeded.
-- * 'latency' - The execution latency of the test authorizer request.
-- * 'log' - The API Gateway execution log for the test authorizer request.
-- * 'policy' - The JSON policy document returned by the 'Authorizer'
-- * 'principalId' - The principal identity returned by the 'Authorizer'
-- * 'responseStatus' - The response status code.
mkTestInvokeAuthorizerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  TestInvokeAuthorizerResponse
mkTestInvokeAuthorizerResponse pResponseStatus_ =
  TestInvokeAuthorizerResponse'
    { log = Lude.Nothing,
      principalId = Lude.Nothing,
      latency = Lude.Nothing,
      authorization = Lude.Nothing,
      claims = Lude.Nothing,
      clientStatus = Lude.Nothing,
      policy = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The API Gateway execution log for the test authorizer request.
--
-- /Note:/ Consider using 'log' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiarsLog :: Lens.Lens' TestInvokeAuthorizerResponse (Lude.Maybe Lude.Text)
tiarsLog = Lens.lens (log :: TestInvokeAuthorizerResponse -> Lude.Maybe Lude.Text) (\s a -> s {log = a} :: TestInvokeAuthorizerResponse)
{-# DEPRECATED tiarsLog "Use generic-lens or generic-optics with 'log' instead." #-}

-- | The principal identity returned by the 'Authorizer'
--
-- /Note:/ Consider using 'principalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiarsPrincipalId :: Lens.Lens' TestInvokeAuthorizerResponse (Lude.Maybe Lude.Text)
tiarsPrincipalId = Lens.lens (principalId :: TestInvokeAuthorizerResponse -> Lude.Maybe Lude.Text) (\s a -> s {principalId = a} :: TestInvokeAuthorizerResponse)
{-# DEPRECATED tiarsPrincipalId "Use generic-lens or generic-optics with 'principalId' instead." #-}

-- | The execution latency of the test authorizer request.
--
-- /Note:/ Consider using 'latency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiarsLatency :: Lens.Lens' TestInvokeAuthorizerResponse (Lude.Maybe Lude.Integer)
tiarsLatency = Lens.lens (latency :: TestInvokeAuthorizerResponse -> Lude.Maybe Lude.Integer) (\s a -> s {latency = a} :: TestInvokeAuthorizerResponse)
{-# DEPRECATED tiarsLatency "Use generic-lens or generic-optics with 'latency' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'authorization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiarsAuthorization :: Lens.Lens' TestInvokeAuthorizerResponse (Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])))
tiarsAuthorization = Lens.lens (authorization :: TestInvokeAuthorizerResponse -> Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text]))) (\s a -> s {authorization = a} :: TestInvokeAuthorizerResponse)
{-# DEPRECATED tiarsAuthorization "Use generic-lens or generic-optics with 'authorization' instead." #-}

-- | The <https://openid.net/specs/openid-connect-core-1_0.html#StandardClaims open identity claims> , with any supported custom attributes, returned from the Cognito Your User Pool configured for the API.
--
-- /Note:/ Consider using 'claims' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiarsClaims :: Lens.Lens' TestInvokeAuthorizerResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
tiarsClaims = Lens.lens (claims :: TestInvokeAuthorizerResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {claims = a} :: TestInvokeAuthorizerResponse)
{-# DEPRECATED tiarsClaims "Use generic-lens or generic-optics with 'claims' instead." #-}

-- | The HTTP status code that the client would have received. Value is 0 if the authorizer succeeded.
--
-- /Note:/ Consider using 'clientStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiarsClientStatus :: Lens.Lens' TestInvokeAuthorizerResponse (Lude.Maybe Lude.Int)
tiarsClientStatus = Lens.lens (clientStatus :: TestInvokeAuthorizerResponse -> Lude.Maybe Lude.Int) (\s a -> s {clientStatus = a} :: TestInvokeAuthorizerResponse)
{-# DEPRECATED tiarsClientStatus "Use generic-lens or generic-optics with 'clientStatus' instead." #-}

-- | The JSON policy document returned by the 'Authorizer'
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiarsPolicy :: Lens.Lens' TestInvokeAuthorizerResponse (Lude.Maybe Lude.Text)
tiarsPolicy = Lens.lens (policy :: TestInvokeAuthorizerResponse -> Lude.Maybe Lude.Text) (\s a -> s {policy = a} :: TestInvokeAuthorizerResponse)
{-# DEPRECATED tiarsPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiarsResponseStatus :: Lens.Lens' TestInvokeAuthorizerResponse Lude.Int
tiarsResponseStatus = Lens.lens (responseStatus :: TestInvokeAuthorizerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: TestInvokeAuthorizerResponse)
{-# DEPRECATED tiarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
