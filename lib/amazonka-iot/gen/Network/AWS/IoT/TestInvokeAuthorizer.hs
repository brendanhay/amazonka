{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.TestInvokeAuthorizer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Tests a custom authorization behavior by invoking a specified custom authorizer. Use this to test and debug the custom authorization behavior of devices that connect to the AWS IoT device gateway.
module Network.AWS.IoT.TestInvokeAuthorizer
  ( -- * Creating a request
    TestInvokeAuthorizer (..),
    mkTestInvokeAuthorizer,

    -- ** Request lenses
    tiaToken,
    tiaTlsContext,
    tiaTokenSignature,
    tiaHttpContext,
    tiaMqttContext,
    tiaAuthorizerName,

    -- * Destructuring the response
    TestInvokeAuthorizerResponse (..),
    mkTestInvokeAuthorizerResponse,

    -- ** Response lenses
    tiarsPolicyDocuments,
    tiarsPrincipalId,
    tiarsDisconnectAfterInSeconds,
    tiarsIsAuthenticated,
    tiarsRefreshAfterInSeconds,
    tiarsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkTestInvokeAuthorizer' smart constructor.
data TestInvokeAuthorizer = TestInvokeAuthorizer'
  { token ::
      Lude.Maybe Lude.Text,
    tlsContext :: Lude.Maybe TLSContext,
    tokenSignature :: Lude.Maybe Lude.Text,
    httpContext :: Lude.Maybe HTTPContext,
    mqttContext :: Lude.Maybe MqttContext,
    authorizerName :: Lude.Text
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
-- * 'authorizerName' - The custom authorizer name.
-- * 'httpContext' - Specifies a test HTTP authorization request.
-- * 'mqttContext' - Specifies a test MQTT authorization request.
-- * 'tlsContext' - Specifies a test TLS authorization request.
-- * 'token' - The token returned by your custom authentication service.
-- * 'tokenSignature' - The signature made with the token and your custom authentication service's private key. This value must be Base-64-encoded.
mkTestInvokeAuthorizer ::
  -- | 'authorizerName'
  Lude.Text ->
  TestInvokeAuthorizer
mkTestInvokeAuthorizer pAuthorizerName_ =
  TestInvokeAuthorizer'
    { token = Lude.Nothing,
      tlsContext = Lude.Nothing,
      tokenSignature = Lude.Nothing,
      httpContext = Lude.Nothing,
      mqttContext = Lude.Nothing,
      authorizerName = pAuthorizerName_
    }

-- | The token returned by your custom authentication service.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiaToken :: Lens.Lens' TestInvokeAuthorizer (Lude.Maybe Lude.Text)
tiaToken = Lens.lens (token :: TestInvokeAuthorizer -> Lude.Maybe Lude.Text) (\s a -> s {token = a} :: TestInvokeAuthorizer)
{-# DEPRECATED tiaToken "Use generic-lens or generic-optics with 'token' instead." #-}

-- | Specifies a test TLS authorization request.
--
-- /Note:/ Consider using 'tlsContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiaTlsContext :: Lens.Lens' TestInvokeAuthorizer (Lude.Maybe TLSContext)
tiaTlsContext = Lens.lens (tlsContext :: TestInvokeAuthorizer -> Lude.Maybe TLSContext) (\s a -> s {tlsContext = a} :: TestInvokeAuthorizer)
{-# DEPRECATED tiaTlsContext "Use generic-lens or generic-optics with 'tlsContext' instead." #-}

-- | The signature made with the token and your custom authentication service's private key. This value must be Base-64-encoded.
--
-- /Note:/ Consider using 'tokenSignature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiaTokenSignature :: Lens.Lens' TestInvokeAuthorizer (Lude.Maybe Lude.Text)
tiaTokenSignature = Lens.lens (tokenSignature :: TestInvokeAuthorizer -> Lude.Maybe Lude.Text) (\s a -> s {tokenSignature = a} :: TestInvokeAuthorizer)
{-# DEPRECATED tiaTokenSignature "Use generic-lens or generic-optics with 'tokenSignature' instead." #-}

-- | Specifies a test HTTP authorization request.
--
-- /Note:/ Consider using 'httpContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiaHttpContext :: Lens.Lens' TestInvokeAuthorizer (Lude.Maybe HTTPContext)
tiaHttpContext = Lens.lens (httpContext :: TestInvokeAuthorizer -> Lude.Maybe HTTPContext) (\s a -> s {httpContext = a} :: TestInvokeAuthorizer)
{-# DEPRECATED tiaHttpContext "Use generic-lens or generic-optics with 'httpContext' instead." #-}

-- | Specifies a test MQTT authorization request.
--
-- /Note:/ Consider using 'mqttContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiaMqttContext :: Lens.Lens' TestInvokeAuthorizer (Lude.Maybe MqttContext)
tiaMqttContext = Lens.lens (mqttContext :: TestInvokeAuthorizer -> Lude.Maybe MqttContext) (\s a -> s {mqttContext = a} :: TestInvokeAuthorizer)
{-# DEPRECATED tiaMqttContext "Use generic-lens or generic-optics with 'mqttContext' instead." #-}

-- | The custom authorizer name.
--
-- /Note:/ Consider using 'authorizerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiaAuthorizerName :: Lens.Lens' TestInvokeAuthorizer Lude.Text
tiaAuthorizerName = Lens.lens (authorizerName :: TestInvokeAuthorizer -> Lude.Text) (\s a -> s {authorizerName = a} :: TestInvokeAuthorizer)
{-# DEPRECATED tiaAuthorizerName "Use generic-lens or generic-optics with 'authorizerName' instead." #-}

instance Lude.AWSRequest TestInvokeAuthorizer where
  type Rs TestInvokeAuthorizer = TestInvokeAuthorizerResponse
  request = Req.postJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          TestInvokeAuthorizerResponse'
            Lude.<$> (x Lude..?> "policyDocuments" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "principalId")
            Lude.<*> (x Lude..?> "disconnectAfterInSeconds")
            Lude.<*> (x Lude..?> "isAuthenticated")
            Lude.<*> (x Lude..?> "refreshAfterInSeconds")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders TestInvokeAuthorizer where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON TestInvokeAuthorizer where
  toJSON TestInvokeAuthorizer' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("token" Lude..=) Lude.<$> token,
            ("tlsContext" Lude..=) Lude.<$> tlsContext,
            ("tokenSignature" Lude..=) Lude.<$> tokenSignature,
            ("httpContext" Lude..=) Lude.<$> httpContext,
            ("mqttContext" Lude..=) Lude.<$> mqttContext
          ]
      )

instance Lude.ToPath TestInvokeAuthorizer where
  toPath TestInvokeAuthorizer' {..} =
    Lude.mconcat ["/authorizer/", Lude.toBS authorizerName, "/test"]

instance Lude.ToQuery TestInvokeAuthorizer where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkTestInvokeAuthorizerResponse' smart constructor.
data TestInvokeAuthorizerResponse = TestInvokeAuthorizerResponse'
  { policyDocuments ::
      Lude.Maybe [Lude.Text],
    principalId ::
      Lude.Maybe Lude.Text,
    disconnectAfterInSeconds ::
      Lude.Maybe Lude.Int,
    isAuthenticated ::
      Lude.Maybe Lude.Bool,
    refreshAfterInSeconds ::
      Lude.Maybe Lude.Int,
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
-- * 'disconnectAfterInSeconds' - The number of seconds after which the connection is terminated.
-- * 'isAuthenticated' - True if the token is authenticated, otherwise false.
-- * 'policyDocuments' - IAM policy documents.
-- * 'principalId' - The principal ID.
-- * 'refreshAfterInSeconds' - The number of seconds after which the temporary credentials are refreshed.
-- * 'responseStatus' - The response status code.
mkTestInvokeAuthorizerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  TestInvokeAuthorizerResponse
mkTestInvokeAuthorizerResponse pResponseStatus_ =
  TestInvokeAuthorizerResponse'
    { policyDocuments = Lude.Nothing,
      principalId = Lude.Nothing,
      disconnectAfterInSeconds = Lude.Nothing,
      isAuthenticated = Lude.Nothing,
      refreshAfterInSeconds = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | IAM policy documents.
--
-- /Note:/ Consider using 'policyDocuments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiarsPolicyDocuments :: Lens.Lens' TestInvokeAuthorizerResponse (Lude.Maybe [Lude.Text])
tiarsPolicyDocuments = Lens.lens (policyDocuments :: TestInvokeAuthorizerResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {policyDocuments = a} :: TestInvokeAuthorizerResponse)
{-# DEPRECATED tiarsPolicyDocuments "Use generic-lens or generic-optics with 'policyDocuments' instead." #-}

-- | The principal ID.
--
-- /Note:/ Consider using 'principalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiarsPrincipalId :: Lens.Lens' TestInvokeAuthorizerResponse (Lude.Maybe Lude.Text)
tiarsPrincipalId = Lens.lens (principalId :: TestInvokeAuthorizerResponse -> Lude.Maybe Lude.Text) (\s a -> s {principalId = a} :: TestInvokeAuthorizerResponse)
{-# DEPRECATED tiarsPrincipalId "Use generic-lens or generic-optics with 'principalId' instead." #-}

-- | The number of seconds after which the connection is terminated.
--
-- /Note:/ Consider using 'disconnectAfterInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiarsDisconnectAfterInSeconds :: Lens.Lens' TestInvokeAuthorizerResponse (Lude.Maybe Lude.Int)
tiarsDisconnectAfterInSeconds = Lens.lens (disconnectAfterInSeconds :: TestInvokeAuthorizerResponse -> Lude.Maybe Lude.Int) (\s a -> s {disconnectAfterInSeconds = a} :: TestInvokeAuthorizerResponse)
{-# DEPRECATED tiarsDisconnectAfterInSeconds "Use generic-lens or generic-optics with 'disconnectAfterInSeconds' instead." #-}

-- | True if the token is authenticated, otherwise false.
--
-- /Note:/ Consider using 'isAuthenticated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiarsIsAuthenticated :: Lens.Lens' TestInvokeAuthorizerResponse (Lude.Maybe Lude.Bool)
tiarsIsAuthenticated = Lens.lens (isAuthenticated :: TestInvokeAuthorizerResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isAuthenticated = a} :: TestInvokeAuthorizerResponse)
{-# DEPRECATED tiarsIsAuthenticated "Use generic-lens or generic-optics with 'isAuthenticated' instead." #-}

-- | The number of seconds after which the temporary credentials are refreshed.
--
-- /Note:/ Consider using 'refreshAfterInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiarsRefreshAfterInSeconds :: Lens.Lens' TestInvokeAuthorizerResponse (Lude.Maybe Lude.Int)
tiarsRefreshAfterInSeconds = Lens.lens (refreshAfterInSeconds :: TestInvokeAuthorizerResponse -> Lude.Maybe Lude.Int) (\s a -> s {refreshAfterInSeconds = a} :: TestInvokeAuthorizerResponse)
{-# DEPRECATED tiarsRefreshAfterInSeconds "Use generic-lens or generic-optics with 'refreshAfterInSeconds' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiarsResponseStatus :: Lens.Lens' TestInvokeAuthorizerResponse Lude.Int
tiarsResponseStatus = Lens.lens (responseStatus :: TestInvokeAuthorizerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: TestInvokeAuthorizerResponse)
{-# DEPRECATED tiarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
