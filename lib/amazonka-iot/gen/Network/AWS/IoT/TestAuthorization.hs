{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.TestAuthorization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Tests if a specified principal is authorized to perform an AWS IoT action on a specified resource. Use this to test and debug the authorization behavior of devices that connect to the AWS IoT device gateway.
module Network.AWS.IoT.TestAuthorization
  ( -- * Creating a request
    TestAuthorization (..),
    mkTestAuthorization,

    -- ** Request lenses
    taClientId,
    taPolicyNamesToAdd,
    taPrincipal,
    taAuthInfos,
    taCognitoIdentityPoolId,
    taPolicyNamesToSkip,

    -- * Destructuring the response
    TestAuthorizationResponse (..),
    mkTestAuthorizationResponse,

    -- ** Response lenses
    tarsAuthResults,
    tarsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkTestAuthorization' smart constructor.
data TestAuthorization = TestAuthorization'
  { -- | The MQTT client ID.
    clientId :: Lude.Maybe Lude.Text,
    -- | When testing custom authorization, the policies specified here are treated as if they are attached to the principal being authorized.
    policyNamesToAdd :: Lude.Maybe [Lude.Text],
    -- | The principal. Valid principals are CertificateArn (arn:aws:iot:/region/ :/accountId/ :cert//certificateId/ ), thingGroupArn (arn:aws:iot:/region/ :/accountId/ :thinggroup//groupName/ ) and CognitoId (/region/ :/id/ ).
    principal :: Lude.Maybe Lude.Text,
    -- | A list of authorization info objects. Simulating authorization will create a response for each @authInfo@ object in the list.
    authInfos :: Lude.NonEmpty AuthInfo,
    -- | The Cognito identity pool ID.
    cognitoIdentityPoolId :: Lude.Maybe Lude.Text,
    -- | When testing custom authorization, the policies specified here are treated as if they are not attached to the principal being authorized.
    policyNamesToSkip :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TestAuthorization' with the minimum fields required to make a request.
--
-- * 'clientId' - The MQTT client ID.
-- * 'policyNamesToAdd' - When testing custom authorization, the policies specified here are treated as if they are attached to the principal being authorized.
-- * 'principal' - The principal. Valid principals are CertificateArn (arn:aws:iot:/region/ :/accountId/ :cert//certificateId/ ), thingGroupArn (arn:aws:iot:/region/ :/accountId/ :thinggroup//groupName/ ) and CognitoId (/region/ :/id/ ).
-- * 'authInfos' - A list of authorization info objects. Simulating authorization will create a response for each @authInfo@ object in the list.
-- * 'cognitoIdentityPoolId' - The Cognito identity pool ID.
-- * 'policyNamesToSkip' - When testing custom authorization, the policies specified here are treated as if they are not attached to the principal being authorized.
mkTestAuthorization ::
  -- | 'authInfos'
  Lude.NonEmpty AuthInfo ->
  TestAuthorization
mkTestAuthorization pAuthInfos_ =
  TestAuthorization'
    { clientId = Lude.Nothing,
      policyNamesToAdd = Lude.Nothing,
      principal = Lude.Nothing,
      authInfos = pAuthInfos_,
      cognitoIdentityPoolId = Lude.Nothing,
      policyNamesToSkip = Lude.Nothing
    }

-- | The MQTT client ID.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taClientId :: Lens.Lens' TestAuthorization (Lude.Maybe Lude.Text)
taClientId = Lens.lens (clientId :: TestAuthorization -> Lude.Maybe Lude.Text) (\s a -> s {clientId = a} :: TestAuthorization)
{-# DEPRECATED taClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

-- | When testing custom authorization, the policies specified here are treated as if they are attached to the principal being authorized.
--
-- /Note:/ Consider using 'policyNamesToAdd' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taPolicyNamesToAdd :: Lens.Lens' TestAuthorization (Lude.Maybe [Lude.Text])
taPolicyNamesToAdd = Lens.lens (policyNamesToAdd :: TestAuthorization -> Lude.Maybe [Lude.Text]) (\s a -> s {policyNamesToAdd = a} :: TestAuthorization)
{-# DEPRECATED taPolicyNamesToAdd "Use generic-lens or generic-optics with 'policyNamesToAdd' instead." #-}

-- | The principal. Valid principals are CertificateArn (arn:aws:iot:/region/ :/accountId/ :cert//certificateId/ ), thingGroupArn (arn:aws:iot:/region/ :/accountId/ :thinggroup//groupName/ ) and CognitoId (/region/ :/id/ ).
--
-- /Note:/ Consider using 'principal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taPrincipal :: Lens.Lens' TestAuthorization (Lude.Maybe Lude.Text)
taPrincipal = Lens.lens (principal :: TestAuthorization -> Lude.Maybe Lude.Text) (\s a -> s {principal = a} :: TestAuthorization)
{-# DEPRECATED taPrincipal "Use generic-lens or generic-optics with 'principal' instead." #-}

-- | A list of authorization info objects. Simulating authorization will create a response for each @authInfo@ object in the list.
--
-- /Note:/ Consider using 'authInfos' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taAuthInfos :: Lens.Lens' TestAuthorization (Lude.NonEmpty AuthInfo)
taAuthInfos = Lens.lens (authInfos :: TestAuthorization -> Lude.NonEmpty AuthInfo) (\s a -> s {authInfos = a} :: TestAuthorization)
{-# DEPRECATED taAuthInfos "Use generic-lens or generic-optics with 'authInfos' instead." #-}

-- | The Cognito identity pool ID.
--
-- /Note:/ Consider using 'cognitoIdentityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taCognitoIdentityPoolId :: Lens.Lens' TestAuthorization (Lude.Maybe Lude.Text)
taCognitoIdentityPoolId = Lens.lens (cognitoIdentityPoolId :: TestAuthorization -> Lude.Maybe Lude.Text) (\s a -> s {cognitoIdentityPoolId = a} :: TestAuthorization)
{-# DEPRECATED taCognitoIdentityPoolId "Use generic-lens or generic-optics with 'cognitoIdentityPoolId' instead." #-}

-- | When testing custom authorization, the policies specified here are treated as if they are not attached to the principal being authorized.
--
-- /Note:/ Consider using 'policyNamesToSkip' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taPolicyNamesToSkip :: Lens.Lens' TestAuthorization (Lude.Maybe [Lude.Text])
taPolicyNamesToSkip = Lens.lens (policyNamesToSkip :: TestAuthorization -> Lude.Maybe [Lude.Text]) (\s a -> s {policyNamesToSkip = a} :: TestAuthorization)
{-# DEPRECATED taPolicyNamesToSkip "Use generic-lens or generic-optics with 'policyNamesToSkip' instead." #-}

instance Lude.AWSRequest TestAuthorization where
  type Rs TestAuthorization = TestAuthorizationResponse
  request = Req.postJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          TestAuthorizationResponse'
            Lude.<$> (x Lude..?> "authResults" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders TestAuthorization where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON TestAuthorization where
  toJSON TestAuthorization' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("policyNamesToAdd" Lude..=) Lude.<$> policyNamesToAdd,
            ("principal" Lude..=) Lude.<$> principal,
            Lude.Just ("authInfos" Lude..= authInfos),
            ("cognitoIdentityPoolId" Lude..=) Lude.<$> cognitoIdentityPoolId,
            ("policyNamesToSkip" Lude..=) Lude.<$> policyNamesToSkip
          ]
      )

instance Lude.ToPath TestAuthorization where
  toPath = Lude.const "/test-authorization"

instance Lude.ToQuery TestAuthorization where
  toQuery TestAuthorization' {..} =
    Lude.mconcat ["clientId" Lude.=: clientId]

-- | /See:/ 'mkTestAuthorizationResponse' smart constructor.
data TestAuthorizationResponse = TestAuthorizationResponse'
  { -- | The authentication results.
    authResults :: Lude.Maybe [AuthResult],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TestAuthorizationResponse' with the minimum fields required to make a request.
--
-- * 'authResults' - The authentication results.
-- * 'responseStatus' - The response status code.
mkTestAuthorizationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  TestAuthorizationResponse
mkTestAuthorizationResponse pResponseStatus_ =
  TestAuthorizationResponse'
    { authResults = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The authentication results.
--
-- /Note:/ Consider using 'authResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tarsAuthResults :: Lens.Lens' TestAuthorizationResponse (Lude.Maybe [AuthResult])
tarsAuthResults = Lens.lens (authResults :: TestAuthorizationResponse -> Lude.Maybe [AuthResult]) (\s a -> s {authResults = a} :: TestAuthorizationResponse)
{-# DEPRECATED tarsAuthResults "Use generic-lens or generic-optics with 'authResults' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tarsResponseStatus :: Lens.Lens' TestAuthorizationResponse Lude.Int
tarsResponseStatus = Lens.lens (responseStatus :: TestAuthorizationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: TestAuthorizationResponse)
{-# DEPRECATED tarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
