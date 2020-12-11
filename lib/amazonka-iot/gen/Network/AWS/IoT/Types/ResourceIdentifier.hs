-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ResourceIdentifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ResourceIdentifier
  ( ResourceIdentifier (..),

    -- * Smart constructor
    mkResourceIdentifier,

    -- * Lenses
    riIamRoleARN,
    riClientId,
    riRoleAliasARN,
    riCaCertificateId,
    riDeviceCertificateId,
    riAccount,
    riPolicyVersionIdentifier,
    riCognitoIdentityPoolId,
  )
where

import Network.AWS.IoT.Types.PolicyVersionIdentifier
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information that identifies the noncompliant resource.
--
-- /See:/ 'mkResourceIdentifier' smart constructor.
data ResourceIdentifier = ResourceIdentifier'
  { iamRoleARN ::
      Lude.Maybe Lude.Text,
    clientId :: Lude.Maybe Lude.Text,
    roleAliasARN :: Lude.Maybe Lude.Text,
    caCertificateId :: Lude.Maybe Lude.Text,
    deviceCertificateId :: Lude.Maybe Lude.Text,
    account :: Lude.Maybe Lude.Text,
    policyVersionIdentifier ::
      Lude.Maybe PolicyVersionIdentifier,
    cognitoIdentityPoolId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceIdentifier' with the minimum fields required to make a request.
--
-- * 'account' - The account with which the resource is associated.
-- * 'caCertificateId' - The ID of the CA certificate used to authorize the certificate.
-- * 'clientId' - The client ID.
-- * 'cognitoIdentityPoolId' - The ID of the Amazon Cognito identity pool.
-- * 'deviceCertificateId' - The ID of the certificate attached to the resource.
-- * 'iamRoleARN' - The ARN of the IAM role that has overly permissive actions.
-- * 'policyVersionIdentifier' - The version of the policy associated with the resource.
-- * 'roleAliasARN' - The ARN of the role alias that has overly permissive actions.
mkResourceIdentifier ::
  ResourceIdentifier
mkResourceIdentifier =
  ResourceIdentifier'
    { iamRoleARN = Lude.Nothing,
      clientId = Lude.Nothing,
      roleAliasARN = Lude.Nothing,
      caCertificateId = Lude.Nothing,
      deviceCertificateId = Lude.Nothing,
      account = Lude.Nothing,
      policyVersionIdentifier = Lude.Nothing,
      cognitoIdentityPoolId = Lude.Nothing
    }

-- | The ARN of the IAM role that has overly permissive actions.
--
-- /Note:/ Consider using 'iamRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riIamRoleARN :: Lens.Lens' ResourceIdentifier (Lude.Maybe Lude.Text)
riIamRoleARN = Lens.lens (iamRoleARN :: ResourceIdentifier -> Lude.Maybe Lude.Text) (\s a -> s {iamRoleARN = a} :: ResourceIdentifier)
{-# DEPRECATED riIamRoleARN "Use generic-lens or generic-optics with 'iamRoleARN' instead." #-}

-- | The client ID.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riClientId :: Lens.Lens' ResourceIdentifier (Lude.Maybe Lude.Text)
riClientId = Lens.lens (clientId :: ResourceIdentifier -> Lude.Maybe Lude.Text) (\s a -> s {clientId = a} :: ResourceIdentifier)
{-# DEPRECATED riClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

-- | The ARN of the role alias that has overly permissive actions.
--
-- /Note:/ Consider using 'roleAliasARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riRoleAliasARN :: Lens.Lens' ResourceIdentifier (Lude.Maybe Lude.Text)
riRoleAliasARN = Lens.lens (roleAliasARN :: ResourceIdentifier -> Lude.Maybe Lude.Text) (\s a -> s {roleAliasARN = a} :: ResourceIdentifier)
{-# DEPRECATED riRoleAliasARN "Use generic-lens or generic-optics with 'roleAliasARN' instead." #-}

-- | The ID of the CA certificate used to authorize the certificate.
--
-- /Note:/ Consider using 'caCertificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riCaCertificateId :: Lens.Lens' ResourceIdentifier (Lude.Maybe Lude.Text)
riCaCertificateId = Lens.lens (caCertificateId :: ResourceIdentifier -> Lude.Maybe Lude.Text) (\s a -> s {caCertificateId = a} :: ResourceIdentifier)
{-# DEPRECATED riCaCertificateId "Use generic-lens or generic-optics with 'caCertificateId' instead." #-}

-- | The ID of the certificate attached to the resource.
--
-- /Note:/ Consider using 'deviceCertificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riDeviceCertificateId :: Lens.Lens' ResourceIdentifier (Lude.Maybe Lude.Text)
riDeviceCertificateId = Lens.lens (deviceCertificateId :: ResourceIdentifier -> Lude.Maybe Lude.Text) (\s a -> s {deviceCertificateId = a} :: ResourceIdentifier)
{-# DEPRECATED riDeviceCertificateId "Use generic-lens or generic-optics with 'deviceCertificateId' instead." #-}

-- | The account with which the resource is associated.
--
-- /Note:/ Consider using 'account' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riAccount :: Lens.Lens' ResourceIdentifier (Lude.Maybe Lude.Text)
riAccount = Lens.lens (account :: ResourceIdentifier -> Lude.Maybe Lude.Text) (\s a -> s {account = a} :: ResourceIdentifier)
{-# DEPRECATED riAccount "Use generic-lens or generic-optics with 'account' instead." #-}

-- | The version of the policy associated with the resource.
--
-- /Note:/ Consider using 'policyVersionIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riPolicyVersionIdentifier :: Lens.Lens' ResourceIdentifier (Lude.Maybe PolicyVersionIdentifier)
riPolicyVersionIdentifier = Lens.lens (policyVersionIdentifier :: ResourceIdentifier -> Lude.Maybe PolicyVersionIdentifier) (\s a -> s {policyVersionIdentifier = a} :: ResourceIdentifier)
{-# DEPRECATED riPolicyVersionIdentifier "Use generic-lens or generic-optics with 'policyVersionIdentifier' instead." #-}

-- | The ID of the Amazon Cognito identity pool.
--
-- /Note:/ Consider using 'cognitoIdentityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riCognitoIdentityPoolId :: Lens.Lens' ResourceIdentifier (Lude.Maybe Lude.Text)
riCognitoIdentityPoolId = Lens.lens (cognitoIdentityPoolId :: ResourceIdentifier -> Lude.Maybe Lude.Text) (\s a -> s {cognitoIdentityPoolId = a} :: ResourceIdentifier)
{-# DEPRECATED riCognitoIdentityPoolId "Use generic-lens or generic-optics with 'cognitoIdentityPoolId' instead." #-}

instance Lude.FromJSON ResourceIdentifier where
  parseJSON =
    Lude.withObject
      "ResourceIdentifier"
      ( \x ->
          ResourceIdentifier'
            Lude.<$> (x Lude..:? "iamRoleArn")
            Lude.<*> (x Lude..:? "clientId")
            Lude.<*> (x Lude..:? "roleAliasArn")
            Lude.<*> (x Lude..:? "caCertificateId")
            Lude.<*> (x Lude..:? "deviceCertificateId")
            Lude.<*> (x Lude..:? "account")
            Lude.<*> (x Lude..:? "policyVersionIdentifier")
            Lude.<*> (x Lude..:? "cognitoIdentityPoolId")
      )

instance Lude.ToJSON ResourceIdentifier where
  toJSON ResourceIdentifier' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("iamRoleArn" Lude..=) Lude.<$> iamRoleARN,
            ("clientId" Lude..=) Lude.<$> clientId,
            ("roleAliasArn" Lude..=) Lude.<$> roleAliasARN,
            ("caCertificateId" Lude..=) Lude.<$> caCertificateId,
            ("deviceCertificateId" Lude..=) Lude.<$> deviceCertificateId,
            ("account" Lude..=) Lude.<$> account,
            ("policyVersionIdentifier" Lude..=)
              Lude.<$> policyVersionIdentifier,
            ("cognitoIdentityPoolId" Lude..=) Lude.<$> cognitoIdentityPoolId
          ]
      )
