{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    riAccount,
    riCaCertificateId,
    riClientId,
    riCognitoIdentityPoolId,
    riDeviceCertificateId,
    riIamRoleArn,
    riPolicyVersionIdentifier,
    riRoleAliasArn,
  )
where

import qualified Network.AWS.IoT.Types.AwsAccountId as Types
import qualified Network.AWS.IoT.Types.CertificateId as Types
import qualified Network.AWS.IoT.Types.ClientId as Types
import qualified Network.AWS.IoT.Types.CognitoIdentityPoolId as Types
import qualified Network.AWS.IoT.Types.IamRoleArn as Types
import qualified Network.AWS.IoT.Types.PolicyVersionIdentifier as Types
import qualified Network.AWS.IoT.Types.RoleAliasArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information that identifies the noncompliant resource.
--
-- /See:/ 'mkResourceIdentifier' smart constructor.
data ResourceIdentifier = ResourceIdentifier'
  { -- | The account with which the resource is associated.
    account :: Core.Maybe Types.AwsAccountId,
    -- | The ID of the CA certificate used to authorize the certificate.
    caCertificateId :: Core.Maybe Types.CertificateId,
    -- | The client ID.
    clientId :: Core.Maybe Types.ClientId,
    -- | The ID of the Amazon Cognito identity pool.
    cognitoIdentityPoolId :: Core.Maybe Types.CognitoIdentityPoolId,
    -- | The ID of the certificate attached to the resource.
    deviceCertificateId :: Core.Maybe Types.CertificateId,
    -- | The ARN of the IAM role that has overly permissive actions.
    iamRoleArn :: Core.Maybe Types.IamRoleArn,
    -- | The version of the policy associated with the resource.
    policyVersionIdentifier :: Core.Maybe Types.PolicyVersionIdentifier,
    -- | The ARN of the role alias that has overly permissive actions.
    roleAliasArn :: Core.Maybe Types.RoleAliasArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceIdentifier' value with any optional fields omitted.
mkResourceIdentifier ::
  ResourceIdentifier
mkResourceIdentifier =
  ResourceIdentifier'
    { account = Core.Nothing,
      caCertificateId = Core.Nothing,
      clientId = Core.Nothing,
      cognitoIdentityPoolId = Core.Nothing,
      deviceCertificateId = Core.Nothing,
      iamRoleArn = Core.Nothing,
      policyVersionIdentifier = Core.Nothing,
      roleAliasArn = Core.Nothing
    }

-- | The account with which the resource is associated.
--
-- /Note:/ Consider using 'account' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riAccount :: Lens.Lens' ResourceIdentifier (Core.Maybe Types.AwsAccountId)
riAccount = Lens.field @"account"
{-# DEPRECATED riAccount "Use generic-lens or generic-optics with 'account' instead." #-}

-- | The ID of the CA certificate used to authorize the certificate.
--
-- /Note:/ Consider using 'caCertificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riCaCertificateId :: Lens.Lens' ResourceIdentifier (Core.Maybe Types.CertificateId)
riCaCertificateId = Lens.field @"caCertificateId"
{-# DEPRECATED riCaCertificateId "Use generic-lens or generic-optics with 'caCertificateId' instead." #-}

-- | The client ID.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riClientId :: Lens.Lens' ResourceIdentifier (Core.Maybe Types.ClientId)
riClientId = Lens.field @"clientId"
{-# DEPRECATED riClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

-- | The ID of the Amazon Cognito identity pool.
--
-- /Note:/ Consider using 'cognitoIdentityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riCognitoIdentityPoolId :: Lens.Lens' ResourceIdentifier (Core.Maybe Types.CognitoIdentityPoolId)
riCognitoIdentityPoolId = Lens.field @"cognitoIdentityPoolId"
{-# DEPRECATED riCognitoIdentityPoolId "Use generic-lens or generic-optics with 'cognitoIdentityPoolId' instead." #-}

-- | The ID of the certificate attached to the resource.
--
-- /Note:/ Consider using 'deviceCertificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riDeviceCertificateId :: Lens.Lens' ResourceIdentifier (Core.Maybe Types.CertificateId)
riDeviceCertificateId = Lens.field @"deviceCertificateId"
{-# DEPRECATED riDeviceCertificateId "Use generic-lens or generic-optics with 'deviceCertificateId' instead." #-}

-- | The ARN of the IAM role that has overly permissive actions.
--
-- /Note:/ Consider using 'iamRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riIamRoleArn :: Lens.Lens' ResourceIdentifier (Core.Maybe Types.IamRoleArn)
riIamRoleArn = Lens.field @"iamRoleArn"
{-# DEPRECATED riIamRoleArn "Use generic-lens or generic-optics with 'iamRoleArn' instead." #-}

-- | The version of the policy associated with the resource.
--
-- /Note:/ Consider using 'policyVersionIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riPolicyVersionIdentifier :: Lens.Lens' ResourceIdentifier (Core.Maybe Types.PolicyVersionIdentifier)
riPolicyVersionIdentifier = Lens.field @"policyVersionIdentifier"
{-# DEPRECATED riPolicyVersionIdentifier "Use generic-lens or generic-optics with 'policyVersionIdentifier' instead." #-}

-- | The ARN of the role alias that has overly permissive actions.
--
-- /Note:/ Consider using 'roleAliasArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riRoleAliasArn :: Lens.Lens' ResourceIdentifier (Core.Maybe Types.RoleAliasArn)
riRoleAliasArn = Lens.field @"roleAliasArn"
{-# DEPRECATED riRoleAliasArn "Use generic-lens or generic-optics with 'roleAliasArn' instead." #-}

instance Core.FromJSON ResourceIdentifier where
  toJSON ResourceIdentifier {..} =
    Core.object
      ( Core.catMaybes
          [ ("account" Core..=) Core.<$> account,
            ("caCertificateId" Core..=) Core.<$> caCertificateId,
            ("clientId" Core..=) Core.<$> clientId,
            ("cognitoIdentityPoolId" Core..=) Core.<$> cognitoIdentityPoolId,
            ("deviceCertificateId" Core..=) Core.<$> deviceCertificateId,
            ("iamRoleArn" Core..=) Core.<$> iamRoleArn,
            ("policyVersionIdentifier" Core..=)
              Core.<$> policyVersionIdentifier,
            ("roleAliasArn" Core..=) Core.<$> roleAliasArn
          ]
      )

instance Core.FromJSON ResourceIdentifier where
  parseJSON =
    Core.withObject "ResourceIdentifier" Core.$
      \x ->
        ResourceIdentifier'
          Core.<$> (x Core..:? "account")
          Core.<*> (x Core..:? "caCertificateId")
          Core.<*> (x Core..:? "clientId")
          Core.<*> (x Core..:? "cognitoIdentityPoolId")
          Core.<*> (x Core..:? "deviceCertificateId")
          Core.<*> (x Core..:? "iamRoleArn")
          Core.<*> (x Core..:? "policyVersionIdentifier")
          Core.<*> (x Core..:? "roleAliasArn")
