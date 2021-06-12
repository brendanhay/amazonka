{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ResourceIdentifier
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ResourceIdentifier where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.PolicyVersionIdentifier
import qualified Network.AWS.Lens as Lens

-- | Information that identifies the noncompliant resource.
--
-- /See:/ 'newResourceIdentifier' smart constructor.
data ResourceIdentifier = ResourceIdentifier'
  { -- | The ARN of the role alias that has overly permissive actions.
    roleAliasArn :: Core.Maybe Core.Text,
    -- | The client ID.
    clientId :: Core.Maybe Core.Text,
    -- | The ARN of the IAM role that has overly permissive actions.
    iamRoleArn :: Core.Maybe Core.Text,
    -- | The ID of the Amazon Cognito identity pool.
    cognitoIdentityPoolId :: Core.Maybe Core.Text,
    -- | The account with which the resource is associated.
    account :: Core.Maybe Core.Text,
    -- | The version of the policy associated with the resource.
    policyVersionIdentifier :: Core.Maybe PolicyVersionIdentifier,
    -- | The ID of the certificate attached to the resource.
    deviceCertificateId :: Core.Maybe Core.Text,
    -- | The ID of the CA certificate used to authorize the certificate.
    caCertificateId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ResourceIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleAliasArn', 'resourceIdentifier_roleAliasArn' - The ARN of the role alias that has overly permissive actions.
--
-- 'clientId', 'resourceIdentifier_clientId' - The client ID.
--
-- 'iamRoleArn', 'resourceIdentifier_iamRoleArn' - The ARN of the IAM role that has overly permissive actions.
--
-- 'cognitoIdentityPoolId', 'resourceIdentifier_cognitoIdentityPoolId' - The ID of the Amazon Cognito identity pool.
--
-- 'account', 'resourceIdentifier_account' - The account with which the resource is associated.
--
-- 'policyVersionIdentifier', 'resourceIdentifier_policyVersionIdentifier' - The version of the policy associated with the resource.
--
-- 'deviceCertificateId', 'resourceIdentifier_deviceCertificateId' - The ID of the certificate attached to the resource.
--
-- 'caCertificateId', 'resourceIdentifier_caCertificateId' - The ID of the CA certificate used to authorize the certificate.
newResourceIdentifier ::
  ResourceIdentifier
newResourceIdentifier =
  ResourceIdentifier'
    { roleAliasArn = Core.Nothing,
      clientId = Core.Nothing,
      iamRoleArn = Core.Nothing,
      cognitoIdentityPoolId = Core.Nothing,
      account = Core.Nothing,
      policyVersionIdentifier = Core.Nothing,
      deviceCertificateId = Core.Nothing,
      caCertificateId = Core.Nothing
    }

-- | The ARN of the role alias that has overly permissive actions.
resourceIdentifier_roleAliasArn :: Lens.Lens' ResourceIdentifier (Core.Maybe Core.Text)
resourceIdentifier_roleAliasArn = Lens.lens (\ResourceIdentifier' {roleAliasArn} -> roleAliasArn) (\s@ResourceIdentifier' {} a -> s {roleAliasArn = a} :: ResourceIdentifier)

-- | The client ID.
resourceIdentifier_clientId :: Lens.Lens' ResourceIdentifier (Core.Maybe Core.Text)
resourceIdentifier_clientId = Lens.lens (\ResourceIdentifier' {clientId} -> clientId) (\s@ResourceIdentifier' {} a -> s {clientId = a} :: ResourceIdentifier)

-- | The ARN of the IAM role that has overly permissive actions.
resourceIdentifier_iamRoleArn :: Lens.Lens' ResourceIdentifier (Core.Maybe Core.Text)
resourceIdentifier_iamRoleArn = Lens.lens (\ResourceIdentifier' {iamRoleArn} -> iamRoleArn) (\s@ResourceIdentifier' {} a -> s {iamRoleArn = a} :: ResourceIdentifier)

-- | The ID of the Amazon Cognito identity pool.
resourceIdentifier_cognitoIdentityPoolId :: Lens.Lens' ResourceIdentifier (Core.Maybe Core.Text)
resourceIdentifier_cognitoIdentityPoolId = Lens.lens (\ResourceIdentifier' {cognitoIdentityPoolId} -> cognitoIdentityPoolId) (\s@ResourceIdentifier' {} a -> s {cognitoIdentityPoolId = a} :: ResourceIdentifier)

-- | The account with which the resource is associated.
resourceIdentifier_account :: Lens.Lens' ResourceIdentifier (Core.Maybe Core.Text)
resourceIdentifier_account = Lens.lens (\ResourceIdentifier' {account} -> account) (\s@ResourceIdentifier' {} a -> s {account = a} :: ResourceIdentifier)

-- | The version of the policy associated with the resource.
resourceIdentifier_policyVersionIdentifier :: Lens.Lens' ResourceIdentifier (Core.Maybe PolicyVersionIdentifier)
resourceIdentifier_policyVersionIdentifier = Lens.lens (\ResourceIdentifier' {policyVersionIdentifier} -> policyVersionIdentifier) (\s@ResourceIdentifier' {} a -> s {policyVersionIdentifier = a} :: ResourceIdentifier)

-- | The ID of the certificate attached to the resource.
resourceIdentifier_deviceCertificateId :: Lens.Lens' ResourceIdentifier (Core.Maybe Core.Text)
resourceIdentifier_deviceCertificateId = Lens.lens (\ResourceIdentifier' {deviceCertificateId} -> deviceCertificateId) (\s@ResourceIdentifier' {} a -> s {deviceCertificateId = a} :: ResourceIdentifier)

-- | The ID of the CA certificate used to authorize the certificate.
resourceIdentifier_caCertificateId :: Lens.Lens' ResourceIdentifier (Core.Maybe Core.Text)
resourceIdentifier_caCertificateId = Lens.lens (\ResourceIdentifier' {caCertificateId} -> caCertificateId) (\s@ResourceIdentifier' {} a -> s {caCertificateId = a} :: ResourceIdentifier)

instance Core.FromJSON ResourceIdentifier where
  parseJSON =
    Core.withObject
      "ResourceIdentifier"
      ( \x ->
          ResourceIdentifier'
            Core.<$> (x Core..:? "roleAliasArn")
            Core.<*> (x Core..:? "clientId")
            Core.<*> (x Core..:? "iamRoleArn")
            Core.<*> (x Core..:? "cognitoIdentityPoolId")
            Core.<*> (x Core..:? "account")
            Core.<*> (x Core..:? "policyVersionIdentifier")
            Core.<*> (x Core..:? "deviceCertificateId")
            Core.<*> (x Core..:? "caCertificateId")
      )

instance Core.Hashable ResourceIdentifier

instance Core.NFData ResourceIdentifier

instance Core.ToJSON ResourceIdentifier where
  toJSON ResourceIdentifier' {..} =
    Core.object
      ( Core.catMaybes
          [ ("roleAliasArn" Core..=) Core.<$> roleAliasArn,
            ("clientId" Core..=) Core.<$> clientId,
            ("iamRoleArn" Core..=) Core.<$> iamRoleArn,
            ("cognitoIdentityPoolId" Core..=)
              Core.<$> cognitoIdentityPoolId,
            ("account" Core..=) Core.<$> account,
            ("policyVersionIdentifier" Core..=)
              Core.<$> policyVersionIdentifier,
            ("deviceCertificateId" Core..=)
              Core.<$> deviceCertificateId,
            ("caCertificateId" Core..=)
              Core.<$> caCertificateId
          ]
      )
