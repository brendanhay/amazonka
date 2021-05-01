{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.IoT.Types.PolicyVersionIdentifier
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information that identifies the noncompliant resource.
--
-- /See:/ 'newResourceIdentifier' smart constructor.
data ResourceIdentifier = ResourceIdentifier'
  { -- | The ARN of the role alias that has overly permissive actions.
    roleAliasArn :: Prelude.Maybe Prelude.Text,
    -- | The client ID.
    clientId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the IAM role that has overly permissive actions.
    iamRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Cognito identity pool.
    cognitoIdentityPoolId :: Prelude.Maybe Prelude.Text,
    -- | The account with which the resource is associated.
    account :: Prelude.Maybe Prelude.Text,
    -- | The version of the policy associated with the resource.
    policyVersionIdentifier :: Prelude.Maybe PolicyVersionIdentifier,
    -- | The ID of the certificate attached to the resource.
    deviceCertificateId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the CA certificate used to authorize the certificate.
    caCertificateId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { roleAliasArn = Prelude.Nothing,
      clientId = Prelude.Nothing,
      iamRoleArn = Prelude.Nothing,
      cognitoIdentityPoolId = Prelude.Nothing,
      account = Prelude.Nothing,
      policyVersionIdentifier = Prelude.Nothing,
      deviceCertificateId = Prelude.Nothing,
      caCertificateId = Prelude.Nothing
    }

-- | The ARN of the role alias that has overly permissive actions.
resourceIdentifier_roleAliasArn :: Lens.Lens' ResourceIdentifier (Prelude.Maybe Prelude.Text)
resourceIdentifier_roleAliasArn = Lens.lens (\ResourceIdentifier' {roleAliasArn} -> roleAliasArn) (\s@ResourceIdentifier' {} a -> s {roleAliasArn = a} :: ResourceIdentifier)

-- | The client ID.
resourceIdentifier_clientId :: Lens.Lens' ResourceIdentifier (Prelude.Maybe Prelude.Text)
resourceIdentifier_clientId = Lens.lens (\ResourceIdentifier' {clientId} -> clientId) (\s@ResourceIdentifier' {} a -> s {clientId = a} :: ResourceIdentifier)

-- | The ARN of the IAM role that has overly permissive actions.
resourceIdentifier_iamRoleArn :: Lens.Lens' ResourceIdentifier (Prelude.Maybe Prelude.Text)
resourceIdentifier_iamRoleArn = Lens.lens (\ResourceIdentifier' {iamRoleArn} -> iamRoleArn) (\s@ResourceIdentifier' {} a -> s {iamRoleArn = a} :: ResourceIdentifier)

-- | The ID of the Amazon Cognito identity pool.
resourceIdentifier_cognitoIdentityPoolId :: Lens.Lens' ResourceIdentifier (Prelude.Maybe Prelude.Text)
resourceIdentifier_cognitoIdentityPoolId = Lens.lens (\ResourceIdentifier' {cognitoIdentityPoolId} -> cognitoIdentityPoolId) (\s@ResourceIdentifier' {} a -> s {cognitoIdentityPoolId = a} :: ResourceIdentifier)

-- | The account with which the resource is associated.
resourceIdentifier_account :: Lens.Lens' ResourceIdentifier (Prelude.Maybe Prelude.Text)
resourceIdentifier_account = Lens.lens (\ResourceIdentifier' {account} -> account) (\s@ResourceIdentifier' {} a -> s {account = a} :: ResourceIdentifier)

-- | The version of the policy associated with the resource.
resourceIdentifier_policyVersionIdentifier :: Lens.Lens' ResourceIdentifier (Prelude.Maybe PolicyVersionIdentifier)
resourceIdentifier_policyVersionIdentifier = Lens.lens (\ResourceIdentifier' {policyVersionIdentifier} -> policyVersionIdentifier) (\s@ResourceIdentifier' {} a -> s {policyVersionIdentifier = a} :: ResourceIdentifier)

-- | The ID of the certificate attached to the resource.
resourceIdentifier_deviceCertificateId :: Lens.Lens' ResourceIdentifier (Prelude.Maybe Prelude.Text)
resourceIdentifier_deviceCertificateId = Lens.lens (\ResourceIdentifier' {deviceCertificateId} -> deviceCertificateId) (\s@ResourceIdentifier' {} a -> s {deviceCertificateId = a} :: ResourceIdentifier)

-- | The ID of the CA certificate used to authorize the certificate.
resourceIdentifier_caCertificateId :: Lens.Lens' ResourceIdentifier (Prelude.Maybe Prelude.Text)
resourceIdentifier_caCertificateId = Lens.lens (\ResourceIdentifier' {caCertificateId} -> caCertificateId) (\s@ResourceIdentifier' {} a -> s {caCertificateId = a} :: ResourceIdentifier)

instance Prelude.FromJSON ResourceIdentifier where
  parseJSON =
    Prelude.withObject
      "ResourceIdentifier"
      ( \x ->
          ResourceIdentifier'
            Prelude.<$> (x Prelude..:? "roleAliasArn")
            Prelude.<*> (x Prelude..:? "clientId")
            Prelude.<*> (x Prelude..:? "iamRoleArn")
            Prelude.<*> (x Prelude..:? "cognitoIdentityPoolId")
            Prelude.<*> (x Prelude..:? "account")
            Prelude.<*> (x Prelude..:? "policyVersionIdentifier")
            Prelude.<*> (x Prelude..:? "deviceCertificateId")
            Prelude.<*> (x Prelude..:? "caCertificateId")
      )

instance Prelude.Hashable ResourceIdentifier

instance Prelude.NFData ResourceIdentifier

instance Prelude.ToJSON ResourceIdentifier where
  toJSON ResourceIdentifier' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("roleAliasArn" Prelude..=)
              Prelude.<$> roleAliasArn,
            ("clientId" Prelude..=) Prelude.<$> clientId,
            ("iamRoleArn" Prelude..=) Prelude.<$> iamRoleArn,
            ("cognitoIdentityPoolId" Prelude..=)
              Prelude.<$> cognitoIdentityPoolId,
            ("account" Prelude..=) Prelude.<$> account,
            ("policyVersionIdentifier" Prelude..=)
              Prelude.<$> policyVersionIdentifier,
            ("deviceCertificateId" Prelude..=)
              Prelude.<$> deviceCertificateId,
            ("caCertificateId" Prelude..=)
              Prelude.<$> caCertificateId
          ]
      )
