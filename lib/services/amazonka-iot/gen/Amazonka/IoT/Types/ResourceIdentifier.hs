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
-- Module      : Amazonka.IoT.Types.ResourceIdentifier
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.ResourceIdentifier where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.IssuerCertificateIdentifier
import Amazonka.IoT.Types.PolicyVersionIdentifier
import qualified Amazonka.Prelude as Prelude

-- | Information that identifies the noncompliant resource.
--
-- /See:/ 'newResourceIdentifier' smart constructor.
data ResourceIdentifier = ResourceIdentifier'
  { -- | The account with which the resource is associated.
    account :: Prelude.Maybe Prelude.Text,
    -- | The ID of the CA certificate used to authorize the certificate.
    caCertificateId :: Prelude.Maybe Prelude.Text,
    -- | The client ID.
    clientId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Cognito identity pool.
    cognitoIdentityPoolId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the identified device certificate.
    deviceCertificateArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the certificate attached to the resource.
    deviceCertificateId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the IAM role that has overly permissive actions.
    iamRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The issuer certificate identifier.
    issuerCertificateIdentifier :: Prelude.Maybe IssuerCertificateIdentifier,
    -- | The version of the policy associated with the resource.
    policyVersionIdentifier :: Prelude.Maybe PolicyVersionIdentifier,
    -- | The ARN of the role alias that has overly permissive actions.
    roleAliasArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'account', 'resourceIdentifier_account' - The account with which the resource is associated.
--
-- 'caCertificateId', 'resourceIdentifier_caCertificateId' - The ID of the CA certificate used to authorize the certificate.
--
-- 'clientId', 'resourceIdentifier_clientId' - The client ID.
--
-- 'cognitoIdentityPoolId', 'resourceIdentifier_cognitoIdentityPoolId' - The ID of the Amazon Cognito identity pool.
--
-- 'deviceCertificateArn', 'resourceIdentifier_deviceCertificateArn' - The ARN of the identified device certificate.
--
-- 'deviceCertificateId', 'resourceIdentifier_deviceCertificateId' - The ID of the certificate attached to the resource.
--
-- 'iamRoleArn', 'resourceIdentifier_iamRoleArn' - The ARN of the IAM role that has overly permissive actions.
--
-- 'issuerCertificateIdentifier', 'resourceIdentifier_issuerCertificateIdentifier' - The issuer certificate identifier.
--
-- 'policyVersionIdentifier', 'resourceIdentifier_policyVersionIdentifier' - The version of the policy associated with the resource.
--
-- 'roleAliasArn', 'resourceIdentifier_roleAliasArn' - The ARN of the role alias that has overly permissive actions.
newResourceIdentifier ::
  ResourceIdentifier
newResourceIdentifier =
  ResourceIdentifier'
    { account = Prelude.Nothing,
      caCertificateId = Prelude.Nothing,
      clientId = Prelude.Nothing,
      cognitoIdentityPoolId = Prelude.Nothing,
      deviceCertificateArn = Prelude.Nothing,
      deviceCertificateId = Prelude.Nothing,
      iamRoleArn = Prelude.Nothing,
      issuerCertificateIdentifier = Prelude.Nothing,
      policyVersionIdentifier = Prelude.Nothing,
      roleAliasArn = Prelude.Nothing
    }

-- | The account with which the resource is associated.
resourceIdentifier_account :: Lens.Lens' ResourceIdentifier (Prelude.Maybe Prelude.Text)
resourceIdentifier_account = Lens.lens (\ResourceIdentifier' {account} -> account) (\s@ResourceIdentifier' {} a -> s {account = a} :: ResourceIdentifier)

-- | The ID of the CA certificate used to authorize the certificate.
resourceIdentifier_caCertificateId :: Lens.Lens' ResourceIdentifier (Prelude.Maybe Prelude.Text)
resourceIdentifier_caCertificateId = Lens.lens (\ResourceIdentifier' {caCertificateId} -> caCertificateId) (\s@ResourceIdentifier' {} a -> s {caCertificateId = a} :: ResourceIdentifier)

-- | The client ID.
resourceIdentifier_clientId :: Lens.Lens' ResourceIdentifier (Prelude.Maybe Prelude.Text)
resourceIdentifier_clientId = Lens.lens (\ResourceIdentifier' {clientId} -> clientId) (\s@ResourceIdentifier' {} a -> s {clientId = a} :: ResourceIdentifier)

-- | The ID of the Amazon Cognito identity pool.
resourceIdentifier_cognitoIdentityPoolId :: Lens.Lens' ResourceIdentifier (Prelude.Maybe Prelude.Text)
resourceIdentifier_cognitoIdentityPoolId = Lens.lens (\ResourceIdentifier' {cognitoIdentityPoolId} -> cognitoIdentityPoolId) (\s@ResourceIdentifier' {} a -> s {cognitoIdentityPoolId = a} :: ResourceIdentifier)

-- | The ARN of the identified device certificate.
resourceIdentifier_deviceCertificateArn :: Lens.Lens' ResourceIdentifier (Prelude.Maybe Prelude.Text)
resourceIdentifier_deviceCertificateArn = Lens.lens (\ResourceIdentifier' {deviceCertificateArn} -> deviceCertificateArn) (\s@ResourceIdentifier' {} a -> s {deviceCertificateArn = a} :: ResourceIdentifier)

-- | The ID of the certificate attached to the resource.
resourceIdentifier_deviceCertificateId :: Lens.Lens' ResourceIdentifier (Prelude.Maybe Prelude.Text)
resourceIdentifier_deviceCertificateId = Lens.lens (\ResourceIdentifier' {deviceCertificateId} -> deviceCertificateId) (\s@ResourceIdentifier' {} a -> s {deviceCertificateId = a} :: ResourceIdentifier)

-- | The ARN of the IAM role that has overly permissive actions.
resourceIdentifier_iamRoleArn :: Lens.Lens' ResourceIdentifier (Prelude.Maybe Prelude.Text)
resourceIdentifier_iamRoleArn = Lens.lens (\ResourceIdentifier' {iamRoleArn} -> iamRoleArn) (\s@ResourceIdentifier' {} a -> s {iamRoleArn = a} :: ResourceIdentifier)

-- | The issuer certificate identifier.
resourceIdentifier_issuerCertificateIdentifier :: Lens.Lens' ResourceIdentifier (Prelude.Maybe IssuerCertificateIdentifier)
resourceIdentifier_issuerCertificateIdentifier = Lens.lens (\ResourceIdentifier' {issuerCertificateIdentifier} -> issuerCertificateIdentifier) (\s@ResourceIdentifier' {} a -> s {issuerCertificateIdentifier = a} :: ResourceIdentifier)

-- | The version of the policy associated with the resource.
resourceIdentifier_policyVersionIdentifier :: Lens.Lens' ResourceIdentifier (Prelude.Maybe PolicyVersionIdentifier)
resourceIdentifier_policyVersionIdentifier = Lens.lens (\ResourceIdentifier' {policyVersionIdentifier} -> policyVersionIdentifier) (\s@ResourceIdentifier' {} a -> s {policyVersionIdentifier = a} :: ResourceIdentifier)

-- | The ARN of the role alias that has overly permissive actions.
resourceIdentifier_roleAliasArn :: Lens.Lens' ResourceIdentifier (Prelude.Maybe Prelude.Text)
resourceIdentifier_roleAliasArn = Lens.lens (\ResourceIdentifier' {roleAliasArn} -> roleAliasArn) (\s@ResourceIdentifier' {} a -> s {roleAliasArn = a} :: ResourceIdentifier)

instance Data.FromJSON ResourceIdentifier where
  parseJSON =
    Data.withObject
      "ResourceIdentifier"
      ( \x ->
          ResourceIdentifier'
            Prelude.<$> (x Data..:? "account")
            Prelude.<*> (x Data..:? "caCertificateId")
            Prelude.<*> (x Data..:? "clientId")
            Prelude.<*> (x Data..:? "cognitoIdentityPoolId")
            Prelude.<*> (x Data..:? "deviceCertificateArn")
            Prelude.<*> (x Data..:? "deviceCertificateId")
            Prelude.<*> (x Data..:? "iamRoleArn")
            Prelude.<*> (x Data..:? "issuerCertificateIdentifier")
            Prelude.<*> (x Data..:? "policyVersionIdentifier")
            Prelude.<*> (x Data..:? "roleAliasArn")
      )

instance Prelude.Hashable ResourceIdentifier where
  hashWithSalt _salt ResourceIdentifier' {..} =
    _salt
      `Prelude.hashWithSalt` account
      `Prelude.hashWithSalt` caCertificateId
      `Prelude.hashWithSalt` clientId
      `Prelude.hashWithSalt` cognitoIdentityPoolId
      `Prelude.hashWithSalt` deviceCertificateArn
      `Prelude.hashWithSalt` deviceCertificateId
      `Prelude.hashWithSalt` iamRoleArn
      `Prelude.hashWithSalt` issuerCertificateIdentifier
      `Prelude.hashWithSalt` policyVersionIdentifier
      `Prelude.hashWithSalt` roleAliasArn

instance Prelude.NFData ResourceIdentifier where
  rnf ResourceIdentifier' {..} =
    Prelude.rnf account
      `Prelude.seq` Prelude.rnf caCertificateId
      `Prelude.seq` Prelude.rnf clientId
      `Prelude.seq` Prelude.rnf cognitoIdentityPoolId
      `Prelude.seq` Prelude.rnf deviceCertificateArn
      `Prelude.seq` Prelude.rnf deviceCertificateId
      `Prelude.seq` Prelude.rnf iamRoleArn
      `Prelude.seq` Prelude.rnf issuerCertificateIdentifier
      `Prelude.seq` Prelude.rnf policyVersionIdentifier
      `Prelude.seq` Prelude.rnf roleAliasArn

instance Data.ToJSON ResourceIdentifier where
  toJSON ResourceIdentifier' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("account" Data..=) Prelude.<$> account,
            ("caCertificateId" Data..=)
              Prelude.<$> caCertificateId,
            ("clientId" Data..=) Prelude.<$> clientId,
            ("cognitoIdentityPoolId" Data..=)
              Prelude.<$> cognitoIdentityPoolId,
            ("deviceCertificateArn" Data..=)
              Prelude.<$> deviceCertificateArn,
            ("deviceCertificateId" Data..=)
              Prelude.<$> deviceCertificateId,
            ("iamRoleArn" Data..=) Prelude.<$> iamRoleArn,
            ("issuerCertificateIdentifier" Data..=)
              Prelude.<$> issuerCertificateIdentifier,
            ("policyVersionIdentifier" Data..=)
              Prelude.<$> policyVersionIdentifier,
            ("roleAliasArn" Data..=) Prelude.<$> roleAliasArn
          ]
      )
