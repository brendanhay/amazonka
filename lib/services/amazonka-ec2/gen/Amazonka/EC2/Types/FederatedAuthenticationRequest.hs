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
-- Module      : Amazonka.EC2.Types.FederatedAuthenticationRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.FederatedAuthenticationRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | The IAM SAML identity provider used for federated authentication.
--
-- /See:/ 'newFederatedAuthenticationRequest' smart constructor.
data FederatedAuthenticationRequest = FederatedAuthenticationRequest'
  { -- | The Amazon Resource Name (ARN) of the IAM SAML identity provider.
    sAMLProviderArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM SAML identity provider for the
    -- self-service portal.
    selfServiceSAMLProviderArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FederatedAuthenticationRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sAMLProviderArn', 'federatedAuthenticationRequest_sAMLProviderArn' - The Amazon Resource Name (ARN) of the IAM SAML identity provider.
--
-- 'selfServiceSAMLProviderArn', 'federatedAuthenticationRequest_selfServiceSAMLProviderArn' - The Amazon Resource Name (ARN) of the IAM SAML identity provider for the
-- self-service portal.
newFederatedAuthenticationRequest ::
  FederatedAuthenticationRequest
newFederatedAuthenticationRequest =
  FederatedAuthenticationRequest'
    { sAMLProviderArn =
        Prelude.Nothing,
      selfServiceSAMLProviderArn =
        Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the IAM SAML identity provider.
federatedAuthenticationRequest_sAMLProviderArn :: Lens.Lens' FederatedAuthenticationRequest (Prelude.Maybe Prelude.Text)
federatedAuthenticationRequest_sAMLProviderArn = Lens.lens (\FederatedAuthenticationRequest' {sAMLProviderArn} -> sAMLProviderArn) (\s@FederatedAuthenticationRequest' {} a -> s {sAMLProviderArn = a} :: FederatedAuthenticationRequest)

-- | The Amazon Resource Name (ARN) of the IAM SAML identity provider for the
-- self-service portal.
federatedAuthenticationRequest_selfServiceSAMLProviderArn :: Lens.Lens' FederatedAuthenticationRequest (Prelude.Maybe Prelude.Text)
federatedAuthenticationRequest_selfServiceSAMLProviderArn = Lens.lens (\FederatedAuthenticationRequest' {selfServiceSAMLProviderArn} -> selfServiceSAMLProviderArn) (\s@FederatedAuthenticationRequest' {} a -> s {selfServiceSAMLProviderArn = a} :: FederatedAuthenticationRequest)

instance
  Prelude.Hashable
    FederatedAuthenticationRequest
  where
  hashWithSalt
    _salt
    FederatedAuthenticationRequest' {..} =
      _salt
        `Prelude.hashWithSalt` sAMLProviderArn
        `Prelude.hashWithSalt` selfServiceSAMLProviderArn

instance
  Prelude.NFData
    FederatedAuthenticationRequest
  where
  rnf FederatedAuthenticationRequest' {..} =
    Prelude.rnf sAMLProviderArn
      `Prelude.seq` Prelude.rnf selfServiceSAMLProviderArn

instance Data.ToQuery FederatedAuthenticationRequest where
  toQuery FederatedAuthenticationRequest' {..} =
    Prelude.mconcat
      [ "SAMLProviderArn" Data.=: sAMLProviderArn,
        "SelfServiceSAMLProviderArn"
          Data.=: selfServiceSAMLProviderArn
      ]
