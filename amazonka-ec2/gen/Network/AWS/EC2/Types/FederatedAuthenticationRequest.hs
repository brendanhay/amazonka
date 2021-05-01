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
-- Module      : Network.AWS.EC2.Types.FederatedAuthenticationRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FederatedAuthenticationRequest where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The IAM SAML identity provider used for federated authentication.
--
-- /See:/ 'newFederatedAuthenticationRequest' smart constructor.
data FederatedAuthenticationRequest = FederatedAuthenticationRequest'
  { -- | The Amazon Resource Name (ARN) of the IAM SAML identity provider for the
    -- self-service portal.
    selfServiceSAMLProviderArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM SAML identity provider.
    sAMLProviderArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FederatedAuthenticationRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'selfServiceSAMLProviderArn', 'federatedAuthenticationRequest_selfServiceSAMLProviderArn' - The Amazon Resource Name (ARN) of the IAM SAML identity provider for the
-- self-service portal.
--
-- 'sAMLProviderArn', 'federatedAuthenticationRequest_sAMLProviderArn' - The Amazon Resource Name (ARN) of the IAM SAML identity provider.
newFederatedAuthenticationRequest ::
  FederatedAuthenticationRequest
newFederatedAuthenticationRequest =
  FederatedAuthenticationRequest'
    { selfServiceSAMLProviderArn =
        Prelude.Nothing,
      sAMLProviderArn = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the IAM SAML identity provider for the
-- self-service portal.
federatedAuthenticationRequest_selfServiceSAMLProviderArn :: Lens.Lens' FederatedAuthenticationRequest (Prelude.Maybe Prelude.Text)
federatedAuthenticationRequest_selfServiceSAMLProviderArn = Lens.lens (\FederatedAuthenticationRequest' {selfServiceSAMLProviderArn} -> selfServiceSAMLProviderArn) (\s@FederatedAuthenticationRequest' {} a -> s {selfServiceSAMLProviderArn = a} :: FederatedAuthenticationRequest)

-- | The Amazon Resource Name (ARN) of the IAM SAML identity provider.
federatedAuthenticationRequest_sAMLProviderArn :: Lens.Lens' FederatedAuthenticationRequest (Prelude.Maybe Prelude.Text)
federatedAuthenticationRequest_sAMLProviderArn = Lens.lens (\FederatedAuthenticationRequest' {sAMLProviderArn} -> sAMLProviderArn) (\s@FederatedAuthenticationRequest' {} a -> s {sAMLProviderArn = a} :: FederatedAuthenticationRequest)

instance
  Prelude.Hashable
    FederatedAuthenticationRequest

instance
  Prelude.NFData
    FederatedAuthenticationRequest

instance
  Prelude.ToQuery
    FederatedAuthenticationRequest
  where
  toQuery FederatedAuthenticationRequest' {..} =
    Prelude.mconcat
      [ "SelfServiceSAMLProviderArn"
          Prelude.=: selfServiceSAMLProviderArn,
        "SAMLProviderArn" Prelude.=: sAMLProviderArn
      ]
