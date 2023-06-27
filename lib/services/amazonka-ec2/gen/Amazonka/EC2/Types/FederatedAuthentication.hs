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
-- Module      : Amazonka.EC2.Types.FederatedAuthentication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.FederatedAuthentication where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes the IAM SAML identity providers used for federated
-- authentication.
--
-- /See:/ 'newFederatedAuthentication' smart constructor.
data FederatedAuthentication = FederatedAuthentication'
  { -- | The Amazon Resource Name (ARN) of the IAM SAML identity provider.
    samlProviderArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM SAML identity provider for the
    -- self-service portal.
    selfServiceSamlProviderArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FederatedAuthentication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'samlProviderArn', 'federatedAuthentication_samlProviderArn' - The Amazon Resource Name (ARN) of the IAM SAML identity provider.
--
-- 'selfServiceSamlProviderArn', 'federatedAuthentication_selfServiceSamlProviderArn' - The Amazon Resource Name (ARN) of the IAM SAML identity provider for the
-- self-service portal.
newFederatedAuthentication ::
  FederatedAuthentication
newFederatedAuthentication =
  FederatedAuthentication'
    { samlProviderArn =
        Prelude.Nothing,
      selfServiceSamlProviderArn = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the IAM SAML identity provider.
federatedAuthentication_samlProviderArn :: Lens.Lens' FederatedAuthentication (Prelude.Maybe Prelude.Text)
federatedAuthentication_samlProviderArn = Lens.lens (\FederatedAuthentication' {samlProviderArn} -> samlProviderArn) (\s@FederatedAuthentication' {} a -> s {samlProviderArn = a} :: FederatedAuthentication)

-- | The Amazon Resource Name (ARN) of the IAM SAML identity provider for the
-- self-service portal.
federatedAuthentication_selfServiceSamlProviderArn :: Lens.Lens' FederatedAuthentication (Prelude.Maybe Prelude.Text)
federatedAuthentication_selfServiceSamlProviderArn = Lens.lens (\FederatedAuthentication' {selfServiceSamlProviderArn} -> selfServiceSamlProviderArn) (\s@FederatedAuthentication' {} a -> s {selfServiceSamlProviderArn = a} :: FederatedAuthentication)

instance Data.FromXML FederatedAuthentication where
  parseXML x =
    FederatedAuthentication'
      Prelude.<$> (x Data..@? "samlProviderArn")
      Prelude.<*> (x Data..@? "selfServiceSamlProviderArn")

instance Prelude.Hashable FederatedAuthentication where
  hashWithSalt _salt FederatedAuthentication' {..} =
    _salt
      `Prelude.hashWithSalt` samlProviderArn
      `Prelude.hashWithSalt` selfServiceSamlProviderArn

instance Prelude.NFData FederatedAuthentication where
  rnf FederatedAuthentication' {..} =
    Prelude.rnf samlProviderArn
      `Prelude.seq` Prelude.rnf selfServiceSamlProviderArn
