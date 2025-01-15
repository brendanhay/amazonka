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
-- Module      : Amazonka.AppMesh.Types.ClientPolicyTls
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.ClientPolicyTls where

import Amazonka.AppMesh.Types.ClientTlsCertificate
import Amazonka.AppMesh.Types.TlsValidationContext
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A reference to an object that represents a Transport Layer Security
-- (TLS) client policy.
--
-- /See:/ 'newClientPolicyTls' smart constructor.
data ClientPolicyTls = ClientPolicyTls'
  { -- | A reference to an object that represents a client\'s TLS certificate.
    certificate :: Prelude.Maybe ClientTlsCertificate,
    -- | Whether the policy is enforced. The default is @True@, if a value isn\'t
    -- specified.
    enforce :: Prelude.Maybe Prelude.Bool,
    -- | One or more ports that the policy is enforced for.
    ports :: Prelude.Maybe [Prelude.Natural],
    -- | A reference to an object that represents a TLS validation context.
    validation :: TlsValidationContext
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClientPolicyTls' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificate', 'clientPolicyTls_certificate' - A reference to an object that represents a client\'s TLS certificate.
--
-- 'enforce', 'clientPolicyTls_enforce' - Whether the policy is enforced. The default is @True@, if a value isn\'t
-- specified.
--
-- 'ports', 'clientPolicyTls_ports' - One or more ports that the policy is enforced for.
--
-- 'validation', 'clientPolicyTls_validation' - A reference to an object that represents a TLS validation context.
newClientPolicyTls ::
  -- | 'validation'
  TlsValidationContext ->
  ClientPolicyTls
newClientPolicyTls pValidation_ =
  ClientPolicyTls'
    { certificate = Prelude.Nothing,
      enforce = Prelude.Nothing,
      ports = Prelude.Nothing,
      validation = pValidation_
    }

-- | A reference to an object that represents a client\'s TLS certificate.
clientPolicyTls_certificate :: Lens.Lens' ClientPolicyTls (Prelude.Maybe ClientTlsCertificate)
clientPolicyTls_certificate = Lens.lens (\ClientPolicyTls' {certificate} -> certificate) (\s@ClientPolicyTls' {} a -> s {certificate = a} :: ClientPolicyTls)

-- | Whether the policy is enforced. The default is @True@, if a value isn\'t
-- specified.
clientPolicyTls_enforce :: Lens.Lens' ClientPolicyTls (Prelude.Maybe Prelude.Bool)
clientPolicyTls_enforce = Lens.lens (\ClientPolicyTls' {enforce} -> enforce) (\s@ClientPolicyTls' {} a -> s {enforce = a} :: ClientPolicyTls)

-- | One or more ports that the policy is enforced for.
clientPolicyTls_ports :: Lens.Lens' ClientPolicyTls (Prelude.Maybe [Prelude.Natural])
clientPolicyTls_ports = Lens.lens (\ClientPolicyTls' {ports} -> ports) (\s@ClientPolicyTls' {} a -> s {ports = a} :: ClientPolicyTls) Prelude.. Lens.mapping Lens.coerced

-- | A reference to an object that represents a TLS validation context.
clientPolicyTls_validation :: Lens.Lens' ClientPolicyTls TlsValidationContext
clientPolicyTls_validation = Lens.lens (\ClientPolicyTls' {validation} -> validation) (\s@ClientPolicyTls' {} a -> s {validation = a} :: ClientPolicyTls)

instance Data.FromJSON ClientPolicyTls where
  parseJSON =
    Data.withObject
      "ClientPolicyTls"
      ( \x ->
          ClientPolicyTls'
            Prelude.<$> (x Data..:? "certificate")
            Prelude.<*> (x Data..:? "enforce")
            Prelude.<*> (x Data..:? "ports" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "validation")
      )

instance Prelude.Hashable ClientPolicyTls where
  hashWithSalt _salt ClientPolicyTls' {..} =
    _salt
      `Prelude.hashWithSalt` certificate
      `Prelude.hashWithSalt` enforce
      `Prelude.hashWithSalt` ports
      `Prelude.hashWithSalt` validation

instance Prelude.NFData ClientPolicyTls where
  rnf ClientPolicyTls' {..} =
    Prelude.rnf certificate `Prelude.seq`
      Prelude.rnf enforce `Prelude.seq`
        Prelude.rnf ports `Prelude.seq`
          Prelude.rnf validation

instance Data.ToJSON ClientPolicyTls where
  toJSON ClientPolicyTls' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("certificate" Data..=) Prelude.<$> certificate,
            ("enforce" Data..=) Prelude.<$> enforce,
            ("ports" Data..=) Prelude.<$> ports,
            Prelude.Just ("validation" Data..= validation)
          ]
      )
