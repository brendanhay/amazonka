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
-- Module      : Amazonka.AppMesh.Types.VirtualGatewayClientPolicyTls
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.VirtualGatewayClientPolicyTls where

import Amazonka.AppMesh.Types.VirtualGatewayClientTlsCertificate
import Amazonka.AppMesh.Types.VirtualGatewayTlsValidationContext
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a Transport Layer Security (TLS) client
-- policy.
--
-- /See:/ 'newVirtualGatewayClientPolicyTls' smart constructor.
data VirtualGatewayClientPolicyTls = VirtualGatewayClientPolicyTls'
  { -- | A reference to an object that represents a virtual gateway\'s client\'s
    -- Transport Layer Security (TLS) certificate.
    certificate :: Prelude.Maybe VirtualGatewayClientTlsCertificate,
    -- | Whether the policy is enforced. The default is @True@, if a value isn\'t
    -- specified.
    enforce :: Prelude.Maybe Prelude.Bool,
    -- | One or more ports that the policy is enforced for.
    ports :: Prelude.Maybe [Prelude.Natural],
    -- | A reference to an object that represents a Transport Layer Security
    -- (TLS) validation context.
    validation :: VirtualGatewayTlsValidationContext
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VirtualGatewayClientPolicyTls' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificate', 'virtualGatewayClientPolicyTls_certificate' - A reference to an object that represents a virtual gateway\'s client\'s
-- Transport Layer Security (TLS) certificate.
--
-- 'enforce', 'virtualGatewayClientPolicyTls_enforce' - Whether the policy is enforced. The default is @True@, if a value isn\'t
-- specified.
--
-- 'ports', 'virtualGatewayClientPolicyTls_ports' - One or more ports that the policy is enforced for.
--
-- 'validation', 'virtualGatewayClientPolicyTls_validation' - A reference to an object that represents a Transport Layer Security
-- (TLS) validation context.
newVirtualGatewayClientPolicyTls ::
  -- | 'validation'
  VirtualGatewayTlsValidationContext ->
  VirtualGatewayClientPolicyTls
newVirtualGatewayClientPolicyTls pValidation_ =
  VirtualGatewayClientPolicyTls'
    { certificate =
        Prelude.Nothing,
      enforce = Prelude.Nothing,
      ports = Prelude.Nothing,
      validation = pValidation_
    }

-- | A reference to an object that represents a virtual gateway\'s client\'s
-- Transport Layer Security (TLS) certificate.
virtualGatewayClientPolicyTls_certificate :: Lens.Lens' VirtualGatewayClientPolicyTls (Prelude.Maybe VirtualGatewayClientTlsCertificate)
virtualGatewayClientPolicyTls_certificate = Lens.lens (\VirtualGatewayClientPolicyTls' {certificate} -> certificate) (\s@VirtualGatewayClientPolicyTls' {} a -> s {certificate = a} :: VirtualGatewayClientPolicyTls)

-- | Whether the policy is enforced. The default is @True@, if a value isn\'t
-- specified.
virtualGatewayClientPolicyTls_enforce :: Lens.Lens' VirtualGatewayClientPolicyTls (Prelude.Maybe Prelude.Bool)
virtualGatewayClientPolicyTls_enforce = Lens.lens (\VirtualGatewayClientPolicyTls' {enforce} -> enforce) (\s@VirtualGatewayClientPolicyTls' {} a -> s {enforce = a} :: VirtualGatewayClientPolicyTls)

-- | One or more ports that the policy is enforced for.
virtualGatewayClientPolicyTls_ports :: Lens.Lens' VirtualGatewayClientPolicyTls (Prelude.Maybe [Prelude.Natural])
virtualGatewayClientPolicyTls_ports = Lens.lens (\VirtualGatewayClientPolicyTls' {ports} -> ports) (\s@VirtualGatewayClientPolicyTls' {} a -> s {ports = a} :: VirtualGatewayClientPolicyTls) Prelude.. Lens.mapping Lens.coerced

-- | A reference to an object that represents a Transport Layer Security
-- (TLS) validation context.
virtualGatewayClientPolicyTls_validation :: Lens.Lens' VirtualGatewayClientPolicyTls VirtualGatewayTlsValidationContext
virtualGatewayClientPolicyTls_validation = Lens.lens (\VirtualGatewayClientPolicyTls' {validation} -> validation) (\s@VirtualGatewayClientPolicyTls' {} a -> s {validation = a} :: VirtualGatewayClientPolicyTls)

instance Data.FromJSON VirtualGatewayClientPolicyTls where
  parseJSON =
    Data.withObject
      "VirtualGatewayClientPolicyTls"
      ( \x ->
          VirtualGatewayClientPolicyTls'
            Prelude.<$> (x Data..:? "certificate")
            Prelude.<*> (x Data..:? "enforce")
            Prelude.<*> (x Data..:? "ports" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "validation")
      )

instance
  Prelude.Hashable
    VirtualGatewayClientPolicyTls
  where
  hashWithSalt _salt VirtualGatewayClientPolicyTls' {..} =
    _salt
      `Prelude.hashWithSalt` certificate
      `Prelude.hashWithSalt` enforce
      `Prelude.hashWithSalt` ports
      `Prelude.hashWithSalt` validation

instance Prelude.NFData VirtualGatewayClientPolicyTls where
  rnf VirtualGatewayClientPolicyTls' {..} =
    Prelude.rnf certificate `Prelude.seq`
      Prelude.rnf enforce `Prelude.seq`
        Prelude.rnf ports `Prelude.seq`
          Prelude.rnf validation

instance Data.ToJSON VirtualGatewayClientPolicyTls where
  toJSON VirtualGatewayClientPolicyTls' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("certificate" Data..=) Prelude.<$> certificate,
            ("enforce" Data..=) Prelude.<$> enforce,
            ("ports" Data..=) Prelude.<$> ports,
            Prelude.Just ("validation" Data..= validation)
          ]
      )
