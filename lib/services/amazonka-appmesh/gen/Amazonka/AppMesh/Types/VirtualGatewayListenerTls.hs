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
-- Module      : Amazonka.AppMesh.Types.VirtualGatewayListenerTls
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.VirtualGatewayListenerTls where

import Amazonka.AppMesh.Types.VirtualGatewayListenerTlsCertificate
import Amazonka.AppMesh.Types.VirtualGatewayListenerTlsMode
import Amazonka.AppMesh.Types.VirtualGatewayListenerTlsValidationContext
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the Transport Layer Security (TLS) properties
-- for a listener.
--
-- /See:/ 'newVirtualGatewayListenerTls' smart constructor.
data VirtualGatewayListenerTls = VirtualGatewayListenerTls'
  { -- | A reference to an object that represents a virtual gateway\'s
    -- listener\'s Transport Layer Security (TLS) validation context.
    validation :: Prelude.Maybe VirtualGatewayListenerTlsValidationContext,
    -- | An object that represents a Transport Layer Security (TLS) certificate.
    certificate :: VirtualGatewayListenerTlsCertificate,
    -- | Specify one of the following modes.
    --
    -- -   ____STRICT – Listener only accepts connections with TLS enabled.
    --
    -- -   ____PERMISSIVE – Listener accepts connections with or without TLS
    --     enabled.
    --
    -- -   ____DISABLED – Listener only accepts connections without TLS.
    mode :: VirtualGatewayListenerTlsMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VirtualGatewayListenerTls' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'validation', 'virtualGatewayListenerTls_validation' - A reference to an object that represents a virtual gateway\'s
-- listener\'s Transport Layer Security (TLS) validation context.
--
-- 'certificate', 'virtualGatewayListenerTls_certificate' - An object that represents a Transport Layer Security (TLS) certificate.
--
-- 'mode', 'virtualGatewayListenerTls_mode' - Specify one of the following modes.
--
-- -   ____STRICT – Listener only accepts connections with TLS enabled.
--
-- -   ____PERMISSIVE – Listener accepts connections with or without TLS
--     enabled.
--
-- -   ____DISABLED – Listener only accepts connections without TLS.
newVirtualGatewayListenerTls ::
  -- | 'certificate'
  VirtualGatewayListenerTlsCertificate ->
  -- | 'mode'
  VirtualGatewayListenerTlsMode ->
  VirtualGatewayListenerTls
newVirtualGatewayListenerTls pCertificate_ pMode_ =
  VirtualGatewayListenerTls'
    { validation =
        Prelude.Nothing,
      certificate = pCertificate_,
      mode = pMode_
    }

-- | A reference to an object that represents a virtual gateway\'s
-- listener\'s Transport Layer Security (TLS) validation context.
virtualGatewayListenerTls_validation :: Lens.Lens' VirtualGatewayListenerTls (Prelude.Maybe VirtualGatewayListenerTlsValidationContext)
virtualGatewayListenerTls_validation = Lens.lens (\VirtualGatewayListenerTls' {validation} -> validation) (\s@VirtualGatewayListenerTls' {} a -> s {validation = a} :: VirtualGatewayListenerTls)

-- | An object that represents a Transport Layer Security (TLS) certificate.
virtualGatewayListenerTls_certificate :: Lens.Lens' VirtualGatewayListenerTls VirtualGatewayListenerTlsCertificate
virtualGatewayListenerTls_certificate = Lens.lens (\VirtualGatewayListenerTls' {certificate} -> certificate) (\s@VirtualGatewayListenerTls' {} a -> s {certificate = a} :: VirtualGatewayListenerTls)

-- | Specify one of the following modes.
--
-- -   ____STRICT – Listener only accepts connections with TLS enabled.
--
-- -   ____PERMISSIVE – Listener accepts connections with or without TLS
--     enabled.
--
-- -   ____DISABLED – Listener only accepts connections without TLS.
virtualGatewayListenerTls_mode :: Lens.Lens' VirtualGatewayListenerTls VirtualGatewayListenerTlsMode
virtualGatewayListenerTls_mode = Lens.lens (\VirtualGatewayListenerTls' {mode} -> mode) (\s@VirtualGatewayListenerTls' {} a -> s {mode = a} :: VirtualGatewayListenerTls)

instance Data.FromJSON VirtualGatewayListenerTls where
  parseJSON =
    Data.withObject
      "VirtualGatewayListenerTls"
      ( \x ->
          VirtualGatewayListenerTls'
            Prelude.<$> (x Data..:? "validation")
            Prelude.<*> (x Data..: "certificate")
            Prelude.<*> (x Data..: "mode")
      )

instance Prelude.Hashable VirtualGatewayListenerTls where
  hashWithSalt _salt VirtualGatewayListenerTls' {..} =
    _salt `Prelude.hashWithSalt` validation
      `Prelude.hashWithSalt` certificate
      `Prelude.hashWithSalt` mode

instance Prelude.NFData VirtualGatewayListenerTls where
  rnf VirtualGatewayListenerTls' {..} =
    Prelude.rnf validation
      `Prelude.seq` Prelude.rnf certificate
      `Prelude.seq` Prelude.rnf mode

instance Data.ToJSON VirtualGatewayListenerTls where
  toJSON VirtualGatewayListenerTls' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("validation" Data..=) Prelude.<$> validation,
            Prelude.Just ("certificate" Data..= certificate),
            Prelude.Just ("mode" Data..= mode)
          ]
      )
