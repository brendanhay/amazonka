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
-- Module      : Amazonka.AppMesh.Types.ListenerTls
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.ListenerTls where

import Amazonka.AppMesh.Types.ListenerTlsCertificate
import Amazonka.AppMesh.Types.ListenerTlsMode
import Amazonka.AppMesh.Types.ListenerTlsValidationContext
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the Transport Layer Security (TLS) properties
-- for a listener.
--
-- /See:/ 'newListenerTls' smart constructor.
data ListenerTls = ListenerTls'
  { -- | A reference to an object that represents a listener\'s Transport Layer
    -- Security (TLS) validation context.
    validation :: Prelude.Maybe ListenerTlsValidationContext,
    -- | A reference to an object that represents a listener\'s Transport Layer
    -- Security (TLS) certificate.
    certificate :: ListenerTlsCertificate,
    -- | Specify one of the following modes.
    --
    -- -   ____STRICT – Listener only accepts connections with TLS enabled.
    --
    -- -   ____PERMISSIVE – Listener accepts connections with or without TLS
    --     enabled.
    --
    -- -   ____DISABLED – Listener only accepts connections without TLS.
    mode :: ListenerTlsMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListenerTls' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'validation', 'listenerTls_validation' - A reference to an object that represents a listener\'s Transport Layer
-- Security (TLS) validation context.
--
-- 'certificate', 'listenerTls_certificate' - A reference to an object that represents a listener\'s Transport Layer
-- Security (TLS) certificate.
--
-- 'mode', 'listenerTls_mode' - Specify one of the following modes.
--
-- -   ____STRICT – Listener only accepts connections with TLS enabled.
--
-- -   ____PERMISSIVE – Listener accepts connections with or without TLS
--     enabled.
--
-- -   ____DISABLED – Listener only accepts connections without TLS.
newListenerTls ::
  -- | 'certificate'
  ListenerTlsCertificate ->
  -- | 'mode'
  ListenerTlsMode ->
  ListenerTls
newListenerTls pCertificate_ pMode_ =
  ListenerTls'
    { validation = Prelude.Nothing,
      certificate = pCertificate_,
      mode = pMode_
    }

-- | A reference to an object that represents a listener\'s Transport Layer
-- Security (TLS) validation context.
listenerTls_validation :: Lens.Lens' ListenerTls (Prelude.Maybe ListenerTlsValidationContext)
listenerTls_validation = Lens.lens (\ListenerTls' {validation} -> validation) (\s@ListenerTls' {} a -> s {validation = a} :: ListenerTls)

-- | A reference to an object that represents a listener\'s Transport Layer
-- Security (TLS) certificate.
listenerTls_certificate :: Lens.Lens' ListenerTls ListenerTlsCertificate
listenerTls_certificate = Lens.lens (\ListenerTls' {certificate} -> certificate) (\s@ListenerTls' {} a -> s {certificate = a} :: ListenerTls)

-- | Specify one of the following modes.
--
-- -   ____STRICT – Listener only accepts connections with TLS enabled.
--
-- -   ____PERMISSIVE – Listener accepts connections with or without TLS
--     enabled.
--
-- -   ____DISABLED – Listener only accepts connections without TLS.
listenerTls_mode :: Lens.Lens' ListenerTls ListenerTlsMode
listenerTls_mode = Lens.lens (\ListenerTls' {mode} -> mode) (\s@ListenerTls' {} a -> s {mode = a} :: ListenerTls)

instance Core.FromJSON ListenerTls where
  parseJSON =
    Core.withObject
      "ListenerTls"
      ( \x ->
          ListenerTls'
            Prelude.<$> (x Core..:? "validation")
            Prelude.<*> (x Core..: "certificate")
            Prelude.<*> (x Core..: "mode")
      )

instance Prelude.Hashable ListenerTls where
  hashWithSalt _salt ListenerTls' {..} =
    _salt `Prelude.hashWithSalt` validation
      `Prelude.hashWithSalt` certificate
      `Prelude.hashWithSalt` mode

instance Prelude.NFData ListenerTls where
  rnf ListenerTls' {..} =
    Prelude.rnf validation
      `Prelude.seq` Prelude.rnf certificate
      `Prelude.seq` Prelude.rnf mode

instance Core.ToJSON ListenerTls where
  toJSON ListenerTls' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("validation" Core..=) Prelude.<$> validation,
            Prelude.Just ("certificate" Core..= certificate),
            Prelude.Just ("mode" Core..= mode)
          ]
      )
