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
-- Module      : Amazonka.AppMesh.Types.VirtualGatewayTlsValidationContextTrust
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.VirtualGatewayTlsValidationContextTrust where

import Amazonka.AppMesh.Types.VirtualGatewayTlsValidationContextAcmTrust
import Amazonka.AppMesh.Types.VirtualGatewayTlsValidationContextFileTrust
import Amazonka.AppMesh.Types.VirtualGatewayTlsValidationContextSdsTrust
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a Transport Layer Security (TLS) validation
-- context trust.
--
-- /See:/ 'newVirtualGatewayTlsValidationContextTrust' smart constructor.
data VirtualGatewayTlsValidationContextTrust = VirtualGatewayTlsValidationContextTrust'
  { -- | A reference to an object that represents a virtual gateway\'s Transport
    -- Layer Security (TLS) Secret Discovery Service validation context trust.
    sds :: Prelude.Maybe VirtualGatewayTlsValidationContextSdsTrust,
    -- | An object that represents a Transport Layer Security (TLS) validation
    -- context trust for a local file.
    file :: Prelude.Maybe VirtualGatewayTlsValidationContextFileTrust,
    -- | A reference to an object that represents a Transport Layer Security
    -- (TLS) validation context trust for an Certificate Manager certificate.
    acm :: Prelude.Maybe VirtualGatewayTlsValidationContextAcmTrust
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VirtualGatewayTlsValidationContextTrust' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sds', 'virtualGatewayTlsValidationContextTrust_sds' - A reference to an object that represents a virtual gateway\'s Transport
-- Layer Security (TLS) Secret Discovery Service validation context trust.
--
-- 'file', 'virtualGatewayTlsValidationContextTrust_file' - An object that represents a Transport Layer Security (TLS) validation
-- context trust for a local file.
--
-- 'acm', 'virtualGatewayTlsValidationContextTrust_acm' - A reference to an object that represents a Transport Layer Security
-- (TLS) validation context trust for an Certificate Manager certificate.
newVirtualGatewayTlsValidationContextTrust ::
  VirtualGatewayTlsValidationContextTrust
newVirtualGatewayTlsValidationContextTrust =
  VirtualGatewayTlsValidationContextTrust'
    { sds =
        Prelude.Nothing,
      file = Prelude.Nothing,
      acm = Prelude.Nothing
    }

-- | A reference to an object that represents a virtual gateway\'s Transport
-- Layer Security (TLS) Secret Discovery Service validation context trust.
virtualGatewayTlsValidationContextTrust_sds :: Lens.Lens' VirtualGatewayTlsValidationContextTrust (Prelude.Maybe VirtualGatewayTlsValidationContextSdsTrust)
virtualGatewayTlsValidationContextTrust_sds = Lens.lens (\VirtualGatewayTlsValidationContextTrust' {sds} -> sds) (\s@VirtualGatewayTlsValidationContextTrust' {} a -> s {sds = a} :: VirtualGatewayTlsValidationContextTrust)

-- | An object that represents a Transport Layer Security (TLS) validation
-- context trust for a local file.
virtualGatewayTlsValidationContextTrust_file :: Lens.Lens' VirtualGatewayTlsValidationContextTrust (Prelude.Maybe VirtualGatewayTlsValidationContextFileTrust)
virtualGatewayTlsValidationContextTrust_file = Lens.lens (\VirtualGatewayTlsValidationContextTrust' {file} -> file) (\s@VirtualGatewayTlsValidationContextTrust' {} a -> s {file = a} :: VirtualGatewayTlsValidationContextTrust)

-- | A reference to an object that represents a Transport Layer Security
-- (TLS) validation context trust for an Certificate Manager certificate.
virtualGatewayTlsValidationContextTrust_acm :: Lens.Lens' VirtualGatewayTlsValidationContextTrust (Prelude.Maybe VirtualGatewayTlsValidationContextAcmTrust)
virtualGatewayTlsValidationContextTrust_acm = Lens.lens (\VirtualGatewayTlsValidationContextTrust' {acm} -> acm) (\s@VirtualGatewayTlsValidationContextTrust' {} a -> s {acm = a} :: VirtualGatewayTlsValidationContextTrust)

instance
  Core.FromJSON
    VirtualGatewayTlsValidationContextTrust
  where
  parseJSON =
    Core.withObject
      "VirtualGatewayTlsValidationContextTrust"
      ( \x ->
          VirtualGatewayTlsValidationContextTrust'
            Prelude.<$> (x Core..:? "sds")
            Prelude.<*> (x Core..:? "file")
            Prelude.<*> (x Core..:? "acm")
      )

instance
  Prelude.Hashable
    VirtualGatewayTlsValidationContextTrust
  where
  hashWithSalt
    _salt
    VirtualGatewayTlsValidationContextTrust' {..} =
      _salt `Prelude.hashWithSalt` sds
        `Prelude.hashWithSalt` file
        `Prelude.hashWithSalt` acm

instance
  Prelude.NFData
    VirtualGatewayTlsValidationContextTrust
  where
  rnf VirtualGatewayTlsValidationContextTrust' {..} =
    Prelude.rnf sds
      `Prelude.seq` Prelude.rnf file
      `Prelude.seq` Prelude.rnf acm

instance
  Core.ToJSON
    VirtualGatewayTlsValidationContextTrust
  where
  toJSON VirtualGatewayTlsValidationContextTrust' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("sds" Core..=) Prelude.<$> sds,
            ("file" Core..=) Prelude.<$> file,
            ("acm" Core..=) Prelude.<$> acm
          ]
      )
