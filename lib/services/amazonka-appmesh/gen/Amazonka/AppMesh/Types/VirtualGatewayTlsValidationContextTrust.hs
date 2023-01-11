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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a Transport Layer Security (TLS) validation
-- context trust.
--
-- /See:/ 'newVirtualGatewayTlsValidationContextTrust' smart constructor.
data VirtualGatewayTlsValidationContextTrust = VirtualGatewayTlsValidationContextTrust'
  { -- | A reference to an object that represents a Transport Layer Security
    -- (TLS) validation context trust for an Certificate Manager certificate.
    acm :: Prelude.Maybe VirtualGatewayTlsValidationContextAcmTrust,
    -- | An object that represents a Transport Layer Security (TLS) validation
    -- context trust for a local file.
    file :: Prelude.Maybe VirtualGatewayTlsValidationContextFileTrust,
    -- | A reference to an object that represents a virtual gateway\'s Transport
    -- Layer Security (TLS) Secret Discovery Service validation context trust.
    sds :: Prelude.Maybe VirtualGatewayTlsValidationContextSdsTrust
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
-- 'acm', 'virtualGatewayTlsValidationContextTrust_acm' - A reference to an object that represents a Transport Layer Security
-- (TLS) validation context trust for an Certificate Manager certificate.
--
-- 'file', 'virtualGatewayTlsValidationContextTrust_file' - An object that represents a Transport Layer Security (TLS) validation
-- context trust for a local file.
--
-- 'sds', 'virtualGatewayTlsValidationContextTrust_sds' - A reference to an object that represents a virtual gateway\'s Transport
-- Layer Security (TLS) Secret Discovery Service validation context trust.
newVirtualGatewayTlsValidationContextTrust ::
  VirtualGatewayTlsValidationContextTrust
newVirtualGatewayTlsValidationContextTrust =
  VirtualGatewayTlsValidationContextTrust'
    { acm =
        Prelude.Nothing,
      file = Prelude.Nothing,
      sds = Prelude.Nothing
    }

-- | A reference to an object that represents a Transport Layer Security
-- (TLS) validation context trust for an Certificate Manager certificate.
virtualGatewayTlsValidationContextTrust_acm :: Lens.Lens' VirtualGatewayTlsValidationContextTrust (Prelude.Maybe VirtualGatewayTlsValidationContextAcmTrust)
virtualGatewayTlsValidationContextTrust_acm = Lens.lens (\VirtualGatewayTlsValidationContextTrust' {acm} -> acm) (\s@VirtualGatewayTlsValidationContextTrust' {} a -> s {acm = a} :: VirtualGatewayTlsValidationContextTrust)

-- | An object that represents a Transport Layer Security (TLS) validation
-- context trust for a local file.
virtualGatewayTlsValidationContextTrust_file :: Lens.Lens' VirtualGatewayTlsValidationContextTrust (Prelude.Maybe VirtualGatewayTlsValidationContextFileTrust)
virtualGatewayTlsValidationContextTrust_file = Lens.lens (\VirtualGatewayTlsValidationContextTrust' {file} -> file) (\s@VirtualGatewayTlsValidationContextTrust' {} a -> s {file = a} :: VirtualGatewayTlsValidationContextTrust)

-- | A reference to an object that represents a virtual gateway\'s Transport
-- Layer Security (TLS) Secret Discovery Service validation context trust.
virtualGatewayTlsValidationContextTrust_sds :: Lens.Lens' VirtualGatewayTlsValidationContextTrust (Prelude.Maybe VirtualGatewayTlsValidationContextSdsTrust)
virtualGatewayTlsValidationContextTrust_sds = Lens.lens (\VirtualGatewayTlsValidationContextTrust' {sds} -> sds) (\s@VirtualGatewayTlsValidationContextTrust' {} a -> s {sds = a} :: VirtualGatewayTlsValidationContextTrust)

instance
  Data.FromJSON
    VirtualGatewayTlsValidationContextTrust
  where
  parseJSON =
    Data.withObject
      "VirtualGatewayTlsValidationContextTrust"
      ( \x ->
          VirtualGatewayTlsValidationContextTrust'
            Prelude.<$> (x Data..:? "acm")
            Prelude.<*> (x Data..:? "file")
            Prelude.<*> (x Data..:? "sds")
      )

instance
  Prelude.Hashable
    VirtualGatewayTlsValidationContextTrust
  where
  hashWithSalt
    _salt
    VirtualGatewayTlsValidationContextTrust' {..} =
      _salt `Prelude.hashWithSalt` acm
        `Prelude.hashWithSalt` file
        `Prelude.hashWithSalt` sds

instance
  Prelude.NFData
    VirtualGatewayTlsValidationContextTrust
  where
  rnf VirtualGatewayTlsValidationContextTrust' {..} =
    Prelude.rnf acm
      `Prelude.seq` Prelude.rnf file
      `Prelude.seq` Prelude.rnf sds

instance
  Data.ToJSON
    VirtualGatewayTlsValidationContextTrust
  where
  toJSON VirtualGatewayTlsValidationContextTrust' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("acm" Data..=) Prelude.<$> acm,
            ("file" Data..=) Prelude.<$> file,
            ("sds" Data..=) Prelude.<$> sds
          ]
      )
