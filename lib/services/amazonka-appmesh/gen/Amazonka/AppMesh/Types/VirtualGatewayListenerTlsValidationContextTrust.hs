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
-- Module      : Amazonka.AppMesh.Types.VirtualGatewayListenerTlsValidationContextTrust
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.VirtualGatewayListenerTlsValidationContextTrust where

import Amazonka.AppMesh.Types.VirtualGatewayTlsValidationContextFileTrust
import Amazonka.AppMesh.Types.VirtualGatewayTlsValidationContextSdsTrust
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a virtual gateway\'s listener\'s Transport
-- Layer Security (TLS) validation context trust.
--
-- /See:/ 'newVirtualGatewayListenerTlsValidationContextTrust' smart constructor.
data VirtualGatewayListenerTlsValidationContextTrust = VirtualGatewayListenerTlsValidationContextTrust'
  { -- | An object that represents a Transport Layer Security (TLS) validation
    -- context trust for a local file.
    file :: Prelude.Maybe VirtualGatewayTlsValidationContextFileTrust,
    -- | A reference to an object that represents a virtual gateway\'s
    -- listener\'s Transport Layer Security (TLS) Secret Discovery Service
    -- validation context trust.
    sds :: Prelude.Maybe VirtualGatewayTlsValidationContextSdsTrust
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VirtualGatewayListenerTlsValidationContextTrust' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'file', 'virtualGatewayListenerTlsValidationContextTrust_file' - An object that represents a Transport Layer Security (TLS) validation
-- context trust for a local file.
--
-- 'sds', 'virtualGatewayListenerTlsValidationContextTrust_sds' - A reference to an object that represents a virtual gateway\'s
-- listener\'s Transport Layer Security (TLS) Secret Discovery Service
-- validation context trust.
newVirtualGatewayListenerTlsValidationContextTrust ::
  VirtualGatewayListenerTlsValidationContextTrust
newVirtualGatewayListenerTlsValidationContextTrust =
  VirtualGatewayListenerTlsValidationContextTrust'
    { file =
        Prelude.Nothing,
      sds = Prelude.Nothing
    }

-- | An object that represents a Transport Layer Security (TLS) validation
-- context trust for a local file.
virtualGatewayListenerTlsValidationContextTrust_file :: Lens.Lens' VirtualGatewayListenerTlsValidationContextTrust (Prelude.Maybe VirtualGatewayTlsValidationContextFileTrust)
virtualGatewayListenerTlsValidationContextTrust_file = Lens.lens (\VirtualGatewayListenerTlsValidationContextTrust' {file} -> file) (\s@VirtualGatewayListenerTlsValidationContextTrust' {} a -> s {file = a} :: VirtualGatewayListenerTlsValidationContextTrust)

-- | A reference to an object that represents a virtual gateway\'s
-- listener\'s Transport Layer Security (TLS) Secret Discovery Service
-- validation context trust.
virtualGatewayListenerTlsValidationContextTrust_sds :: Lens.Lens' VirtualGatewayListenerTlsValidationContextTrust (Prelude.Maybe VirtualGatewayTlsValidationContextSdsTrust)
virtualGatewayListenerTlsValidationContextTrust_sds = Lens.lens (\VirtualGatewayListenerTlsValidationContextTrust' {sds} -> sds) (\s@VirtualGatewayListenerTlsValidationContextTrust' {} a -> s {sds = a} :: VirtualGatewayListenerTlsValidationContextTrust)

instance
  Data.FromJSON
    VirtualGatewayListenerTlsValidationContextTrust
  where
  parseJSON =
    Data.withObject
      "VirtualGatewayListenerTlsValidationContextTrust"
      ( \x ->
          VirtualGatewayListenerTlsValidationContextTrust'
            Prelude.<$> (x Data..:? "file")
            Prelude.<*> (x Data..:? "sds")
      )

instance
  Prelude.Hashable
    VirtualGatewayListenerTlsValidationContextTrust
  where
  hashWithSalt
    _salt
    VirtualGatewayListenerTlsValidationContextTrust' {..} =
      _salt
        `Prelude.hashWithSalt` file
        `Prelude.hashWithSalt` sds

instance
  Prelude.NFData
    VirtualGatewayListenerTlsValidationContextTrust
  where
  rnf
    VirtualGatewayListenerTlsValidationContextTrust' {..} =
      Prelude.rnf file `Prelude.seq` Prelude.rnf sds

instance
  Data.ToJSON
    VirtualGatewayListenerTlsValidationContextTrust
  where
  toJSON
    VirtualGatewayListenerTlsValidationContextTrust' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("file" Data..=) Prelude.<$> file,
              ("sds" Data..=) Prelude.<$> sds
            ]
        )
