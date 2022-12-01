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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.VirtualGatewayListenerTlsValidationContextTrust where

import Amazonka.AppMesh.Types.VirtualGatewayTlsValidationContextFileTrust
import Amazonka.AppMesh.Types.VirtualGatewayTlsValidationContextSdsTrust
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a virtual gateway\'s listener\'s Transport
-- Layer Security (TLS) validation context trust.
--
-- /See:/ 'newVirtualGatewayListenerTlsValidationContextTrust' smart constructor.
data VirtualGatewayListenerTlsValidationContextTrust = VirtualGatewayListenerTlsValidationContextTrust'
  { -- | A reference to an object that represents a virtual gateway\'s
    -- listener\'s Transport Layer Security (TLS) Secret Discovery Service
    -- validation context trust.
    sds :: Prelude.Maybe VirtualGatewayTlsValidationContextSdsTrust,
    -- | An object that represents a Transport Layer Security (TLS) validation
    -- context trust for a local file.
    file :: Prelude.Maybe VirtualGatewayTlsValidationContextFileTrust
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
-- 'sds', 'virtualGatewayListenerTlsValidationContextTrust_sds' - A reference to an object that represents a virtual gateway\'s
-- listener\'s Transport Layer Security (TLS) Secret Discovery Service
-- validation context trust.
--
-- 'file', 'virtualGatewayListenerTlsValidationContextTrust_file' - An object that represents a Transport Layer Security (TLS) validation
-- context trust for a local file.
newVirtualGatewayListenerTlsValidationContextTrust ::
  VirtualGatewayListenerTlsValidationContextTrust
newVirtualGatewayListenerTlsValidationContextTrust =
  VirtualGatewayListenerTlsValidationContextTrust'
    { sds =
        Prelude.Nothing,
      file = Prelude.Nothing
    }

-- | A reference to an object that represents a virtual gateway\'s
-- listener\'s Transport Layer Security (TLS) Secret Discovery Service
-- validation context trust.
virtualGatewayListenerTlsValidationContextTrust_sds :: Lens.Lens' VirtualGatewayListenerTlsValidationContextTrust (Prelude.Maybe VirtualGatewayTlsValidationContextSdsTrust)
virtualGatewayListenerTlsValidationContextTrust_sds = Lens.lens (\VirtualGatewayListenerTlsValidationContextTrust' {sds} -> sds) (\s@VirtualGatewayListenerTlsValidationContextTrust' {} a -> s {sds = a} :: VirtualGatewayListenerTlsValidationContextTrust)

-- | An object that represents a Transport Layer Security (TLS) validation
-- context trust for a local file.
virtualGatewayListenerTlsValidationContextTrust_file :: Lens.Lens' VirtualGatewayListenerTlsValidationContextTrust (Prelude.Maybe VirtualGatewayTlsValidationContextFileTrust)
virtualGatewayListenerTlsValidationContextTrust_file = Lens.lens (\VirtualGatewayListenerTlsValidationContextTrust' {file} -> file) (\s@VirtualGatewayListenerTlsValidationContextTrust' {} a -> s {file = a} :: VirtualGatewayListenerTlsValidationContextTrust)

instance
  Core.FromJSON
    VirtualGatewayListenerTlsValidationContextTrust
  where
  parseJSON =
    Core.withObject
      "VirtualGatewayListenerTlsValidationContextTrust"
      ( \x ->
          VirtualGatewayListenerTlsValidationContextTrust'
            Prelude.<$> (x Core..:? "sds") Prelude.<*> (x Core..:? "file")
      )

instance
  Prelude.Hashable
    VirtualGatewayListenerTlsValidationContextTrust
  where
  hashWithSalt
    _salt
    VirtualGatewayListenerTlsValidationContextTrust' {..} =
      _salt `Prelude.hashWithSalt` sds
        `Prelude.hashWithSalt` file

instance
  Prelude.NFData
    VirtualGatewayListenerTlsValidationContextTrust
  where
  rnf
    VirtualGatewayListenerTlsValidationContextTrust' {..} =
      Prelude.rnf sds `Prelude.seq` Prelude.rnf file

instance
  Core.ToJSON
    VirtualGatewayListenerTlsValidationContextTrust
  where
  toJSON
    VirtualGatewayListenerTlsValidationContextTrust' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("sds" Core..=) Prelude.<$> sds,
              ("file" Core..=) Prelude.<$> file
            ]
        )
