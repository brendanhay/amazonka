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
-- Module      : Amazonka.AppMesh.Types.VirtualGatewayTlsValidationContextFileTrust
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.VirtualGatewayTlsValidationContextFileTrust where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a Transport Layer Security (TLS) validation
-- context trust for a local file.
--
-- /See:/ 'newVirtualGatewayTlsValidationContextFileTrust' smart constructor.
data VirtualGatewayTlsValidationContextFileTrust = VirtualGatewayTlsValidationContextFileTrust'
  { -- | The certificate trust chain for a certificate stored on the file system
    -- of the virtual node that the proxy is running on.
    certificateChain :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VirtualGatewayTlsValidationContextFileTrust' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateChain', 'virtualGatewayTlsValidationContextFileTrust_certificateChain' - The certificate trust chain for a certificate stored on the file system
-- of the virtual node that the proxy is running on.
newVirtualGatewayTlsValidationContextFileTrust ::
  -- | 'certificateChain'
  Prelude.Text ->
  VirtualGatewayTlsValidationContextFileTrust
newVirtualGatewayTlsValidationContextFileTrust
  pCertificateChain_ =
    VirtualGatewayTlsValidationContextFileTrust'
      { certificateChain =
          pCertificateChain_
      }

-- | The certificate trust chain for a certificate stored on the file system
-- of the virtual node that the proxy is running on.
virtualGatewayTlsValidationContextFileTrust_certificateChain :: Lens.Lens' VirtualGatewayTlsValidationContextFileTrust Prelude.Text
virtualGatewayTlsValidationContextFileTrust_certificateChain = Lens.lens (\VirtualGatewayTlsValidationContextFileTrust' {certificateChain} -> certificateChain) (\s@VirtualGatewayTlsValidationContextFileTrust' {} a -> s {certificateChain = a} :: VirtualGatewayTlsValidationContextFileTrust)

instance
  Data.FromJSON
    VirtualGatewayTlsValidationContextFileTrust
  where
  parseJSON =
    Data.withObject
      "VirtualGatewayTlsValidationContextFileTrust"
      ( \x ->
          VirtualGatewayTlsValidationContextFileTrust'
            Prelude.<$> (x Data..: "certificateChain")
      )

instance
  Prelude.Hashable
    VirtualGatewayTlsValidationContextFileTrust
  where
  hashWithSalt
    _salt
    VirtualGatewayTlsValidationContextFileTrust' {..} =
      _salt `Prelude.hashWithSalt` certificateChain

instance
  Prelude.NFData
    VirtualGatewayTlsValidationContextFileTrust
  where
  rnf VirtualGatewayTlsValidationContextFileTrust' {..} =
    Prelude.rnf certificateChain

instance
  Data.ToJSON
    VirtualGatewayTlsValidationContextFileTrust
  where
  toJSON
    VirtualGatewayTlsValidationContextFileTrust' {..} =
      Data.object
        ( Prelude.catMaybes
            [ Prelude.Just
                ("certificateChain" Data..= certificateChain)
            ]
        )
