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
-- Module      : Amazonka.AppMesh.Types.TlsValidationContextFileTrust
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.TlsValidationContextFileTrust where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a Transport Layer Security (TLS) validation
-- context trust for a local file.
--
-- /See:/ 'newTlsValidationContextFileTrust' smart constructor.
data TlsValidationContextFileTrust = TlsValidationContextFileTrust'
  { -- | The certificate trust chain for a certificate stored on the file system
    -- of the virtual node that the proxy is running on.
    certificateChain :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TlsValidationContextFileTrust' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateChain', 'tlsValidationContextFileTrust_certificateChain' - The certificate trust chain for a certificate stored on the file system
-- of the virtual node that the proxy is running on.
newTlsValidationContextFileTrust ::
  -- | 'certificateChain'
  Prelude.Text ->
  TlsValidationContextFileTrust
newTlsValidationContextFileTrust pCertificateChain_ =
  TlsValidationContextFileTrust'
    { certificateChain =
        pCertificateChain_
    }

-- | The certificate trust chain for a certificate stored on the file system
-- of the virtual node that the proxy is running on.
tlsValidationContextFileTrust_certificateChain :: Lens.Lens' TlsValidationContextFileTrust Prelude.Text
tlsValidationContextFileTrust_certificateChain = Lens.lens (\TlsValidationContextFileTrust' {certificateChain} -> certificateChain) (\s@TlsValidationContextFileTrust' {} a -> s {certificateChain = a} :: TlsValidationContextFileTrust)

instance Data.FromJSON TlsValidationContextFileTrust where
  parseJSON =
    Data.withObject
      "TlsValidationContextFileTrust"
      ( \x ->
          TlsValidationContextFileTrust'
            Prelude.<$> (x Data..: "certificateChain")
      )

instance
  Prelude.Hashable
    TlsValidationContextFileTrust
  where
  hashWithSalt _salt TlsValidationContextFileTrust' {..} =
    _salt `Prelude.hashWithSalt` certificateChain

instance Prelude.NFData TlsValidationContextFileTrust where
  rnf TlsValidationContextFileTrust' {..} =
    Prelude.rnf certificateChain

instance Data.ToJSON TlsValidationContextFileTrust where
  toJSON TlsValidationContextFileTrust' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("certificateChain" Data..= certificateChain)
          ]
      )
