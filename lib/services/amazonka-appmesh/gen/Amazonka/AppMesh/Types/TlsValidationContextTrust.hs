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
-- Module      : Amazonka.AppMesh.Types.TlsValidationContextTrust
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.TlsValidationContextTrust where

import Amazonka.AppMesh.Types.TlsValidationContextAcmTrust
import Amazonka.AppMesh.Types.TlsValidationContextFileTrust
import Amazonka.AppMesh.Types.TlsValidationContextSdsTrust
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a Transport Layer Security (TLS) validation
-- context trust.
--
-- /See:/ 'newTlsValidationContextTrust' smart constructor.
data TlsValidationContextTrust = TlsValidationContextTrust'
  { -- | A reference to an object that represents a Transport Layer Security
    -- (TLS) validation context trust for an Certificate Manager certificate.
    acm :: Prelude.Maybe TlsValidationContextAcmTrust,
    -- | A reference to an object that represents a Transport Layer Security
    -- (TLS) Secret Discovery Service validation context trust.
    sds :: Prelude.Maybe TlsValidationContextSdsTrust,
    -- | An object that represents a Transport Layer Security (TLS) validation
    -- context trust for a local file.
    file :: Prelude.Maybe TlsValidationContextFileTrust
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TlsValidationContextTrust' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acm', 'tlsValidationContextTrust_acm' - A reference to an object that represents a Transport Layer Security
-- (TLS) validation context trust for an Certificate Manager certificate.
--
-- 'sds', 'tlsValidationContextTrust_sds' - A reference to an object that represents a Transport Layer Security
-- (TLS) Secret Discovery Service validation context trust.
--
-- 'file', 'tlsValidationContextTrust_file' - An object that represents a Transport Layer Security (TLS) validation
-- context trust for a local file.
newTlsValidationContextTrust ::
  TlsValidationContextTrust
newTlsValidationContextTrust =
  TlsValidationContextTrust'
    { acm = Prelude.Nothing,
      sds = Prelude.Nothing,
      file = Prelude.Nothing
    }

-- | A reference to an object that represents a Transport Layer Security
-- (TLS) validation context trust for an Certificate Manager certificate.
tlsValidationContextTrust_acm :: Lens.Lens' TlsValidationContextTrust (Prelude.Maybe TlsValidationContextAcmTrust)
tlsValidationContextTrust_acm = Lens.lens (\TlsValidationContextTrust' {acm} -> acm) (\s@TlsValidationContextTrust' {} a -> s {acm = a} :: TlsValidationContextTrust)

-- | A reference to an object that represents a Transport Layer Security
-- (TLS) Secret Discovery Service validation context trust.
tlsValidationContextTrust_sds :: Lens.Lens' TlsValidationContextTrust (Prelude.Maybe TlsValidationContextSdsTrust)
tlsValidationContextTrust_sds = Lens.lens (\TlsValidationContextTrust' {sds} -> sds) (\s@TlsValidationContextTrust' {} a -> s {sds = a} :: TlsValidationContextTrust)

-- | An object that represents a Transport Layer Security (TLS) validation
-- context trust for a local file.
tlsValidationContextTrust_file :: Lens.Lens' TlsValidationContextTrust (Prelude.Maybe TlsValidationContextFileTrust)
tlsValidationContextTrust_file = Lens.lens (\TlsValidationContextTrust' {file} -> file) (\s@TlsValidationContextTrust' {} a -> s {file = a} :: TlsValidationContextTrust)

instance Core.FromJSON TlsValidationContextTrust where
  parseJSON =
    Core.withObject
      "TlsValidationContextTrust"
      ( \x ->
          TlsValidationContextTrust'
            Prelude.<$> (x Core..:? "acm")
            Prelude.<*> (x Core..:? "sds")
            Prelude.<*> (x Core..:? "file")
      )

instance Prelude.Hashable TlsValidationContextTrust where
  hashWithSalt salt' TlsValidationContextTrust' {..} =
    salt' `Prelude.hashWithSalt` file
      `Prelude.hashWithSalt` sds
      `Prelude.hashWithSalt` acm

instance Prelude.NFData TlsValidationContextTrust where
  rnf TlsValidationContextTrust' {..} =
    Prelude.rnf acm `Prelude.seq` Prelude.rnf file
      `Prelude.seq` Prelude.rnf sds

instance Core.ToJSON TlsValidationContextTrust where
  toJSON TlsValidationContextTrust' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("acm" Core..=) Prelude.<$> acm,
            ("sds" Core..=) Prelude.<$> sds,
            ("file" Core..=) Prelude.<$> file
          ]
      )
