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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.TlsValidationContextTrust where

import Amazonka.AppMesh.Types.TlsValidationContextAcmTrust
import Amazonka.AppMesh.Types.TlsValidationContextFileTrust
import Amazonka.AppMesh.Types.TlsValidationContextSdsTrust
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a Transport Layer Security (TLS) validation
-- context trust.
--
-- /See:/ 'newTlsValidationContextTrust' smart constructor.
data TlsValidationContextTrust = TlsValidationContextTrust'
  { -- | A reference to an object that represents a Transport Layer Security
    -- (TLS) Secret Discovery Service validation context trust.
    sds :: Prelude.Maybe TlsValidationContextSdsTrust,
    -- | An object that represents a Transport Layer Security (TLS) validation
    -- context trust for a local file.
    file :: Prelude.Maybe TlsValidationContextFileTrust,
    -- | A reference to an object that represents a Transport Layer Security
    -- (TLS) validation context trust for an Certificate Manager certificate.
    acm :: Prelude.Maybe TlsValidationContextAcmTrust
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
-- 'sds', 'tlsValidationContextTrust_sds' - A reference to an object that represents a Transport Layer Security
-- (TLS) Secret Discovery Service validation context trust.
--
-- 'file', 'tlsValidationContextTrust_file' - An object that represents a Transport Layer Security (TLS) validation
-- context trust for a local file.
--
-- 'acm', 'tlsValidationContextTrust_acm' - A reference to an object that represents a Transport Layer Security
-- (TLS) validation context trust for an Certificate Manager certificate.
newTlsValidationContextTrust ::
  TlsValidationContextTrust
newTlsValidationContextTrust =
  TlsValidationContextTrust'
    { sds = Prelude.Nothing,
      file = Prelude.Nothing,
      acm = Prelude.Nothing
    }

-- | A reference to an object that represents a Transport Layer Security
-- (TLS) Secret Discovery Service validation context trust.
tlsValidationContextTrust_sds :: Lens.Lens' TlsValidationContextTrust (Prelude.Maybe TlsValidationContextSdsTrust)
tlsValidationContextTrust_sds = Lens.lens (\TlsValidationContextTrust' {sds} -> sds) (\s@TlsValidationContextTrust' {} a -> s {sds = a} :: TlsValidationContextTrust)

-- | An object that represents a Transport Layer Security (TLS) validation
-- context trust for a local file.
tlsValidationContextTrust_file :: Lens.Lens' TlsValidationContextTrust (Prelude.Maybe TlsValidationContextFileTrust)
tlsValidationContextTrust_file = Lens.lens (\TlsValidationContextTrust' {file} -> file) (\s@TlsValidationContextTrust' {} a -> s {file = a} :: TlsValidationContextTrust)

-- | A reference to an object that represents a Transport Layer Security
-- (TLS) validation context trust for an Certificate Manager certificate.
tlsValidationContextTrust_acm :: Lens.Lens' TlsValidationContextTrust (Prelude.Maybe TlsValidationContextAcmTrust)
tlsValidationContextTrust_acm = Lens.lens (\TlsValidationContextTrust' {acm} -> acm) (\s@TlsValidationContextTrust' {} a -> s {acm = a} :: TlsValidationContextTrust)

instance Core.FromJSON TlsValidationContextTrust where
  parseJSON =
    Core.withObject
      "TlsValidationContextTrust"
      ( \x ->
          TlsValidationContextTrust'
            Prelude.<$> (x Core..:? "sds")
            Prelude.<*> (x Core..:? "file")
            Prelude.<*> (x Core..:? "acm")
      )

instance Prelude.Hashable TlsValidationContextTrust where
  hashWithSalt _salt TlsValidationContextTrust' {..} =
    _salt `Prelude.hashWithSalt` sds
      `Prelude.hashWithSalt` file
      `Prelude.hashWithSalt` acm

instance Prelude.NFData TlsValidationContextTrust where
  rnf TlsValidationContextTrust' {..} =
    Prelude.rnf sds
      `Prelude.seq` Prelude.rnf file
      `Prelude.seq` Prelude.rnf acm

instance Core.ToJSON TlsValidationContextTrust where
  toJSON TlsValidationContextTrust' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("sds" Core..=) Prelude.<$> sds,
            ("file" Core..=) Prelude.<$> file,
            ("acm" Core..=) Prelude.<$> acm
          ]
      )
