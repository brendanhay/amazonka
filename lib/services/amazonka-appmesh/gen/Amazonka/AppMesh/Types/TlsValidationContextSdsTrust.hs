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
-- Module      : Amazonka.AppMesh.Types.TlsValidationContextSdsTrust
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.TlsValidationContextSdsTrust where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a Transport Layer Security (TLS) Secret
-- Discovery Service validation context trust. The proxy must be configured
-- with a local SDS provider via a Unix Domain Socket. See App Mesh
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/tls.html TLS documentation>
-- for more info.
--
-- /See:/ 'newTlsValidationContextSdsTrust' smart constructor.
data TlsValidationContextSdsTrust = TlsValidationContextSdsTrust'
  { -- | A reference to an object that represents the name of the secret for a
    -- Transport Layer Security (TLS) Secret Discovery Service validation
    -- context trust.
    secretName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TlsValidationContextSdsTrust' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'secretName', 'tlsValidationContextSdsTrust_secretName' - A reference to an object that represents the name of the secret for a
-- Transport Layer Security (TLS) Secret Discovery Service validation
-- context trust.
newTlsValidationContextSdsTrust ::
  -- | 'secretName'
  Prelude.Text ->
  TlsValidationContextSdsTrust
newTlsValidationContextSdsTrust pSecretName_ =
  TlsValidationContextSdsTrust'
    { secretName =
        pSecretName_
    }

-- | A reference to an object that represents the name of the secret for a
-- Transport Layer Security (TLS) Secret Discovery Service validation
-- context trust.
tlsValidationContextSdsTrust_secretName :: Lens.Lens' TlsValidationContextSdsTrust Prelude.Text
tlsValidationContextSdsTrust_secretName = Lens.lens (\TlsValidationContextSdsTrust' {secretName} -> secretName) (\s@TlsValidationContextSdsTrust' {} a -> s {secretName = a} :: TlsValidationContextSdsTrust)

instance Data.FromJSON TlsValidationContextSdsTrust where
  parseJSON =
    Data.withObject
      "TlsValidationContextSdsTrust"
      ( \x ->
          TlsValidationContextSdsTrust'
            Prelude.<$> (x Data..: "secretName")
      )

instance
  Prelude.Hashable
    TlsValidationContextSdsTrust
  where
  hashWithSalt _salt TlsValidationContextSdsTrust' {..} =
    _salt `Prelude.hashWithSalt` secretName

instance Prelude.NFData TlsValidationContextSdsTrust where
  rnf TlsValidationContextSdsTrust' {..} =
    Prelude.rnf secretName

instance Data.ToJSON TlsValidationContextSdsTrust where
  toJSON TlsValidationContextSdsTrust' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("secretName" Data..= secretName)]
      )
