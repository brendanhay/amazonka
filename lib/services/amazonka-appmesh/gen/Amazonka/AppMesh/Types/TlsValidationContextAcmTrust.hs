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
-- Module      : Amazonka.AppMesh.Types.TlsValidationContextAcmTrust
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.TlsValidationContextAcmTrust where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a Transport Layer Security (TLS) validation
-- context trust for an Certificate Manager certificate.
--
-- /See:/ 'newTlsValidationContextAcmTrust' smart constructor.
data TlsValidationContextAcmTrust = TlsValidationContextAcmTrust'
  { -- | One or more ACM Amazon Resource Name (ARN)s.
    certificateAuthorityArns :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TlsValidationContextAcmTrust' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateAuthorityArns', 'tlsValidationContextAcmTrust_certificateAuthorityArns' - One or more ACM Amazon Resource Name (ARN)s.
newTlsValidationContextAcmTrust ::
  -- | 'certificateAuthorityArns'
  Prelude.NonEmpty Prelude.Text ->
  TlsValidationContextAcmTrust
newTlsValidationContextAcmTrust
  pCertificateAuthorityArns_ =
    TlsValidationContextAcmTrust'
      { certificateAuthorityArns =
          Lens.coerced
            Lens.# pCertificateAuthorityArns_
      }

-- | One or more ACM Amazon Resource Name (ARN)s.
tlsValidationContextAcmTrust_certificateAuthorityArns :: Lens.Lens' TlsValidationContextAcmTrust (Prelude.NonEmpty Prelude.Text)
tlsValidationContextAcmTrust_certificateAuthorityArns = Lens.lens (\TlsValidationContextAcmTrust' {certificateAuthorityArns} -> certificateAuthorityArns) (\s@TlsValidationContextAcmTrust' {} a -> s {certificateAuthorityArns = a} :: TlsValidationContextAcmTrust) Prelude.. Lens.coerced

instance Core.FromJSON TlsValidationContextAcmTrust where
  parseJSON =
    Core.withObject
      "TlsValidationContextAcmTrust"
      ( \x ->
          TlsValidationContextAcmTrust'
            Prelude.<$> (x Core..: "certificateAuthorityArns")
      )

instance
  Prelude.Hashable
    TlsValidationContextAcmTrust
  where
  hashWithSalt _salt TlsValidationContextAcmTrust' {..} =
    _salt
      `Prelude.hashWithSalt` certificateAuthorityArns

instance Prelude.NFData TlsValidationContextAcmTrust where
  rnf TlsValidationContextAcmTrust' {..} =
    Prelude.rnf certificateAuthorityArns

instance Core.ToJSON TlsValidationContextAcmTrust where
  toJSON TlsValidationContextAcmTrust' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "certificateAuthorityArns"
                  Core..= certificateAuthorityArns
              )
          ]
      )
