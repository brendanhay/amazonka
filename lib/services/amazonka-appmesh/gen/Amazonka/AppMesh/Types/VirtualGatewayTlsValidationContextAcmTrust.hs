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
-- Module      : Amazonka.AppMesh.Types.VirtualGatewayTlsValidationContextAcmTrust
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.VirtualGatewayTlsValidationContextAcmTrust where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a Transport Layer Security (TLS) validation
-- context trust for an Certificate Manager certificate.
--
-- /See:/ 'newVirtualGatewayTlsValidationContextAcmTrust' smart constructor.
data VirtualGatewayTlsValidationContextAcmTrust = VirtualGatewayTlsValidationContextAcmTrust'
  { -- | One or more ACM Amazon Resource Name (ARN)s.
    certificateAuthorityArns :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VirtualGatewayTlsValidationContextAcmTrust' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateAuthorityArns', 'virtualGatewayTlsValidationContextAcmTrust_certificateAuthorityArns' - One or more ACM Amazon Resource Name (ARN)s.
newVirtualGatewayTlsValidationContextAcmTrust ::
  -- | 'certificateAuthorityArns'
  Prelude.NonEmpty Prelude.Text ->
  VirtualGatewayTlsValidationContextAcmTrust
newVirtualGatewayTlsValidationContextAcmTrust
  pCertificateAuthorityArns_ =
    VirtualGatewayTlsValidationContextAcmTrust'
      { certificateAuthorityArns =
          Lens.coerced
            Lens.# pCertificateAuthorityArns_
      }

-- | One or more ACM Amazon Resource Name (ARN)s.
virtualGatewayTlsValidationContextAcmTrust_certificateAuthorityArns :: Lens.Lens' VirtualGatewayTlsValidationContextAcmTrust (Prelude.NonEmpty Prelude.Text)
virtualGatewayTlsValidationContextAcmTrust_certificateAuthorityArns = Lens.lens (\VirtualGatewayTlsValidationContextAcmTrust' {certificateAuthorityArns} -> certificateAuthorityArns) (\s@VirtualGatewayTlsValidationContextAcmTrust' {} a -> s {certificateAuthorityArns = a} :: VirtualGatewayTlsValidationContextAcmTrust) Prelude.. Lens.coerced

instance
  Core.FromJSON
    VirtualGatewayTlsValidationContextAcmTrust
  where
  parseJSON =
    Core.withObject
      "VirtualGatewayTlsValidationContextAcmTrust"
      ( \x ->
          VirtualGatewayTlsValidationContextAcmTrust'
            Prelude.<$> (x Core..: "certificateAuthorityArns")
      )

instance
  Prelude.Hashable
    VirtualGatewayTlsValidationContextAcmTrust
  where
  hashWithSalt
    _salt
    VirtualGatewayTlsValidationContextAcmTrust' {..} =
      _salt
        `Prelude.hashWithSalt` certificateAuthorityArns

instance
  Prelude.NFData
    VirtualGatewayTlsValidationContextAcmTrust
  where
  rnf VirtualGatewayTlsValidationContextAcmTrust' {..} =
    Prelude.rnf certificateAuthorityArns

instance
  Core.ToJSON
    VirtualGatewayTlsValidationContextAcmTrust
  where
  toJSON
    VirtualGatewayTlsValidationContextAcmTrust' {..} =
      Core.object
        ( Prelude.catMaybes
            [ Prelude.Just
                ( "certificateAuthorityArns"
                    Core..= certificateAuthorityArns
                )
            ]
        )
