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
-- Module      : Amazonka.IoT.Types.IssuerCertificateIdentifier
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.IssuerCertificateIdentifier where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The certificate issuer indentifier.
--
-- /See:/ 'newIssuerCertificateIdentifier' smart constructor.
data IssuerCertificateIdentifier = IssuerCertificateIdentifier'
  { -- | The issuer certificate serial number.
    issuerCertificateSerialNumber :: Prelude.Maybe Prelude.Text,
    -- | The issuer ID.
    issuerId :: Prelude.Maybe Prelude.Text,
    -- | The subject of the issuer certificate.
    issuerCertificateSubject :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IssuerCertificateIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'issuerCertificateSerialNumber', 'issuerCertificateIdentifier_issuerCertificateSerialNumber' - The issuer certificate serial number.
--
-- 'issuerId', 'issuerCertificateIdentifier_issuerId' - The issuer ID.
--
-- 'issuerCertificateSubject', 'issuerCertificateIdentifier_issuerCertificateSubject' - The subject of the issuer certificate.
newIssuerCertificateIdentifier ::
  IssuerCertificateIdentifier
newIssuerCertificateIdentifier =
  IssuerCertificateIdentifier'
    { issuerCertificateSerialNumber =
        Prelude.Nothing,
      issuerId = Prelude.Nothing,
      issuerCertificateSubject = Prelude.Nothing
    }

-- | The issuer certificate serial number.
issuerCertificateIdentifier_issuerCertificateSerialNumber :: Lens.Lens' IssuerCertificateIdentifier (Prelude.Maybe Prelude.Text)
issuerCertificateIdentifier_issuerCertificateSerialNumber = Lens.lens (\IssuerCertificateIdentifier' {issuerCertificateSerialNumber} -> issuerCertificateSerialNumber) (\s@IssuerCertificateIdentifier' {} a -> s {issuerCertificateSerialNumber = a} :: IssuerCertificateIdentifier)

-- | The issuer ID.
issuerCertificateIdentifier_issuerId :: Lens.Lens' IssuerCertificateIdentifier (Prelude.Maybe Prelude.Text)
issuerCertificateIdentifier_issuerId = Lens.lens (\IssuerCertificateIdentifier' {issuerId} -> issuerId) (\s@IssuerCertificateIdentifier' {} a -> s {issuerId = a} :: IssuerCertificateIdentifier)

-- | The subject of the issuer certificate.
issuerCertificateIdentifier_issuerCertificateSubject :: Lens.Lens' IssuerCertificateIdentifier (Prelude.Maybe Prelude.Text)
issuerCertificateIdentifier_issuerCertificateSubject = Lens.lens (\IssuerCertificateIdentifier' {issuerCertificateSubject} -> issuerCertificateSubject) (\s@IssuerCertificateIdentifier' {} a -> s {issuerCertificateSubject = a} :: IssuerCertificateIdentifier)

instance Core.FromJSON IssuerCertificateIdentifier where
  parseJSON =
    Core.withObject
      "IssuerCertificateIdentifier"
      ( \x ->
          IssuerCertificateIdentifier'
            Prelude.<$> (x Core..:? "issuerCertificateSerialNumber")
            Prelude.<*> (x Core..:? "issuerId")
            Prelude.<*> (x Core..:? "issuerCertificateSubject")
      )

instance Prelude.Hashable IssuerCertificateIdentifier where
  hashWithSalt _salt IssuerCertificateIdentifier' {..} =
    _salt
      `Prelude.hashWithSalt` issuerCertificateSerialNumber
      `Prelude.hashWithSalt` issuerId
      `Prelude.hashWithSalt` issuerCertificateSubject

instance Prelude.NFData IssuerCertificateIdentifier where
  rnf IssuerCertificateIdentifier' {..} =
    Prelude.rnf issuerCertificateSerialNumber
      `Prelude.seq` Prelude.rnf issuerId
      `Prelude.seq` Prelude.rnf issuerCertificateSubject

instance Core.ToJSON IssuerCertificateIdentifier where
  toJSON IssuerCertificateIdentifier' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("issuerCertificateSerialNumber" Core..=)
              Prelude.<$> issuerCertificateSerialNumber,
            ("issuerId" Core..=) Prelude.<$> issuerId,
            ("issuerCertificateSubject" Core..=)
              Prelude.<$> issuerCertificateSubject
          ]
      )
