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
-- Module      : Amazonka.WorkSpacesWeb.Types.CertificateSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpacesWeb.Types.CertificateSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The summary of the certificate.
--
-- /See:/ 'newCertificateSummary' smart constructor.
data CertificateSummary = CertificateSummary'
  { -- | The entity that issued the certificate.
    issuer :: Prelude.Maybe Prelude.Text,
    -- | The certificate is not valid after this date.
    notValidAfter :: Prelude.Maybe Data.POSIX,
    -- | A hexadecimal identifier for the certificate.
    thumbprint :: Prelude.Maybe Prelude.Text,
    -- | The certificate is not valid before this date.
    notValidBefore :: Prelude.Maybe Data.POSIX,
    -- | The entity the certificate belongs to.
    subject :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CertificateSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'issuer', 'certificateSummary_issuer' - The entity that issued the certificate.
--
-- 'notValidAfter', 'certificateSummary_notValidAfter' - The certificate is not valid after this date.
--
-- 'thumbprint', 'certificateSummary_thumbprint' - A hexadecimal identifier for the certificate.
--
-- 'notValidBefore', 'certificateSummary_notValidBefore' - The certificate is not valid before this date.
--
-- 'subject', 'certificateSummary_subject' - The entity the certificate belongs to.
newCertificateSummary ::
  CertificateSummary
newCertificateSummary =
  CertificateSummary'
    { issuer = Prelude.Nothing,
      notValidAfter = Prelude.Nothing,
      thumbprint = Prelude.Nothing,
      notValidBefore = Prelude.Nothing,
      subject = Prelude.Nothing
    }

-- | The entity that issued the certificate.
certificateSummary_issuer :: Lens.Lens' CertificateSummary (Prelude.Maybe Prelude.Text)
certificateSummary_issuer = Lens.lens (\CertificateSummary' {issuer} -> issuer) (\s@CertificateSummary' {} a -> s {issuer = a} :: CertificateSummary)

-- | The certificate is not valid after this date.
certificateSummary_notValidAfter :: Lens.Lens' CertificateSummary (Prelude.Maybe Prelude.UTCTime)
certificateSummary_notValidAfter = Lens.lens (\CertificateSummary' {notValidAfter} -> notValidAfter) (\s@CertificateSummary' {} a -> s {notValidAfter = a} :: CertificateSummary) Prelude.. Lens.mapping Data._Time

-- | A hexadecimal identifier for the certificate.
certificateSummary_thumbprint :: Lens.Lens' CertificateSummary (Prelude.Maybe Prelude.Text)
certificateSummary_thumbprint = Lens.lens (\CertificateSummary' {thumbprint} -> thumbprint) (\s@CertificateSummary' {} a -> s {thumbprint = a} :: CertificateSummary)

-- | The certificate is not valid before this date.
certificateSummary_notValidBefore :: Lens.Lens' CertificateSummary (Prelude.Maybe Prelude.UTCTime)
certificateSummary_notValidBefore = Lens.lens (\CertificateSummary' {notValidBefore} -> notValidBefore) (\s@CertificateSummary' {} a -> s {notValidBefore = a} :: CertificateSummary) Prelude.. Lens.mapping Data._Time

-- | The entity the certificate belongs to.
certificateSummary_subject :: Lens.Lens' CertificateSummary (Prelude.Maybe Prelude.Text)
certificateSummary_subject = Lens.lens (\CertificateSummary' {subject} -> subject) (\s@CertificateSummary' {} a -> s {subject = a} :: CertificateSummary)

instance Data.FromJSON CertificateSummary where
  parseJSON =
    Data.withObject
      "CertificateSummary"
      ( \x ->
          CertificateSummary'
            Prelude.<$> (x Data..:? "issuer")
            Prelude.<*> (x Data..:? "notValidAfter")
            Prelude.<*> (x Data..:? "thumbprint")
            Prelude.<*> (x Data..:? "notValidBefore")
            Prelude.<*> (x Data..:? "subject")
      )

instance Prelude.Hashable CertificateSummary where
  hashWithSalt _salt CertificateSummary' {..} =
    _salt `Prelude.hashWithSalt` issuer
      `Prelude.hashWithSalt` notValidAfter
      `Prelude.hashWithSalt` thumbprint
      `Prelude.hashWithSalt` notValidBefore
      `Prelude.hashWithSalt` subject

instance Prelude.NFData CertificateSummary where
  rnf CertificateSummary' {..} =
    Prelude.rnf issuer
      `Prelude.seq` Prelude.rnf notValidAfter
      `Prelude.seq` Prelude.rnf thumbprint
      `Prelude.seq` Prelude.rnf notValidBefore
      `Prelude.seq` Prelude.rnf subject
