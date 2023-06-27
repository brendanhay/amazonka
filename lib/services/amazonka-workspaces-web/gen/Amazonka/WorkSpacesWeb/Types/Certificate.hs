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
-- Module      : Amazonka.WorkSpacesWeb.Types.Certificate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpacesWeb.Types.Certificate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The certificate.
--
-- /See:/ 'newCertificate' smart constructor.
data Certificate = Certificate'
  { -- | The body of the certificate.
    body :: Prelude.Maybe Data.Base64,
    -- | The entity that issued the certificate.
    issuer :: Prelude.Maybe Prelude.Text,
    -- | The certificate is not valid after this date.
    notValidAfter :: Prelude.Maybe Data.POSIX,
    -- | The certificate is not valid before this date.
    notValidBefore :: Prelude.Maybe Data.POSIX,
    -- | The entity the certificate belongs to.
    subject :: Prelude.Maybe Prelude.Text,
    -- | A hexadecimal identifier for the certificate.
    thumbprint :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Certificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'body', 'certificate_body' - The body of the certificate.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'issuer', 'certificate_issuer' - The entity that issued the certificate.
--
-- 'notValidAfter', 'certificate_notValidAfter' - The certificate is not valid after this date.
--
-- 'notValidBefore', 'certificate_notValidBefore' - The certificate is not valid before this date.
--
-- 'subject', 'certificate_subject' - The entity the certificate belongs to.
--
-- 'thumbprint', 'certificate_thumbprint' - A hexadecimal identifier for the certificate.
newCertificate ::
  Certificate
newCertificate =
  Certificate'
    { body = Prelude.Nothing,
      issuer = Prelude.Nothing,
      notValidAfter = Prelude.Nothing,
      notValidBefore = Prelude.Nothing,
      subject = Prelude.Nothing,
      thumbprint = Prelude.Nothing
    }

-- | The body of the certificate.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
certificate_body :: Lens.Lens' Certificate (Prelude.Maybe Prelude.ByteString)
certificate_body = Lens.lens (\Certificate' {body} -> body) (\s@Certificate' {} a -> s {body = a} :: Certificate) Prelude.. Lens.mapping Data._Base64

-- | The entity that issued the certificate.
certificate_issuer :: Lens.Lens' Certificate (Prelude.Maybe Prelude.Text)
certificate_issuer = Lens.lens (\Certificate' {issuer} -> issuer) (\s@Certificate' {} a -> s {issuer = a} :: Certificate)

-- | The certificate is not valid after this date.
certificate_notValidAfter :: Lens.Lens' Certificate (Prelude.Maybe Prelude.UTCTime)
certificate_notValidAfter = Lens.lens (\Certificate' {notValidAfter} -> notValidAfter) (\s@Certificate' {} a -> s {notValidAfter = a} :: Certificate) Prelude.. Lens.mapping Data._Time

-- | The certificate is not valid before this date.
certificate_notValidBefore :: Lens.Lens' Certificate (Prelude.Maybe Prelude.UTCTime)
certificate_notValidBefore = Lens.lens (\Certificate' {notValidBefore} -> notValidBefore) (\s@Certificate' {} a -> s {notValidBefore = a} :: Certificate) Prelude.. Lens.mapping Data._Time

-- | The entity the certificate belongs to.
certificate_subject :: Lens.Lens' Certificate (Prelude.Maybe Prelude.Text)
certificate_subject = Lens.lens (\Certificate' {subject} -> subject) (\s@Certificate' {} a -> s {subject = a} :: Certificate)

-- | A hexadecimal identifier for the certificate.
certificate_thumbprint :: Lens.Lens' Certificate (Prelude.Maybe Prelude.Text)
certificate_thumbprint = Lens.lens (\Certificate' {thumbprint} -> thumbprint) (\s@Certificate' {} a -> s {thumbprint = a} :: Certificate)

instance Data.FromJSON Certificate where
  parseJSON =
    Data.withObject
      "Certificate"
      ( \x ->
          Certificate'
            Prelude.<$> (x Data..:? "body")
            Prelude.<*> (x Data..:? "issuer")
            Prelude.<*> (x Data..:? "notValidAfter")
            Prelude.<*> (x Data..:? "notValidBefore")
            Prelude.<*> (x Data..:? "subject")
            Prelude.<*> (x Data..:? "thumbprint")
      )

instance Prelude.Hashable Certificate where
  hashWithSalt _salt Certificate' {..} =
    _salt
      `Prelude.hashWithSalt` body
      `Prelude.hashWithSalt` issuer
      `Prelude.hashWithSalt` notValidAfter
      `Prelude.hashWithSalt` notValidBefore
      `Prelude.hashWithSalt` subject
      `Prelude.hashWithSalt` thumbprint

instance Prelude.NFData Certificate where
  rnf Certificate' {..} =
    Prelude.rnf body
      `Prelude.seq` Prelude.rnf issuer
      `Prelude.seq` Prelude.rnf notValidAfter
      `Prelude.seq` Prelude.rnf notValidBefore
      `Prelude.seq` Prelude.rnf subject
      `Prelude.seq` Prelude.rnf thumbprint
