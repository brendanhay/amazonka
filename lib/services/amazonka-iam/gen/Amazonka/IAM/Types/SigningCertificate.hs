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
-- Module      : Amazonka.IAM.Types.SigningCertificate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IAM.Types.SigningCertificate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types.StatusType
import qualified Amazonka.Prelude as Prelude

-- | Contains information about an X.509 signing certificate.
--
-- This data type is used as a response element in the
-- UploadSigningCertificate and ListSigningCertificates operations.
--
-- /See:/ 'newSigningCertificate' smart constructor.
data SigningCertificate = SigningCertificate'
  { -- | The date when the signing certificate was uploaded.
    uploadDate :: Prelude.Maybe Data.ISO8601,
    -- | The name of the user the signing certificate is associated with.
    userName :: Prelude.Text,
    -- | The ID for the signing certificate.
    certificateId :: Prelude.Text,
    -- | The contents of the signing certificate.
    certificateBody :: Prelude.Text,
    -- | The status of the signing certificate. @Active@ means that the key is
    -- valid for API calls, while @Inactive@ means it is not.
    status :: StatusType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SigningCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'uploadDate', 'signingCertificate_uploadDate' - The date when the signing certificate was uploaded.
--
-- 'userName', 'signingCertificate_userName' - The name of the user the signing certificate is associated with.
--
-- 'certificateId', 'signingCertificate_certificateId' - The ID for the signing certificate.
--
-- 'certificateBody', 'signingCertificate_certificateBody' - The contents of the signing certificate.
--
-- 'status', 'signingCertificate_status' - The status of the signing certificate. @Active@ means that the key is
-- valid for API calls, while @Inactive@ means it is not.
newSigningCertificate ::
  -- | 'userName'
  Prelude.Text ->
  -- | 'certificateId'
  Prelude.Text ->
  -- | 'certificateBody'
  Prelude.Text ->
  -- | 'status'
  StatusType ->
  SigningCertificate
newSigningCertificate
  pUserName_
  pCertificateId_
  pCertificateBody_
  pStatus_ =
    SigningCertificate'
      { uploadDate = Prelude.Nothing,
        userName = pUserName_,
        certificateId = pCertificateId_,
        certificateBody = pCertificateBody_,
        status = pStatus_
      }

-- | The date when the signing certificate was uploaded.
signingCertificate_uploadDate :: Lens.Lens' SigningCertificate (Prelude.Maybe Prelude.UTCTime)
signingCertificate_uploadDate = Lens.lens (\SigningCertificate' {uploadDate} -> uploadDate) (\s@SigningCertificate' {} a -> s {uploadDate = a} :: SigningCertificate) Prelude.. Lens.mapping Data._Time

-- | The name of the user the signing certificate is associated with.
signingCertificate_userName :: Lens.Lens' SigningCertificate Prelude.Text
signingCertificate_userName = Lens.lens (\SigningCertificate' {userName} -> userName) (\s@SigningCertificate' {} a -> s {userName = a} :: SigningCertificate)

-- | The ID for the signing certificate.
signingCertificate_certificateId :: Lens.Lens' SigningCertificate Prelude.Text
signingCertificate_certificateId = Lens.lens (\SigningCertificate' {certificateId} -> certificateId) (\s@SigningCertificate' {} a -> s {certificateId = a} :: SigningCertificate)

-- | The contents of the signing certificate.
signingCertificate_certificateBody :: Lens.Lens' SigningCertificate Prelude.Text
signingCertificate_certificateBody = Lens.lens (\SigningCertificate' {certificateBody} -> certificateBody) (\s@SigningCertificate' {} a -> s {certificateBody = a} :: SigningCertificate)

-- | The status of the signing certificate. @Active@ means that the key is
-- valid for API calls, while @Inactive@ means it is not.
signingCertificate_status :: Lens.Lens' SigningCertificate StatusType
signingCertificate_status = Lens.lens (\SigningCertificate' {status} -> status) (\s@SigningCertificate' {} a -> s {status = a} :: SigningCertificate)

instance Data.FromXML SigningCertificate where
  parseXML x =
    SigningCertificate'
      Prelude.<$> (x Data..@? "UploadDate")
      Prelude.<*> (x Data..@ "UserName")
      Prelude.<*> (x Data..@ "CertificateId")
      Prelude.<*> (x Data..@ "CertificateBody")
      Prelude.<*> (x Data..@ "Status")

instance Prelude.Hashable SigningCertificate where
  hashWithSalt _salt SigningCertificate' {..} =
    _salt
      `Prelude.hashWithSalt` uploadDate
      `Prelude.hashWithSalt` userName
      `Prelude.hashWithSalt` certificateId
      `Prelude.hashWithSalt` certificateBody
      `Prelude.hashWithSalt` status

instance Prelude.NFData SigningCertificate where
  rnf SigningCertificate' {..} =
    Prelude.rnf uploadDate `Prelude.seq`
      Prelude.rnf userName `Prelude.seq`
        Prelude.rnf certificateId `Prelude.seq`
          Prelude.rnf certificateBody `Prelude.seq`
            Prelude.rnf status
