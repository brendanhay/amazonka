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
-- Module      : Amazonka.IAM.Types.ServerCertificateMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IAM.Types.ServerCertificateMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a server certificate without its certificate
-- body, certificate chain, and private key.
--
-- This data type is used as a response element in the
-- UploadServerCertificate and ListServerCertificates operations.
--
-- /See:/ 'newServerCertificateMetadata' smart constructor.
data ServerCertificateMetadata = ServerCertificateMetadata'
  { -- | The date on which the certificate is set to expire.
    expiration :: Prelude.Maybe Data.ISO8601,
    -- | The date when the server certificate was uploaded.
    uploadDate :: Prelude.Maybe Data.ISO8601,
    -- | The path to the server certificate. For more information about paths,
    -- see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
    -- in the /IAM User Guide/.
    path :: Prelude.Text,
    -- | The name that identifies the server certificate.
    serverCertificateName :: Prelude.Text,
    -- | The stable and unique string identifying the server certificate. For
    -- more information about IDs, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
    -- in the /IAM User Guide/.
    serverCertificateId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) specifying the server certificate. For
    -- more information about ARNs and how to use them in policies, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
    -- in the /IAM User Guide/.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServerCertificateMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expiration', 'serverCertificateMetadata_expiration' - The date on which the certificate is set to expire.
--
-- 'uploadDate', 'serverCertificateMetadata_uploadDate' - The date when the server certificate was uploaded.
--
-- 'path', 'serverCertificateMetadata_path' - The path to the server certificate. For more information about paths,
-- see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
--
-- 'serverCertificateName', 'serverCertificateMetadata_serverCertificateName' - The name that identifies the server certificate.
--
-- 'serverCertificateId', 'serverCertificateMetadata_serverCertificateId' - The stable and unique string identifying the server certificate. For
-- more information about IDs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
--
-- 'arn', 'serverCertificateMetadata_arn' - The Amazon Resource Name (ARN) specifying the server certificate. For
-- more information about ARNs and how to use them in policies, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
newServerCertificateMetadata ::
  -- | 'path'
  Prelude.Text ->
  -- | 'serverCertificateName'
  Prelude.Text ->
  -- | 'serverCertificateId'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  ServerCertificateMetadata
newServerCertificateMetadata
  pPath_
  pServerCertificateName_
  pServerCertificateId_
  pArn_ =
    ServerCertificateMetadata'
      { expiration =
          Prelude.Nothing,
        uploadDate = Prelude.Nothing,
        path = pPath_,
        serverCertificateName = pServerCertificateName_,
        serverCertificateId = pServerCertificateId_,
        arn = pArn_
      }

-- | The date on which the certificate is set to expire.
serverCertificateMetadata_expiration :: Lens.Lens' ServerCertificateMetadata (Prelude.Maybe Prelude.UTCTime)
serverCertificateMetadata_expiration = Lens.lens (\ServerCertificateMetadata' {expiration} -> expiration) (\s@ServerCertificateMetadata' {} a -> s {expiration = a} :: ServerCertificateMetadata) Prelude.. Lens.mapping Data._Time

-- | The date when the server certificate was uploaded.
serverCertificateMetadata_uploadDate :: Lens.Lens' ServerCertificateMetadata (Prelude.Maybe Prelude.UTCTime)
serverCertificateMetadata_uploadDate = Lens.lens (\ServerCertificateMetadata' {uploadDate} -> uploadDate) (\s@ServerCertificateMetadata' {} a -> s {uploadDate = a} :: ServerCertificateMetadata) Prelude.. Lens.mapping Data._Time

-- | The path to the server certificate. For more information about paths,
-- see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
serverCertificateMetadata_path :: Lens.Lens' ServerCertificateMetadata Prelude.Text
serverCertificateMetadata_path = Lens.lens (\ServerCertificateMetadata' {path} -> path) (\s@ServerCertificateMetadata' {} a -> s {path = a} :: ServerCertificateMetadata)

-- | The name that identifies the server certificate.
serverCertificateMetadata_serverCertificateName :: Lens.Lens' ServerCertificateMetadata Prelude.Text
serverCertificateMetadata_serverCertificateName = Lens.lens (\ServerCertificateMetadata' {serverCertificateName} -> serverCertificateName) (\s@ServerCertificateMetadata' {} a -> s {serverCertificateName = a} :: ServerCertificateMetadata)

-- | The stable and unique string identifying the server certificate. For
-- more information about IDs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
serverCertificateMetadata_serverCertificateId :: Lens.Lens' ServerCertificateMetadata Prelude.Text
serverCertificateMetadata_serverCertificateId = Lens.lens (\ServerCertificateMetadata' {serverCertificateId} -> serverCertificateId) (\s@ServerCertificateMetadata' {} a -> s {serverCertificateId = a} :: ServerCertificateMetadata)

-- | The Amazon Resource Name (ARN) specifying the server certificate. For
-- more information about ARNs and how to use them in policies, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
serverCertificateMetadata_arn :: Lens.Lens' ServerCertificateMetadata Prelude.Text
serverCertificateMetadata_arn = Lens.lens (\ServerCertificateMetadata' {arn} -> arn) (\s@ServerCertificateMetadata' {} a -> s {arn = a} :: ServerCertificateMetadata)

instance Data.FromXML ServerCertificateMetadata where
  parseXML x =
    ServerCertificateMetadata'
      Prelude.<$> (x Data..@? "Expiration")
      Prelude.<*> (x Data..@? "UploadDate")
      Prelude.<*> (x Data..@ "Path")
      Prelude.<*> (x Data..@ "ServerCertificateName")
      Prelude.<*> (x Data..@ "ServerCertificateId")
      Prelude.<*> (x Data..@ "Arn")

instance Prelude.Hashable ServerCertificateMetadata where
  hashWithSalt _salt ServerCertificateMetadata' {..} =
    _salt `Prelude.hashWithSalt` expiration
      `Prelude.hashWithSalt` uploadDate
      `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` serverCertificateName
      `Prelude.hashWithSalt` serverCertificateId
      `Prelude.hashWithSalt` arn

instance Prelude.NFData ServerCertificateMetadata where
  rnf ServerCertificateMetadata' {..} =
    Prelude.rnf expiration
      `Prelude.seq` Prelude.rnf uploadDate
      `Prelude.seq` Prelude.rnf path
      `Prelude.seq` Prelude.rnf serverCertificateName
      `Prelude.seq` Prelude.rnf serverCertificateId
      `Prelude.seq` Prelude.rnf arn
