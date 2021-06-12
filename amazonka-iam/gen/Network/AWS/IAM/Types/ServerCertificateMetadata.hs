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
-- Module      : Network.AWS.IAM.Types.ServerCertificateMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.ServerCertificateMetadata where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains information about a server certificate without its certificate
-- body, certificate chain, and private key.
--
-- This data type is used as a response element in the
-- UploadServerCertificate and ListServerCertificates operations.
--
-- /See:/ 'newServerCertificateMetadata' smart constructor.
data ServerCertificateMetadata = ServerCertificateMetadata'
  { -- | The date when the server certificate was uploaded.
    uploadDate :: Core.Maybe Core.ISO8601,
    -- | The date on which the certificate is set to expire.
    expiration :: Core.Maybe Core.ISO8601,
    -- | The path to the server certificate. For more information about paths,
    -- see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
    -- in the /IAM User Guide/.
    path :: Core.Text,
    -- | The name that identifies the server certificate.
    serverCertificateName :: Core.Text,
    -- | The stable and unique string identifying the server certificate. For
    -- more information about IDs, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
    -- in the /IAM User Guide/.
    serverCertificateId :: Core.Text,
    -- | The Amazon Resource Name (ARN) specifying the server certificate. For
    -- more information about ARNs and how to use them in policies, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
    -- in the /IAM User Guide/.
    arn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ServerCertificateMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'uploadDate', 'serverCertificateMetadata_uploadDate' - The date when the server certificate was uploaded.
--
-- 'expiration', 'serverCertificateMetadata_expiration' - The date on which the certificate is set to expire.
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
  Core.Text ->
  -- | 'serverCertificateName'
  Core.Text ->
  -- | 'serverCertificateId'
  Core.Text ->
  -- | 'arn'
  Core.Text ->
  ServerCertificateMetadata
newServerCertificateMetadata
  pPath_
  pServerCertificateName_
  pServerCertificateId_
  pArn_ =
    ServerCertificateMetadata'
      { uploadDate =
          Core.Nothing,
        expiration = Core.Nothing,
        path = pPath_,
        serverCertificateName = pServerCertificateName_,
        serverCertificateId = pServerCertificateId_,
        arn = pArn_
      }

-- | The date when the server certificate was uploaded.
serverCertificateMetadata_uploadDate :: Lens.Lens' ServerCertificateMetadata (Core.Maybe Core.UTCTime)
serverCertificateMetadata_uploadDate = Lens.lens (\ServerCertificateMetadata' {uploadDate} -> uploadDate) (\s@ServerCertificateMetadata' {} a -> s {uploadDate = a} :: ServerCertificateMetadata) Core.. Lens.mapping Core._Time

-- | The date on which the certificate is set to expire.
serverCertificateMetadata_expiration :: Lens.Lens' ServerCertificateMetadata (Core.Maybe Core.UTCTime)
serverCertificateMetadata_expiration = Lens.lens (\ServerCertificateMetadata' {expiration} -> expiration) (\s@ServerCertificateMetadata' {} a -> s {expiration = a} :: ServerCertificateMetadata) Core.. Lens.mapping Core._Time

-- | The path to the server certificate. For more information about paths,
-- see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
serverCertificateMetadata_path :: Lens.Lens' ServerCertificateMetadata Core.Text
serverCertificateMetadata_path = Lens.lens (\ServerCertificateMetadata' {path} -> path) (\s@ServerCertificateMetadata' {} a -> s {path = a} :: ServerCertificateMetadata)

-- | The name that identifies the server certificate.
serverCertificateMetadata_serverCertificateName :: Lens.Lens' ServerCertificateMetadata Core.Text
serverCertificateMetadata_serverCertificateName = Lens.lens (\ServerCertificateMetadata' {serverCertificateName} -> serverCertificateName) (\s@ServerCertificateMetadata' {} a -> s {serverCertificateName = a} :: ServerCertificateMetadata)

-- | The stable and unique string identifying the server certificate. For
-- more information about IDs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
serverCertificateMetadata_serverCertificateId :: Lens.Lens' ServerCertificateMetadata Core.Text
serverCertificateMetadata_serverCertificateId = Lens.lens (\ServerCertificateMetadata' {serverCertificateId} -> serverCertificateId) (\s@ServerCertificateMetadata' {} a -> s {serverCertificateId = a} :: ServerCertificateMetadata)

-- | The Amazon Resource Name (ARN) specifying the server certificate. For
-- more information about ARNs and how to use them in policies, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
serverCertificateMetadata_arn :: Lens.Lens' ServerCertificateMetadata Core.Text
serverCertificateMetadata_arn = Lens.lens (\ServerCertificateMetadata' {arn} -> arn) (\s@ServerCertificateMetadata' {} a -> s {arn = a} :: ServerCertificateMetadata)

instance Core.FromXML ServerCertificateMetadata where
  parseXML x =
    ServerCertificateMetadata'
      Core.<$> (x Core..@? "UploadDate")
      Core.<*> (x Core..@? "Expiration")
      Core.<*> (x Core..@ "Path")
      Core.<*> (x Core..@ "ServerCertificateName")
      Core.<*> (x Core..@ "ServerCertificateId")
      Core.<*> (x Core..@ "Arn")

instance Core.Hashable ServerCertificateMetadata

instance Core.NFData ServerCertificateMetadata
