{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SSM.Types.DocumentVersionInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.DocumentVersionInfo where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSM.Types.DocumentFormat
import Network.AWS.SSM.Types.DocumentStatus
import Network.AWS.SSM.Types.ReviewStatus

-- | Version information about the document.
--
-- /See:/ 'newDocumentVersionInfo' smart constructor.
data DocumentVersionInfo = DocumentVersionInfo'
  { -- | The status of the Systems Manager document, such as @Creating@,
    -- @Active@, @Failed@, and @Deleting@.
    status :: Prelude.Maybe DocumentStatus,
    -- | The date the document was created.
    createdDate :: Prelude.Maybe Prelude.POSIX,
    -- | A message returned by AWS Systems Manager that explains the @Status@
    -- value. For example, a @Failed@ status might be explained by the
    -- @StatusInformation@ message, \"The specified S3 bucket does not exist.
    -- Verify that the URL of the S3 bucket is correct.\"
    statusInformation :: Prelude.Maybe Prelude.Text,
    -- | The version of the artifact associated with the document. For example,
    -- \"Release 12, Update 6\". This value is unique across all versions of a
    -- document, and cannot be changed.
    versionName :: Prelude.Maybe Prelude.Text,
    -- | The document name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The document format, either JSON or YAML.
    documentFormat :: Prelude.Maybe DocumentFormat,
    -- | The current status of the approval review for the latest version of the
    -- document.
    reviewStatus :: Prelude.Maybe ReviewStatus,
    -- | An identifier for the default version of the document.
    isDefaultVersion :: Prelude.Maybe Prelude.Bool,
    -- | The document version.
    documentVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DocumentVersionInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'documentVersionInfo_status' - The status of the Systems Manager document, such as @Creating@,
-- @Active@, @Failed@, and @Deleting@.
--
-- 'createdDate', 'documentVersionInfo_createdDate' - The date the document was created.
--
-- 'statusInformation', 'documentVersionInfo_statusInformation' - A message returned by AWS Systems Manager that explains the @Status@
-- value. For example, a @Failed@ status might be explained by the
-- @StatusInformation@ message, \"The specified S3 bucket does not exist.
-- Verify that the URL of the S3 bucket is correct.\"
--
-- 'versionName', 'documentVersionInfo_versionName' - The version of the artifact associated with the document. For example,
-- \"Release 12, Update 6\". This value is unique across all versions of a
-- document, and cannot be changed.
--
-- 'name', 'documentVersionInfo_name' - The document name.
--
-- 'documentFormat', 'documentVersionInfo_documentFormat' - The document format, either JSON or YAML.
--
-- 'reviewStatus', 'documentVersionInfo_reviewStatus' - The current status of the approval review for the latest version of the
-- document.
--
-- 'isDefaultVersion', 'documentVersionInfo_isDefaultVersion' - An identifier for the default version of the document.
--
-- 'documentVersion', 'documentVersionInfo_documentVersion' - The document version.
newDocumentVersionInfo ::
  DocumentVersionInfo
newDocumentVersionInfo =
  DocumentVersionInfo'
    { status = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      statusInformation = Prelude.Nothing,
      versionName = Prelude.Nothing,
      name = Prelude.Nothing,
      documentFormat = Prelude.Nothing,
      reviewStatus = Prelude.Nothing,
      isDefaultVersion = Prelude.Nothing,
      documentVersion = Prelude.Nothing
    }

-- | The status of the Systems Manager document, such as @Creating@,
-- @Active@, @Failed@, and @Deleting@.
documentVersionInfo_status :: Lens.Lens' DocumentVersionInfo (Prelude.Maybe DocumentStatus)
documentVersionInfo_status = Lens.lens (\DocumentVersionInfo' {status} -> status) (\s@DocumentVersionInfo' {} a -> s {status = a} :: DocumentVersionInfo)

-- | The date the document was created.
documentVersionInfo_createdDate :: Lens.Lens' DocumentVersionInfo (Prelude.Maybe Prelude.UTCTime)
documentVersionInfo_createdDate = Lens.lens (\DocumentVersionInfo' {createdDate} -> createdDate) (\s@DocumentVersionInfo' {} a -> s {createdDate = a} :: DocumentVersionInfo) Prelude.. Lens.mapping Prelude._Time

-- | A message returned by AWS Systems Manager that explains the @Status@
-- value. For example, a @Failed@ status might be explained by the
-- @StatusInformation@ message, \"The specified S3 bucket does not exist.
-- Verify that the URL of the S3 bucket is correct.\"
documentVersionInfo_statusInformation :: Lens.Lens' DocumentVersionInfo (Prelude.Maybe Prelude.Text)
documentVersionInfo_statusInformation = Lens.lens (\DocumentVersionInfo' {statusInformation} -> statusInformation) (\s@DocumentVersionInfo' {} a -> s {statusInformation = a} :: DocumentVersionInfo)

-- | The version of the artifact associated with the document. For example,
-- \"Release 12, Update 6\". This value is unique across all versions of a
-- document, and cannot be changed.
documentVersionInfo_versionName :: Lens.Lens' DocumentVersionInfo (Prelude.Maybe Prelude.Text)
documentVersionInfo_versionName = Lens.lens (\DocumentVersionInfo' {versionName} -> versionName) (\s@DocumentVersionInfo' {} a -> s {versionName = a} :: DocumentVersionInfo)

-- | The document name.
documentVersionInfo_name :: Lens.Lens' DocumentVersionInfo (Prelude.Maybe Prelude.Text)
documentVersionInfo_name = Lens.lens (\DocumentVersionInfo' {name} -> name) (\s@DocumentVersionInfo' {} a -> s {name = a} :: DocumentVersionInfo)

-- | The document format, either JSON or YAML.
documentVersionInfo_documentFormat :: Lens.Lens' DocumentVersionInfo (Prelude.Maybe DocumentFormat)
documentVersionInfo_documentFormat = Lens.lens (\DocumentVersionInfo' {documentFormat} -> documentFormat) (\s@DocumentVersionInfo' {} a -> s {documentFormat = a} :: DocumentVersionInfo)

-- | The current status of the approval review for the latest version of the
-- document.
documentVersionInfo_reviewStatus :: Lens.Lens' DocumentVersionInfo (Prelude.Maybe ReviewStatus)
documentVersionInfo_reviewStatus = Lens.lens (\DocumentVersionInfo' {reviewStatus} -> reviewStatus) (\s@DocumentVersionInfo' {} a -> s {reviewStatus = a} :: DocumentVersionInfo)

-- | An identifier for the default version of the document.
documentVersionInfo_isDefaultVersion :: Lens.Lens' DocumentVersionInfo (Prelude.Maybe Prelude.Bool)
documentVersionInfo_isDefaultVersion = Lens.lens (\DocumentVersionInfo' {isDefaultVersion} -> isDefaultVersion) (\s@DocumentVersionInfo' {} a -> s {isDefaultVersion = a} :: DocumentVersionInfo)

-- | The document version.
documentVersionInfo_documentVersion :: Lens.Lens' DocumentVersionInfo (Prelude.Maybe Prelude.Text)
documentVersionInfo_documentVersion = Lens.lens (\DocumentVersionInfo' {documentVersion} -> documentVersion) (\s@DocumentVersionInfo' {} a -> s {documentVersion = a} :: DocumentVersionInfo)

instance Prelude.FromJSON DocumentVersionInfo where
  parseJSON =
    Prelude.withObject
      "DocumentVersionInfo"
      ( \x ->
          DocumentVersionInfo'
            Prelude.<$> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "CreatedDate")
            Prelude.<*> (x Prelude..:? "StatusInformation")
            Prelude.<*> (x Prelude..:? "VersionName")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "DocumentFormat")
            Prelude.<*> (x Prelude..:? "ReviewStatus")
            Prelude.<*> (x Prelude..:? "IsDefaultVersion")
            Prelude.<*> (x Prelude..:? "DocumentVersion")
      )

instance Prelude.Hashable DocumentVersionInfo

instance Prelude.NFData DocumentVersionInfo
