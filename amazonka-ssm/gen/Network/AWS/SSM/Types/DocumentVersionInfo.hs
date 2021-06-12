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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SSM.Types.DocumentFormat
import Network.AWS.SSM.Types.DocumentStatus
import Network.AWS.SSM.Types.ReviewStatus

-- | Version information about the document.
--
-- /See:/ 'newDocumentVersionInfo' smart constructor.
data DocumentVersionInfo = DocumentVersionInfo'
  { -- | The status of the Systems Manager document, such as @Creating@,
    -- @Active@, @Failed@, and @Deleting@.
    status :: Core.Maybe DocumentStatus,
    -- | The date the document was created.
    createdDate :: Core.Maybe Core.POSIX,
    -- | A message returned by AWS Systems Manager that explains the @Status@
    -- value. For example, a @Failed@ status might be explained by the
    -- @StatusInformation@ message, \"The specified S3 bucket does not exist.
    -- Verify that the URL of the S3 bucket is correct.\"
    statusInformation :: Core.Maybe Core.Text,
    -- | The version of the artifact associated with the document. For example,
    -- \"Release 12, Update 6\". This value is unique across all versions of a
    -- document, and cannot be changed.
    versionName :: Core.Maybe Core.Text,
    -- | The document name.
    name :: Core.Maybe Core.Text,
    -- | The document format, either JSON or YAML.
    documentFormat :: Core.Maybe DocumentFormat,
    -- | The current status of the approval review for the latest version of the
    -- document.
    reviewStatus :: Core.Maybe ReviewStatus,
    -- | An identifier for the default version of the document.
    isDefaultVersion :: Core.Maybe Core.Bool,
    -- | The document version.
    documentVersion :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { status = Core.Nothing,
      createdDate = Core.Nothing,
      statusInformation = Core.Nothing,
      versionName = Core.Nothing,
      name = Core.Nothing,
      documentFormat = Core.Nothing,
      reviewStatus = Core.Nothing,
      isDefaultVersion = Core.Nothing,
      documentVersion = Core.Nothing
    }

-- | The status of the Systems Manager document, such as @Creating@,
-- @Active@, @Failed@, and @Deleting@.
documentVersionInfo_status :: Lens.Lens' DocumentVersionInfo (Core.Maybe DocumentStatus)
documentVersionInfo_status = Lens.lens (\DocumentVersionInfo' {status} -> status) (\s@DocumentVersionInfo' {} a -> s {status = a} :: DocumentVersionInfo)

-- | The date the document was created.
documentVersionInfo_createdDate :: Lens.Lens' DocumentVersionInfo (Core.Maybe Core.UTCTime)
documentVersionInfo_createdDate = Lens.lens (\DocumentVersionInfo' {createdDate} -> createdDate) (\s@DocumentVersionInfo' {} a -> s {createdDate = a} :: DocumentVersionInfo) Core.. Lens.mapping Core._Time

-- | A message returned by AWS Systems Manager that explains the @Status@
-- value. For example, a @Failed@ status might be explained by the
-- @StatusInformation@ message, \"The specified S3 bucket does not exist.
-- Verify that the URL of the S3 bucket is correct.\"
documentVersionInfo_statusInformation :: Lens.Lens' DocumentVersionInfo (Core.Maybe Core.Text)
documentVersionInfo_statusInformation = Lens.lens (\DocumentVersionInfo' {statusInformation} -> statusInformation) (\s@DocumentVersionInfo' {} a -> s {statusInformation = a} :: DocumentVersionInfo)

-- | The version of the artifact associated with the document. For example,
-- \"Release 12, Update 6\". This value is unique across all versions of a
-- document, and cannot be changed.
documentVersionInfo_versionName :: Lens.Lens' DocumentVersionInfo (Core.Maybe Core.Text)
documentVersionInfo_versionName = Lens.lens (\DocumentVersionInfo' {versionName} -> versionName) (\s@DocumentVersionInfo' {} a -> s {versionName = a} :: DocumentVersionInfo)

-- | The document name.
documentVersionInfo_name :: Lens.Lens' DocumentVersionInfo (Core.Maybe Core.Text)
documentVersionInfo_name = Lens.lens (\DocumentVersionInfo' {name} -> name) (\s@DocumentVersionInfo' {} a -> s {name = a} :: DocumentVersionInfo)

-- | The document format, either JSON or YAML.
documentVersionInfo_documentFormat :: Lens.Lens' DocumentVersionInfo (Core.Maybe DocumentFormat)
documentVersionInfo_documentFormat = Lens.lens (\DocumentVersionInfo' {documentFormat} -> documentFormat) (\s@DocumentVersionInfo' {} a -> s {documentFormat = a} :: DocumentVersionInfo)

-- | The current status of the approval review for the latest version of the
-- document.
documentVersionInfo_reviewStatus :: Lens.Lens' DocumentVersionInfo (Core.Maybe ReviewStatus)
documentVersionInfo_reviewStatus = Lens.lens (\DocumentVersionInfo' {reviewStatus} -> reviewStatus) (\s@DocumentVersionInfo' {} a -> s {reviewStatus = a} :: DocumentVersionInfo)

-- | An identifier for the default version of the document.
documentVersionInfo_isDefaultVersion :: Lens.Lens' DocumentVersionInfo (Core.Maybe Core.Bool)
documentVersionInfo_isDefaultVersion = Lens.lens (\DocumentVersionInfo' {isDefaultVersion} -> isDefaultVersion) (\s@DocumentVersionInfo' {} a -> s {isDefaultVersion = a} :: DocumentVersionInfo)

-- | The document version.
documentVersionInfo_documentVersion :: Lens.Lens' DocumentVersionInfo (Core.Maybe Core.Text)
documentVersionInfo_documentVersion = Lens.lens (\DocumentVersionInfo' {documentVersion} -> documentVersion) (\s@DocumentVersionInfo' {} a -> s {documentVersion = a} :: DocumentVersionInfo)

instance Core.FromJSON DocumentVersionInfo where
  parseJSON =
    Core.withObject
      "DocumentVersionInfo"
      ( \x ->
          DocumentVersionInfo'
            Core.<$> (x Core..:? "Status")
            Core.<*> (x Core..:? "CreatedDate")
            Core.<*> (x Core..:? "StatusInformation")
            Core.<*> (x Core..:? "VersionName")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "DocumentFormat")
            Core.<*> (x Core..:? "ReviewStatus")
            Core.<*> (x Core..:? "IsDefaultVersion")
            Core.<*> (x Core..:? "DocumentVersion")
      )

instance Core.Hashable DocumentVersionInfo

instance Core.NFData DocumentVersionInfo
