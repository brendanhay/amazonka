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
-- Module      : Amazonka.SSM.Types.DocumentVersionInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.DocumentVersionInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.DocumentFormat
import Amazonka.SSM.Types.DocumentStatus
import Amazonka.SSM.Types.ReviewStatus

-- | Version information about the document.
--
-- /See:/ 'newDocumentVersionInfo' smart constructor.
data DocumentVersionInfo = DocumentVersionInfo'
  { -- | The document name.
    name :: Prelude.Maybe Prelude.Text,
    -- | An identifier for the default version of the document.
    isDefaultVersion :: Prelude.Maybe Prelude.Bool,
    -- | The friendly name of the SSM document. This value can differ for each
    -- version of the document. If you want to update this value, see
    -- UpdateDocument.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The status of the SSM document, such as @Creating@, @Active@, @Failed@,
    -- and @Deleting@.
    status :: Prelude.Maybe DocumentStatus,
    -- | The version of the artifact associated with the document. For example,
    -- \"Release 12, Update 6\". This value is unique across all versions of a
    -- document, and can\'t be changed.
    versionName :: Prelude.Maybe Prelude.Text,
    -- | A message returned by Amazon Web Services Systems Manager that explains
    -- the @Status@ value. For example, a @Failed@ status might be explained by
    -- the @StatusInformation@ message, \"The specified S3 bucket doesn\'t
    -- exist. Verify that the URL of the S3 bucket is correct.\"
    statusInformation :: Prelude.Maybe Prelude.Text,
    -- | The date the document was created.
    createdDate :: Prelude.Maybe Core.POSIX,
    -- | The current status of the approval review for the latest version of the
    -- document.
    reviewStatus :: Prelude.Maybe ReviewStatus,
    -- | The document format, either JSON or YAML.
    documentFormat :: Prelude.Maybe DocumentFormat,
    -- | The document version.
    documentVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DocumentVersionInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'documentVersionInfo_name' - The document name.
--
-- 'isDefaultVersion', 'documentVersionInfo_isDefaultVersion' - An identifier for the default version of the document.
--
-- 'displayName', 'documentVersionInfo_displayName' - The friendly name of the SSM document. This value can differ for each
-- version of the document. If you want to update this value, see
-- UpdateDocument.
--
-- 'status', 'documentVersionInfo_status' - The status of the SSM document, such as @Creating@, @Active@, @Failed@,
-- and @Deleting@.
--
-- 'versionName', 'documentVersionInfo_versionName' - The version of the artifact associated with the document. For example,
-- \"Release 12, Update 6\". This value is unique across all versions of a
-- document, and can\'t be changed.
--
-- 'statusInformation', 'documentVersionInfo_statusInformation' - A message returned by Amazon Web Services Systems Manager that explains
-- the @Status@ value. For example, a @Failed@ status might be explained by
-- the @StatusInformation@ message, \"The specified S3 bucket doesn\'t
-- exist. Verify that the URL of the S3 bucket is correct.\"
--
-- 'createdDate', 'documentVersionInfo_createdDate' - The date the document was created.
--
-- 'reviewStatus', 'documentVersionInfo_reviewStatus' - The current status of the approval review for the latest version of the
-- document.
--
-- 'documentFormat', 'documentVersionInfo_documentFormat' - The document format, either JSON or YAML.
--
-- 'documentVersion', 'documentVersionInfo_documentVersion' - The document version.
newDocumentVersionInfo ::
  DocumentVersionInfo
newDocumentVersionInfo =
  DocumentVersionInfo'
    { name = Prelude.Nothing,
      isDefaultVersion = Prelude.Nothing,
      displayName = Prelude.Nothing,
      status = Prelude.Nothing,
      versionName = Prelude.Nothing,
      statusInformation = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      reviewStatus = Prelude.Nothing,
      documentFormat = Prelude.Nothing,
      documentVersion = Prelude.Nothing
    }

-- | The document name.
documentVersionInfo_name :: Lens.Lens' DocumentVersionInfo (Prelude.Maybe Prelude.Text)
documentVersionInfo_name = Lens.lens (\DocumentVersionInfo' {name} -> name) (\s@DocumentVersionInfo' {} a -> s {name = a} :: DocumentVersionInfo)

-- | An identifier for the default version of the document.
documentVersionInfo_isDefaultVersion :: Lens.Lens' DocumentVersionInfo (Prelude.Maybe Prelude.Bool)
documentVersionInfo_isDefaultVersion = Lens.lens (\DocumentVersionInfo' {isDefaultVersion} -> isDefaultVersion) (\s@DocumentVersionInfo' {} a -> s {isDefaultVersion = a} :: DocumentVersionInfo)

-- | The friendly name of the SSM document. This value can differ for each
-- version of the document. If you want to update this value, see
-- UpdateDocument.
documentVersionInfo_displayName :: Lens.Lens' DocumentVersionInfo (Prelude.Maybe Prelude.Text)
documentVersionInfo_displayName = Lens.lens (\DocumentVersionInfo' {displayName} -> displayName) (\s@DocumentVersionInfo' {} a -> s {displayName = a} :: DocumentVersionInfo)

-- | The status of the SSM document, such as @Creating@, @Active@, @Failed@,
-- and @Deleting@.
documentVersionInfo_status :: Lens.Lens' DocumentVersionInfo (Prelude.Maybe DocumentStatus)
documentVersionInfo_status = Lens.lens (\DocumentVersionInfo' {status} -> status) (\s@DocumentVersionInfo' {} a -> s {status = a} :: DocumentVersionInfo)

-- | The version of the artifact associated with the document. For example,
-- \"Release 12, Update 6\". This value is unique across all versions of a
-- document, and can\'t be changed.
documentVersionInfo_versionName :: Lens.Lens' DocumentVersionInfo (Prelude.Maybe Prelude.Text)
documentVersionInfo_versionName = Lens.lens (\DocumentVersionInfo' {versionName} -> versionName) (\s@DocumentVersionInfo' {} a -> s {versionName = a} :: DocumentVersionInfo)

-- | A message returned by Amazon Web Services Systems Manager that explains
-- the @Status@ value. For example, a @Failed@ status might be explained by
-- the @StatusInformation@ message, \"The specified S3 bucket doesn\'t
-- exist. Verify that the URL of the S3 bucket is correct.\"
documentVersionInfo_statusInformation :: Lens.Lens' DocumentVersionInfo (Prelude.Maybe Prelude.Text)
documentVersionInfo_statusInformation = Lens.lens (\DocumentVersionInfo' {statusInformation} -> statusInformation) (\s@DocumentVersionInfo' {} a -> s {statusInformation = a} :: DocumentVersionInfo)

-- | The date the document was created.
documentVersionInfo_createdDate :: Lens.Lens' DocumentVersionInfo (Prelude.Maybe Prelude.UTCTime)
documentVersionInfo_createdDate = Lens.lens (\DocumentVersionInfo' {createdDate} -> createdDate) (\s@DocumentVersionInfo' {} a -> s {createdDate = a} :: DocumentVersionInfo) Prelude.. Lens.mapping Core._Time

-- | The current status of the approval review for the latest version of the
-- document.
documentVersionInfo_reviewStatus :: Lens.Lens' DocumentVersionInfo (Prelude.Maybe ReviewStatus)
documentVersionInfo_reviewStatus = Lens.lens (\DocumentVersionInfo' {reviewStatus} -> reviewStatus) (\s@DocumentVersionInfo' {} a -> s {reviewStatus = a} :: DocumentVersionInfo)

-- | The document format, either JSON or YAML.
documentVersionInfo_documentFormat :: Lens.Lens' DocumentVersionInfo (Prelude.Maybe DocumentFormat)
documentVersionInfo_documentFormat = Lens.lens (\DocumentVersionInfo' {documentFormat} -> documentFormat) (\s@DocumentVersionInfo' {} a -> s {documentFormat = a} :: DocumentVersionInfo)

-- | The document version.
documentVersionInfo_documentVersion :: Lens.Lens' DocumentVersionInfo (Prelude.Maybe Prelude.Text)
documentVersionInfo_documentVersion = Lens.lens (\DocumentVersionInfo' {documentVersion} -> documentVersion) (\s@DocumentVersionInfo' {} a -> s {documentVersion = a} :: DocumentVersionInfo)

instance Core.FromJSON DocumentVersionInfo where
  parseJSON =
    Core.withObject
      "DocumentVersionInfo"
      ( \x ->
          DocumentVersionInfo'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "IsDefaultVersion")
            Prelude.<*> (x Core..:? "DisplayName")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "VersionName")
            Prelude.<*> (x Core..:? "StatusInformation")
            Prelude.<*> (x Core..:? "CreatedDate")
            Prelude.<*> (x Core..:? "ReviewStatus")
            Prelude.<*> (x Core..:? "DocumentFormat")
            Prelude.<*> (x Core..:? "DocumentVersion")
      )

instance Prelude.Hashable DocumentVersionInfo where
  hashWithSalt _salt DocumentVersionInfo' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` isDefaultVersion
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` versionName
      `Prelude.hashWithSalt` statusInformation
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` reviewStatus
      `Prelude.hashWithSalt` documentFormat
      `Prelude.hashWithSalt` documentVersion

instance Prelude.NFData DocumentVersionInfo where
  rnf DocumentVersionInfo' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf isDefaultVersion
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf versionName
      `Prelude.seq` Prelude.rnf statusInformation
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf reviewStatus
      `Prelude.seq` Prelude.rnf documentFormat
      `Prelude.seq` Prelude.rnf documentVersion
