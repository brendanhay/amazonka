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
-- Module      : Amazonka.SSM.Types.DocumentIdentifier
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.DocumentIdentifier where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.DocumentFormat
import Amazonka.SSM.Types.DocumentRequires
import Amazonka.SSM.Types.DocumentType
import Amazonka.SSM.Types.PlatformType
import Amazonka.SSM.Types.ReviewStatus
import Amazonka.SSM.Types.Tag

-- | Describes the name of a SSM document.
--
-- /See:/ 'newDocumentIdentifier' smart constructor.
data DocumentIdentifier = DocumentIdentifier'
  { -- | The tags, or metadata, that have been applied to the document.
    tags :: Prelude.Maybe [Tag],
    -- | A list of SSM documents required by a document. For example, an
    -- @ApplicationConfiguration@ document requires an
    -- @ApplicationConfigurationSchema@ document.
    requires :: Prelude.Maybe (Prelude.NonEmpty DocumentRequires),
    -- | The user in your organization who created the document.
    author :: Prelude.Maybe Prelude.Text,
    -- | The document type.
    documentType :: Prelude.Maybe DocumentType,
    -- | The name of the SSM document.
    name :: Prelude.Maybe Prelude.Text,
    -- | An optional field where you can specify a friendly name for the SSM
    -- document. This value can differ for each version of the document. If you
    -- want to update this value, see UpdateDocument.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services user account that created the document.
    owner :: Prelude.Maybe Prelude.Text,
    -- | The target type which defines the kinds of resources the document can
    -- run on. For example, @\/AWS::EC2::Instance@. For a list of valid
    -- resource types, see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html Amazon Web Services resource and property types reference>
    -- in the /CloudFormation User Guide/.
    targetType :: Prelude.Maybe Prelude.Text,
    -- | An optional field specifying the version of the artifact associated with
    -- the document. For example, \"Release 12, Update 6\". This value is
    -- unique across all versions of a document, and can\'t be changed.
    versionName :: Prelude.Maybe Prelude.Text,
    -- | The operating system platform.
    platformTypes :: Prelude.Maybe [PlatformType],
    -- | The schema version.
    schemaVersion :: Prelude.Maybe Prelude.Text,
    -- | The date the SSM document was created.
    createdDate :: Prelude.Maybe Core.POSIX,
    -- | The current status of a document review.
    reviewStatus :: Prelude.Maybe ReviewStatus,
    -- | The document format, either JSON or YAML.
    documentFormat :: Prelude.Maybe DocumentFormat,
    -- | The document version.
    documentVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DocumentIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'documentIdentifier_tags' - The tags, or metadata, that have been applied to the document.
--
-- 'requires', 'documentIdentifier_requires' - A list of SSM documents required by a document. For example, an
-- @ApplicationConfiguration@ document requires an
-- @ApplicationConfigurationSchema@ document.
--
-- 'author', 'documentIdentifier_author' - The user in your organization who created the document.
--
-- 'documentType', 'documentIdentifier_documentType' - The document type.
--
-- 'name', 'documentIdentifier_name' - The name of the SSM document.
--
-- 'displayName', 'documentIdentifier_displayName' - An optional field where you can specify a friendly name for the SSM
-- document. This value can differ for each version of the document. If you
-- want to update this value, see UpdateDocument.
--
-- 'owner', 'documentIdentifier_owner' - The Amazon Web Services user account that created the document.
--
-- 'targetType', 'documentIdentifier_targetType' - The target type which defines the kinds of resources the document can
-- run on. For example, @\/AWS::EC2::Instance@. For a list of valid
-- resource types, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html Amazon Web Services resource and property types reference>
-- in the /CloudFormation User Guide/.
--
-- 'versionName', 'documentIdentifier_versionName' - An optional field specifying the version of the artifact associated with
-- the document. For example, \"Release 12, Update 6\". This value is
-- unique across all versions of a document, and can\'t be changed.
--
-- 'platformTypes', 'documentIdentifier_platformTypes' - The operating system platform.
--
-- 'schemaVersion', 'documentIdentifier_schemaVersion' - The schema version.
--
-- 'createdDate', 'documentIdentifier_createdDate' - The date the SSM document was created.
--
-- 'reviewStatus', 'documentIdentifier_reviewStatus' - The current status of a document review.
--
-- 'documentFormat', 'documentIdentifier_documentFormat' - The document format, either JSON or YAML.
--
-- 'documentVersion', 'documentIdentifier_documentVersion' - The document version.
newDocumentIdentifier ::
  DocumentIdentifier
newDocumentIdentifier =
  DocumentIdentifier'
    { tags = Prelude.Nothing,
      requires = Prelude.Nothing,
      author = Prelude.Nothing,
      documentType = Prelude.Nothing,
      name = Prelude.Nothing,
      displayName = Prelude.Nothing,
      owner = Prelude.Nothing,
      targetType = Prelude.Nothing,
      versionName = Prelude.Nothing,
      platformTypes = Prelude.Nothing,
      schemaVersion = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      reviewStatus = Prelude.Nothing,
      documentFormat = Prelude.Nothing,
      documentVersion = Prelude.Nothing
    }

-- | The tags, or metadata, that have been applied to the document.
documentIdentifier_tags :: Lens.Lens' DocumentIdentifier (Prelude.Maybe [Tag])
documentIdentifier_tags = Lens.lens (\DocumentIdentifier' {tags} -> tags) (\s@DocumentIdentifier' {} a -> s {tags = a} :: DocumentIdentifier) Prelude.. Lens.mapping Lens.coerced

-- | A list of SSM documents required by a document. For example, an
-- @ApplicationConfiguration@ document requires an
-- @ApplicationConfigurationSchema@ document.
documentIdentifier_requires :: Lens.Lens' DocumentIdentifier (Prelude.Maybe (Prelude.NonEmpty DocumentRequires))
documentIdentifier_requires = Lens.lens (\DocumentIdentifier' {requires} -> requires) (\s@DocumentIdentifier' {} a -> s {requires = a} :: DocumentIdentifier) Prelude.. Lens.mapping Lens.coerced

-- | The user in your organization who created the document.
documentIdentifier_author :: Lens.Lens' DocumentIdentifier (Prelude.Maybe Prelude.Text)
documentIdentifier_author = Lens.lens (\DocumentIdentifier' {author} -> author) (\s@DocumentIdentifier' {} a -> s {author = a} :: DocumentIdentifier)

-- | The document type.
documentIdentifier_documentType :: Lens.Lens' DocumentIdentifier (Prelude.Maybe DocumentType)
documentIdentifier_documentType = Lens.lens (\DocumentIdentifier' {documentType} -> documentType) (\s@DocumentIdentifier' {} a -> s {documentType = a} :: DocumentIdentifier)

-- | The name of the SSM document.
documentIdentifier_name :: Lens.Lens' DocumentIdentifier (Prelude.Maybe Prelude.Text)
documentIdentifier_name = Lens.lens (\DocumentIdentifier' {name} -> name) (\s@DocumentIdentifier' {} a -> s {name = a} :: DocumentIdentifier)

-- | An optional field where you can specify a friendly name for the SSM
-- document. This value can differ for each version of the document. If you
-- want to update this value, see UpdateDocument.
documentIdentifier_displayName :: Lens.Lens' DocumentIdentifier (Prelude.Maybe Prelude.Text)
documentIdentifier_displayName = Lens.lens (\DocumentIdentifier' {displayName} -> displayName) (\s@DocumentIdentifier' {} a -> s {displayName = a} :: DocumentIdentifier)

-- | The Amazon Web Services user account that created the document.
documentIdentifier_owner :: Lens.Lens' DocumentIdentifier (Prelude.Maybe Prelude.Text)
documentIdentifier_owner = Lens.lens (\DocumentIdentifier' {owner} -> owner) (\s@DocumentIdentifier' {} a -> s {owner = a} :: DocumentIdentifier)

-- | The target type which defines the kinds of resources the document can
-- run on. For example, @\/AWS::EC2::Instance@. For a list of valid
-- resource types, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html Amazon Web Services resource and property types reference>
-- in the /CloudFormation User Guide/.
documentIdentifier_targetType :: Lens.Lens' DocumentIdentifier (Prelude.Maybe Prelude.Text)
documentIdentifier_targetType = Lens.lens (\DocumentIdentifier' {targetType} -> targetType) (\s@DocumentIdentifier' {} a -> s {targetType = a} :: DocumentIdentifier)

-- | An optional field specifying the version of the artifact associated with
-- the document. For example, \"Release 12, Update 6\". This value is
-- unique across all versions of a document, and can\'t be changed.
documentIdentifier_versionName :: Lens.Lens' DocumentIdentifier (Prelude.Maybe Prelude.Text)
documentIdentifier_versionName = Lens.lens (\DocumentIdentifier' {versionName} -> versionName) (\s@DocumentIdentifier' {} a -> s {versionName = a} :: DocumentIdentifier)

-- | The operating system platform.
documentIdentifier_platformTypes :: Lens.Lens' DocumentIdentifier (Prelude.Maybe [PlatformType])
documentIdentifier_platformTypes = Lens.lens (\DocumentIdentifier' {platformTypes} -> platformTypes) (\s@DocumentIdentifier' {} a -> s {platformTypes = a} :: DocumentIdentifier) Prelude.. Lens.mapping Lens.coerced

-- | The schema version.
documentIdentifier_schemaVersion :: Lens.Lens' DocumentIdentifier (Prelude.Maybe Prelude.Text)
documentIdentifier_schemaVersion = Lens.lens (\DocumentIdentifier' {schemaVersion} -> schemaVersion) (\s@DocumentIdentifier' {} a -> s {schemaVersion = a} :: DocumentIdentifier)

-- | The date the SSM document was created.
documentIdentifier_createdDate :: Lens.Lens' DocumentIdentifier (Prelude.Maybe Prelude.UTCTime)
documentIdentifier_createdDate = Lens.lens (\DocumentIdentifier' {createdDate} -> createdDate) (\s@DocumentIdentifier' {} a -> s {createdDate = a} :: DocumentIdentifier) Prelude.. Lens.mapping Core._Time

-- | The current status of a document review.
documentIdentifier_reviewStatus :: Lens.Lens' DocumentIdentifier (Prelude.Maybe ReviewStatus)
documentIdentifier_reviewStatus = Lens.lens (\DocumentIdentifier' {reviewStatus} -> reviewStatus) (\s@DocumentIdentifier' {} a -> s {reviewStatus = a} :: DocumentIdentifier)

-- | The document format, either JSON or YAML.
documentIdentifier_documentFormat :: Lens.Lens' DocumentIdentifier (Prelude.Maybe DocumentFormat)
documentIdentifier_documentFormat = Lens.lens (\DocumentIdentifier' {documentFormat} -> documentFormat) (\s@DocumentIdentifier' {} a -> s {documentFormat = a} :: DocumentIdentifier)

-- | The document version.
documentIdentifier_documentVersion :: Lens.Lens' DocumentIdentifier (Prelude.Maybe Prelude.Text)
documentIdentifier_documentVersion = Lens.lens (\DocumentIdentifier' {documentVersion} -> documentVersion) (\s@DocumentIdentifier' {} a -> s {documentVersion = a} :: DocumentIdentifier)

instance Core.FromJSON DocumentIdentifier where
  parseJSON =
    Core.withObject
      "DocumentIdentifier"
      ( \x ->
          DocumentIdentifier'
            Prelude.<$> (x Core..:? "Tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Requires")
            Prelude.<*> (x Core..:? "Author")
            Prelude.<*> (x Core..:? "DocumentType")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "DisplayName")
            Prelude.<*> (x Core..:? "Owner")
            Prelude.<*> (x Core..:? "TargetType")
            Prelude.<*> (x Core..:? "VersionName")
            Prelude.<*> (x Core..:? "PlatformTypes" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "SchemaVersion")
            Prelude.<*> (x Core..:? "CreatedDate")
            Prelude.<*> (x Core..:? "ReviewStatus")
            Prelude.<*> (x Core..:? "DocumentFormat")
            Prelude.<*> (x Core..:? "DocumentVersion")
      )

instance Prelude.Hashable DocumentIdentifier where
  hashWithSalt _salt DocumentIdentifier' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` requires
      `Prelude.hashWithSalt` author
      `Prelude.hashWithSalt` documentType
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` targetType
      `Prelude.hashWithSalt` versionName
      `Prelude.hashWithSalt` platformTypes
      `Prelude.hashWithSalt` schemaVersion
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` reviewStatus
      `Prelude.hashWithSalt` documentFormat
      `Prelude.hashWithSalt` documentVersion

instance Prelude.NFData DocumentIdentifier where
  rnf DocumentIdentifier' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf requires
      `Prelude.seq` Prelude.rnf author
      `Prelude.seq` Prelude.rnf documentType
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf targetType
      `Prelude.seq` Prelude.rnf versionName
      `Prelude.seq` Prelude.rnf platformTypes
      `Prelude.seq` Prelude.rnf schemaVersion
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf reviewStatus
      `Prelude.seq` Prelude.rnf documentFormat
      `Prelude.seq` Prelude.rnf documentVersion
