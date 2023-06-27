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
-- Module      : Amazonka.SSM.Types.DocumentDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.DocumentDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.AttachmentInformation
import Amazonka.SSM.Types.DocumentFormat
import Amazonka.SSM.Types.DocumentHashType
import Amazonka.SSM.Types.DocumentParameter
import Amazonka.SSM.Types.DocumentRequires
import Amazonka.SSM.Types.DocumentStatus
import Amazonka.SSM.Types.DocumentType
import Amazonka.SSM.Types.PlatformType
import Amazonka.SSM.Types.ReviewInformation
import Amazonka.SSM.Types.ReviewStatus
import Amazonka.SSM.Types.Tag

-- | Describes an Amazon Web Services Systems Manager document (SSM
-- document).
--
-- /See:/ 'newDocumentDescription' smart constructor.
data DocumentDescription = DocumentDescription'
  { -- | The version of the document currently approved for use in the
    -- organization.
    approvedVersion :: Prelude.Maybe Prelude.Text,
    -- | Details about the document attachments, including names, locations,
    -- sizes, and so on.
    attachmentsInformation :: Prelude.Maybe [AttachmentInformation],
    -- | The user in your organization who created the document.
    author :: Prelude.Maybe Prelude.Text,
    -- | The classification of a document to help you identify and categorize its
    -- use.
    category :: Prelude.Maybe [Prelude.Text],
    -- | The value that identifies a document\'s category.
    categoryEnum :: Prelude.Maybe [Prelude.Text],
    -- | The date when the document was created.
    createdDate :: Prelude.Maybe Data.POSIX,
    -- | The default version.
    defaultVersion :: Prelude.Maybe Prelude.Text,
    -- | A description of the document.
    description :: Prelude.Maybe Prelude.Text,
    -- | The friendly name of the SSM document. This value can differ for each
    -- version of the document. If you want to update this value, see
    -- UpdateDocument.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The document format, either JSON or YAML.
    documentFormat :: Prelude.Maybe DocumentFormat,
    -- | The type of document.
    documentType :: Prelude.Maybe DocumentType,
    -- | The document version.
    documentVersion :: Prelude.Maybe Prelude.Text,
    -- | The Sha256 or Sha1 hash created by the system when the document was
    -- created.
    --
    -- Sha1 hashes have been deprecated.
    hash :: Prelude.Maybe Prelude.Text,
    -- | The hash type of the document. Valid values include @Sha256@ or @Sha1@.
    --
    -- Sha1 hashes have been deprecated.
    hashType :: Prelude.Maybe DocumentHashType,
    -- | The latest version of the document.
    latestVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the SSM document.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services user that created the document.
    owner :: Prelude.Maybe Prelude.Text,
    -- | A description of the parameters for a document.
    parameters :: Prelude.Maybe [DocumentParameter],
    -- | The version of the document that is currently under review.
    pendingReviewVersion :: Prelude.Maybe Prelude.Text,
    -- | The list of operating system (OS) platforms compatible with this SSM
    -- document.
    platformTypes :: Prelude.Maybe [PlatformType],
    -- | A list of SSM documents required by a document. For example, an
    -- @ApplicationConfiguration@ document requires an
    -- @ApplicationConfigurationSchema@ document.
    requires :: Prelude.Maybe (Prelude.NonEmpty DocumentRequires),
    -- | Details about the review of a document.
    reviewInformation :: Prelude.Maybe (Prelude.NonEmpty ReviewInformation),
    -- | The current status of the review.
    reviewStatus :: Prelude.Maybe ReviewStatus,
    -- | The schema version.
    schemaVersion :: Prelude.Maybe Prelude.Text,
    -- | The SHA1 hash of the document, which you can use for verification.
    sha1 :: Prelude.Maybe Prelude.Text,
    -- | The status of the SSM document.
    status :: Prelude.Maybe DocumentStatus,
    -- | A message returned by Amazon Web Services Systems Manager that explains
    -- the @Status@ value. For example, a @Failed@ status might be explained by
    -- the @StatusInformation@ message, \"The specified S3 bucket doesn\'t
    -- exist. Verify that the URL of the S3 bucket is correct.\"
    statusInformation :: Prelude.Maybe Prelude.Text,
    -- | The tags, or metadata, that have been applied to the document.
    tags :: Prelude.Maybe [Tag],
    -- | The target type which defines the kinds of resources the document can
    -- run on. For example, @\/AWS::EC2::Instance@. For a list of valid
    -- resource types, see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html Amazon Web Services resource and property types reference>
    -- in the /CloudFormation User Guide/.
    targetType :: Prelude.Maybe Prelude.Text,
    -- | The version of the artifact associated with the document.
    versionName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DocumentDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'approvedVersion', 'documentDescription_approvedVersion' - The version of the document currently approved for use in the
-- organization.
--
-- 'attachmentsInformation', 'documentDescription_attachmentsInformation' - Details about the document attachments, including names, locations,
-- sizes, and so on.
--
-- 'author', 'documentDescription_author' - The user in your organization who created the document.
--
-- 'category', 'documentDescription_category' - The classification of a document to help you identify and categorize its
-- use.
--
-- 'categoryEnum', 'documentDescription_categoryEnum' - The value that identifies a document\'s category.
--
-- 'createdDate', 'documentDescription_createdDate' - The date when the document was created.
--
-- 'defaultVersion', 'documentDescription_defaultVersion' - The default version.
--
-- 'description', 'documentDescription_description' - A description of the document.
--
-- 'displayName', 'documentDescription_displayName' - The friendly name of the SSM document. This value can differ for each
-- version of the document. If you want to update this value, see
-- UpdateDocument.
--
-- 'documentFormat', 'documentDescription_documentFormat' - The document format, either JSON or YAML.
--
-- 'documentType', 'documentDescription_documentType' - The type of document.
--
-- 'documentVersion', 'documentDescription_documentVersion' - The document version.
--
-- 'hash', 'documentDescription_hash' - The Sha256 or Sha1 hash created by the system when the document was
-- created.
--
-- Sha1 hashes have been deprecated.
--
-- 'hashType', 'documentDescription_hashType' - The hash type of the document. Valid values include @Sha256@ or @Sha1@.
--
-- Sha1 hashes have been deprecated.
--
-- 'latestVersion', 'documentDescription_latestVersion' - The latest version of the document.
--
-- 'name', 'documentDescription_name' - The name of the SSM document.
--
-- 'owner', 'documentDescription_owner' - The Amazon Web Services user that created the document.
--
-- 'parameters', 'documentDescription_parameters' - A description of the parameters for a document.
--
-- 'pendingReviewVersion', 'documentDescription_pendingReviewVersion' - The version of the document that is currently under review.
--
-- 'platformTypes', 'documentDescription_platformTypes' - The list of operating system (OS) platforms compatible with this SSM
-- document.
--
-- 'requires', 'documentDescription_requires' - A list of SSM documents required by a document. For example, an
-- @ApplicationConfiguration@ document requires an
-- @ApplicationConfigurationSchema@ document.
--
-- 'reviewInformation', 'documentDescription_reviewInformation' - Details about the review of a document.
--
-- 'reviewStatus', 'documentDescription_reviewStatus' - The current status of the review.
--
-- 'schemaVersion', 'documentDescription_schemaVersion' - The schema version.
--
-- 'sha1', 'documentDescription_sha1' - The SHA1 hash of the document, which you can use for verification.
--
-- 'status', 'documentDescription_status' - The status of the SSM document.
--
-- 'statusInformation', 'documentDescription_statusInformation' - A message returned by Amazon Web Services Systems Manager that explains
-- the @Status@ value. For example, a @Failed@ status might be explained by
-- the @StatusInformation@ message, \"The specified S3 bucket doesn\'t
-- exist. Verify that the URL of the S3 bucket is correct.\"
--
-- 'tags', 'documentDescription_tags' - The tags, or metadata, that have been applied to the document.
--
-- 'targetType', 'documentDescription_targetType' - The target type which defines the kinds of resources the document can
-- run on. For example, @\/AWS::EC2::Instance@. For a list of valid
-- resource types, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html Amazon Web Services resource and property types reference>
-- in the /CloudFormation User Guide/.
--
-- 'versionName', 'documentDescription_versionName' - The version of the artifact associated with the document.
newDocumentDescription ::
  DocumentDescription
newDocumentDescription =
  DocumentDescription'
    { approvedVersion =
        Prelude.Nothing,
      attachmentsInformation = Prelude.Nothing,
      author = Prelude.Nothing,
      category = Prelude.Nothing,
      categoryEnum = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      defaultVersion = Prelude.Nothing,
      description = Prelude.Nothing,
      displayName = Prelude.Nothing,
      documentFormat = Prelude.Nothing,
      documentType = Prelude.Nothing,
      documentVersion = Prelude.Nothing,
      hash = Prelude.Nothing,
      hashType = Prelude.Nothing,
      latestVersion = Prelude.Nothing,
      name = Prelude.Nothing,
      owner = Prelude.Nothing,
      parameters = Prelude.Nothing,
      pendingReviewVersion = Prelude.Nothing,
      platformTypes = Prelude.Nothing,
      requires = Prelude.Nothing,
      reviewInformation = Prelude.Nothing,
      reviewStatus = Prelude.Nothing,
      schemaVersion = Prelude.Nothing,
      sha1 = Prelude.Nothing,
      status = Prelude.Nothing,
      statusInformation = Prelude.Nothing,
      tags = Prelude.Nothing,
      targetType = Prelude.Nothing,
      versionName = Prelude.Nothing
    }

-- | The version of the document currently approved for use in the
-- organization.
documentDescription_approvedVersion :: Lens.Lens' DocumentDescription (Prelude.Maybe Prelude.Text)
documentDescription_approvedVersion = Lens.lens (\DocumentDescription' {approvedVersion} -> approvedVersion) (\s@DocumentDescription' {} a -> s {approvedVersion = a} :: DocumentDescription)

-- | Details about the document attachments, including names, locations,
-- sizes, and so on.
documentDescription_attachmentsInformation :: Lens.Lens' DocumentDescription (Prelude.Maybe [AttachmentInformation])
documentDescription_attachmentsInformation = Lens.lens (\DocumentDescription' {attachmentsInformation} -> attachmentsInformation) (\s@DocumentDescription' {} a -> s {attachmentsInformation = a} :: DocumentDescription) Prelude.. Lens.mapping Lens.coerced

-- | The user in your organization who created the document.
documentDescription_author :: Lens.Lens' DocumentDescription (Prelude.Maybe Prelude.Text)
documentDescription_author = Lens.lens (\DocumentDescription' {author} -> author) (\s@DocumentDescription' {} a -> s {author = a} :: DocumentDescription)

-- | The classification of a document to help you identify and categorize its
-- use.
documentDescription_category :: Lens.Lens' DocumentDescription (Prelude.Maybe [Prelude.Text])
documentDescription_category = Lens.lens (\DocumentDescription' {category} -> category) (\s@DocumentDescription' {} a -> s {category = a} :: DocumentDescription) Prelude.. Lens.mapping Lens.coerced

-- | The value that identifies a document\'s category.
documentDescription_categoryEnum :: Lens.Lens' DocumentDescription (Prelude.Maybe [Prelude.Text])
documentDescription_categoryEnum = Lens.lens (\DocumentDescription' {categoryEnum} -> categoryEnum) (\s@DocumentDescription' {} a -> s {categoryEnum = a} :: DocumentDescription) Prelude.. Lens.mapping Lens.coerced

-- | The date when the document was created.
documentDescription_createdDate :: Lens.Lens' DocumentDescription (Prelude.Maybe Prelude.UTCTime)
documentDescription_createdDate = Lens.lens (\DocumentDescription' {createdDate} -> createdDate) (\s@DocumentDescription' {} a -> s {createdDate = a} :: DocumentDescription) Prelude.. Lens.mapping Data._Time

-- | The default version.
documentDescription_defaultVersion :: Lens.Lens' DocumentDescription (Prelude.Maybe Prelude.Text)
documentDescription_defaultVersion = Lens.lens (\DocumentDescription' {defaultVersion} -> defaultVersion) (\s@DocumentDescription' {} a -> s {defaultVersion = a} :: DocumentDescription)

-- | A description of the document.
documentDescription_description :: Lens.Lens' DocumentDescription (Prelude.Maybe Prelude.Text)
documentDescription_description = Lens.lens (\DocumentDescription' {description} -> description) (\s@DocumentDescription' {} a -> s {description = a} :: DocumentDescription)

-- | The friendly name of the SSM document. This value can differ for each
-- version of the document. If you want to update this value, see
-- UpdateDocument.
documentDescription_displayName :: Lens.Lens' DocumentDescription (Prelude.Maybe Prelude.Text)
documentDescription_displayName = Lens.lens (\DocumentDescription' {displayName} -> displayName) (\s@DocumentDescription' {} a -> s {displayName = a} :: DocumentDescription)

-- | The document format, either JSON or YAML.
documentDescription_documentFormat :: Lens.Lens' DocumentDescription (Prelude.Maybe DocumentFormat)
documentDescription_documentFormat = Lens.lens (\DocumentDescription' {documentFormat} -> documentFormat) (\s@DocumentDescription' {} a -> s {documentFormat = a} :: DocumentDescription)

-- | The type of document.
documentDescription_documentType :: Lens.Lens' DocumentDescription (Prelude.Maybe DocumentType)
documentDescription_documentType = Lens.lens (\DocumentDescription' {documentType} -> documentType) (\s@DocumentDescription' {} a -> s {documentType = a} :: DocumentDescription)

-- | The document version.
documentDescription_documentVersion :: Lens.Lens' DocumentDescription (Prelude.Maybe Prelude.Text)
documentDescription_documentVersion = Lens.lens (\DocumentDescription' {documentVersion} -> documentVersion) (\s@DocumentDescription' {} a -> s {documentVersion = a} :: DocumentDescription)

-- | The Sha256 or Sha1 hash created by the system when the document was
-- created.
--
-- Sha1 hashes have been deprecated.
documentDescription_hash :: Lens.Lens' DocumentDescription (Prelude.Maybe Prelude.Text)
documentDescription_hash = Lens.lens (\DocumentDescription' {hash} -> hash) (\s@DocumentDescription' {} a -> s {hash = a} :: DocumentDescription)

-- | The hash type of the document. Valid values include @Sha256@ or @Sha1@.
--
-- Sha1 hashes have been deprecated.
documentDescription_hashType :: Lens.Lens' DocumentDescription (Prelude.Maybe DocumentHashType)
documentDescription_hashType = Lens.lens (\DocumentDescription' {hashType} -> hashType) (\s@DocumentDescription' {} a -> s {hashType = a} :: DocumentDescription)

-- | The latest version of the document.
documentDescription_latestVersion :: Lens.Lens' DocumentDescription (Prelude.Maybe Prelude.Text)
documentDescription_latestVersion = Lens.lens (\DocumentDescription' {latestVersion} -> latestVersion) (\s@DocumentDescription' {} a -> s {latestVersion = a} :: DocumentDescription)

-- | The name of the SSM document.
documentDescription_name :: Lens.Lens' DocumentDescription (Prelude.Maybe Prelude.Text)
documentDescription_name = Lens.lens (\DocumentDescription' {name} -> name) (\s@DocumentDescription' {} a -> s {name = a} :: DocumentDescription)

-- | The Amazon Web Services user that created the document.
documentDescription_owner :: Lens.Lens' DocumentDescription (Prelude.Maybe Prelude.Text)
documentDescription_owner = Lens.lens (\DocumentDescription' {owner} -> owner) (\s@DocumentDescription' {} a -> s {owner = a} :: DocumentDescription)

-- | A description of the parameters for a document.
documentDescription_parameters :: Lens.Lens' DocumentDescription (Prelude.Maybe [DocumentParameter])
documentDescription_parameters = Lens.lens (\DocumentDescription' {parameters} -> parameters) (\s@DocumentDescription' {} a -> s {parameters = a} :: DocumentDescription) Prelude.. Lens.mapping Lens.coerced

-- | The version of the document that is currently under review.
documentDescription_pendingReviewVersion :: Lens.Lens' DocumentDescription (Prelude.Maybe Prelude.Text)
documentDescription_pendingReviewVersion = Lens.lens (\DocumentDescription' {pendingReviewVersion} -> pendingReviewVersion) (\s@DocumentDescription' {} a -> s {pendingReviewVersion = a} :: DocumentDescription)

-- | The list of operating system (OS) platforms compatible with this SSM
-- document.
documentDescription_platformTypes :: Lens.Lens' DocumentDescription (Prelude.Maybe [PlatformType])
documentDescription_platformTypes = Lens.lens (\DocumentDescription' {platformTypes} -> platformTypes) (\s@DocumentDescription' {} a -> s {platformTypes = a} :: DocumentDescription) Prelude.. Lens.mapping Lens.coerced

-- | A list of SSM documents required by a document. For example, an
-- @ApplicationConfiguration@ document requires an
-- @ApplicationConfigurationSchema@ document.
documentDescription_requires :: Lens.Lens' DocumentDescription (Prelude.Maybe (Prelude.NonEmpty DocumentRequires))
documentDescription_requires = Lens.lens (\DocumentDescription' {requires} -> requires) (\s@DocumentDescription' {} a -> s {requires = a} :: DocumentDescription) Prelude.. Lens.mapping Lens.coerced

-- | Details about the review of a document.
documentDescription_reviewInformation :: Lens.Lens' DocumentDescription (Prelude.Maybe (Prelude.NonEmpty ReviewInformation))
documentDescription_reviewInformation = Lens.lens (\DocumentDescription' {reviewInformation} -> reviewInformation) (\s@DocumentDescription' {} a -> s {reviewInformation = a} :: DocumentDescription) Prelude.. Lens.mapping Lens.coerced

-- | The current status of the review.
documentDescription_reviewStatus :: Lens.Lens' DocumentDescription (Prelude.Maybe ReviewStatus)
documentDescription_reviewStatus = Lens.lens (\DocumentDescription' {reviewStatus} -> reviewStatus) (\s@DocumentDescription' {} a -> s {reviewStatus = a} :: DocumentDescription)

-- | The schema version.
documentDescription_schemaVersion :: Lens.Lens' DocumentDescription (Prelude.Maybe Prelude.Text)
documentDescription_schemaVersion = Lens.lens (\DocumentDescription' {schemaVersion} -> schemaVersion) (\s@DocumentDescription' {} a -> s {schemaVersion = a} :: DocumentDescription)

-- | The SHA1 hash of the document, which you can use for verification.
documentDescription_sha1 :: Lens.Lens' DocumentDescription (Prelude.Maybe Prelude.Text)
documentDescription_sha1 = Lens.lens (\DocumentDescription' {sha1} -> sha1) (\s@DocumentDescription' {} a -> s {sha1 = a} :: DocumentDescription)

-- | The status of the SSM document.
documentDescription_status :: Lens.Lens' DocumentDescription (Prelude.Maybe DocumentStatus)
documentDescription_status = Lens.lens (\DocumentDescription' {status} -> status) (\s@DocumentDescription' {} a -> s {status = a} :: DocumentDescription)

-- | A message returned by Amazon Web Services Systems Manager that explains
-- the @Status@ value. For example, a @Failed@ status might be explained by
-- the @StatusInformation@ message, \"The specified S3 bucket doesn\'t
-- exist. Verify that the URL of the S3 bucket is correct.\"
documentDescription_statusInformation :: Lens.Lens' DocumentDescription (Prelude.Maybe Prelude.Text)
documentDescription_statusInformation = Lens.lens (\DocumentDescription' {statusInformation} -> statusInformation) (\s@DocumentDescription' {} a -> s {statusInformation = a} :: DocumentDescription)

-- | The tags, or metadata, that have been applied to the document.
documentDescription_tags :: Lens.Lens' DocumentDescription (Prelude.Maybe [Tag])
documentDescription_tags = Lens.lens (\DocumentDescription' {tags} -> tags) (\s@DocumentDescription' {} a -> s {tags = a} :: DocumentDescription) Prelude.. Lens.mapping Lens.coerced

-- | The target type which defines the kinds of resources the document can
-- run on. For example, @\/AWS::EC2::Instance@. For a list of valid
-- resource types, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html Amazon Web Services resource and property types reference>
-- in the /CloudFormation User Guide/.
documentDescription_targetType :: Lens.Lens' DocumentDescription (Prelude.Maybe Prelude.Text)
documentDescription_targetType = Lens.lens (\DocumentDescription' {targetType} -> targetType) (\s@DocumentDescription' {} a -> s {targetType = a} :: DocumentDescription)

-- | The version of the artifact associated with the document.
documentDescription_versionName :: Lens.Lens' DocumentDescription (Prelude.Maybe Prelude.Text)
documentDescription_versionName = Lens.lens (\DocumentDescription' {versionName} -> versionName) (\s@DocumentDescription' {} a -> s {versionName = a} :: DocumentDescription)

instance Data.FromJSON DocumentDescription where
  parseJSON =
    Data.withObject
      "DocumentDescription"
      ( \x ->
          DocumentDescription'
            Prelude.<$> (x Data..:? "ApprovedVersion")
            Prelude.<*> ( x
                            Data..:? "AttachmentsInformation"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Author")
            Prelude.<*> (x Data..:? "Category" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "CategoryEnum" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "CreatedDate")
            Prelude.<*> (x Data..:? "DefaultVersion")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "DisplayName")
            Prelude.<*> (x Data..:? "DocumentFormat")
            Prelude.<*> (x Data..:? "DocumentType")
            Prelude.<*> (x Data..:? "DocumentVersion")
            Prelude.<*> (x Data..:? "Hash")
            Prelude.<*> (x Data..:? "HashType")
            Prelude.<*> (x Data..:? "LatestVersion")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Owner")
            Prelude.<*> (x Data..:? "Parameters" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "PendingReviewVersion")
            Prelude.<*> (x Data..:? "PlatformTypes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Requires")
            Prelude.<*> (x Data..:? "ReviewInformation")
            Prelude.<*> (x Data..:? "ReviewStatus")
            Prelude.<*> (x Data..:? "SchemaVersion")
            Prelude.<*> (x Data..:? "Sha1")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "StatusInformation")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "TargetType")
            Prelude.<*> (x Data..:? "VersionName")
      )

instance Prelude.Hashable DocumentDescription where
  hashWithSalt _salt DocumentDescription' {..} =
    _salt
      `Prelude.hashWithSalt` approvedVersion
      `Prelude.hashWithSalt` attachmentsInformation
      `Prelude.hashWithSalt` author
      `Prelude.hashWithSalt` category
      `Prelude.hashWithSalt` categoryEnum
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` defaultVersion
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` documentFormat
      `Prelude.hashWithSalt` documentType
      `Prelude.hashWithSalt` documentVersion
      `Prelude.hashWithSalt` hash
      `Prelude.hashWithSalt` hashType
      `Prelude.hashWithSalt` latestVersion
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` pendingReviewVersion
      `Prelude.hashWithSalt` platformTypes
      `Prelude.hashWithSalt` requires
      `Prelude.hashWithSalt` reviewInformation
      `Prelude.hashWithSalt` reviewStatus
      `Prelude.hashWithSalt` schemaVersion
      `Prelude.hashWithSalt` sha1
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusInformation
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` targetType
      `Prelude.hashWithSalt` versionName

instance Prelude.NFData DocumentDescription where
  rnf DocumentDescription' {..} =
    Prelude.rnf approvedVersion
      `Prelude.seq` Prelude.rnf attachmentsInformation
      `Prelude.seq` Prelude.rnf author
      `Prelude.seq` Prelude.rnf category
      `Prelude.seq` Prelude.rnf categoryEnum
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf defaultVersion
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf documentFormat
      `Prelude.seq` Prelude.rnf documentType
      `Prelude.seq` Prelude.rnf documentVersion
      `Prelude.seq` Prelude.rnf hash
      `Prelude.seq` Prelude.rnf hashType
      `Prelude.seq` Prelude.rnf latestVersion
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf pendingReviewVersion
      `Prelude.seq` Prelude.rnf platformTypes
      `Prelude.seq` Prelude.rnf requires
      `Prelude.seq` Prelude.rnf
        reviewInformation
      `Prelude.seq` Prelude.rnf reviewStatus
      `Prelude.seq` Prelude.rnf
        schemaVersion
      `Prelude.seq` Prelude.rnf sha1
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf
        statusInformation
      `Prelude.seq` Prelude.rnf
        tags
      `Prelude.seq` Prelude.rnf
        targetType
      `Prelude.seq` Prelude.rnf
        versionName
