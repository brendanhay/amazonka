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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.DocumentDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
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

-- | Describes a Amazon Web Services Systems Manager document (SSM document).
--
-- /See:/ 'newDocumentDescription' smart constructor.
data DocumentDescription = DocumentDescription'
  { -- | The status of the SSM document.
    status :: Prelude.Maybe DocumentStatus,
    -- | The type of document.
    documentType :: Prelude.Maybe DocumentType,
    -- | The Sha256 or Sha1 hash created by the system when the document was
    -- created.
    --
    -- Sha1 hashes have been deprecated.
    hash :: Prelude.Maybe Prelude.Text,
    -- | The version of the artifact associated with the document.
    versionName :: Prelude.Maybe Prelude.Text,
    -- | The schema version.
    schemaVersion :: Prelude.Maybe Prelude.Text,
    -- | The SHA1 hash of the document, which you can use for verification.
    sha1 :: Prelude.Maybe Prelude.Text,
    -- | The current status of the review.
    reviewStatus :: Prelude.Maybe ReviewStatus,
    -- | Details about the document attachments, including names, locations,
    -- sizes, and so on.
    attachmentsInformation :: Prelude.Maybe [AttachmentInformation],
    -- | The default version.
    defaultVersion :: Prelude.Maybe Prelude.Text,
    -- | The target type which defines the kinds of resources the document can
    -- run on. For example, @\/AWS::EC2::Instance@. For a list of valid
    -- resource types, see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html Amazon Web Services resource and property types reference>
    -- in the /CloudFormation User Guide/.
    targetType :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services user account that created the document.
    owner :: Prelude.Maybe Prelude.Text,
    -- | The list of OS platforms compatible with this SSM document.
    platformTypes :: Prelude.Maybe [PlatformType],
    -- | The date when the document was created.
    createdDate :: Prelude.Maybe Core.POSIX,
    -- | The document format, either JSON or YAML.
    documentFormat :: Prelude.Maybe DocumentFormat,
    -- | The version of the document that is currently under review.
    pendingReviewVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the SSM document.
    name :: Prelude.Maybe Prelude.Text,
    -- | The hash type of the document. Valid values include @Sha256@ or @Sha1@.
    --
    -- Sha1 hashes have been deprecated.
    hashType :: Prelude.Maybe DocumentHashType,
    -- | A description of the parameters for a document.
    parameters :: Prelude.Maybe [DocumentParameter],
    -- | The document version.
    documentVersion :: Prelude.Maybe Prelude.Text,
    -- | The user in your organization who created the document.
    author :: Prelude.Maybe Prelude.Text,
    -- | The friendly name of the SSM document. This value can differ for each
    -- version of the document. If you want to update this value, see
    -- UpdateDocument.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | A message returned by Amazon Web Services Systems Manager that explains
    -- the @Status@ value. For example, a @Failed@ status might be explained by
    -- the @StatusInformation@ message, \"The specified S3 bucket doesn\'t
    -- exist. Verify that the URL of the S3 bucket is correct.\"
    statusInformation :: Prelude.Maybe Prelude.Text,
    -- | A description of the document.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of SSM documents required by a document. For example, an
    -- @ApplicationConfiguration@ document requires an
    -- @ApplicationConfigurationSchema@ document.
    requires :: Prelude.Maybe (Prelude.NonEmpty DocumentRequires),
    -- | Details about the review of a document.
    reviewInformation :: Prelude.Maybe (Prelude.NonEmpty ReviewInformation),
    -- | The tags, or metadata, that have been applied to the document.
    tags :: Prelude.Maybe [Tag],
    -- | The latest version of the document.
    latestVersion :: Prelude.Maybe Prelude.Text,
    -- | The version of the document currently approved for use in the
    -- organization.
    approvedVersion :: Prelude.Maybe Prelude.Text
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
-- 'status', 'documentDescription_status' - The status of the SSM document.
--
-- 'documentType', 'documentDescription_documentType' - The type of document.
--
-- 'hash', 'documentDescription_hash' - The Sha256 or Sha1 hash created by the system when the document was
-- created.
--
-- Sha1 hashes have been deprecated.
--
-- 'versionName', 'documentDescription_versionName' - The version of the artifact associated with the document.
--
-- 'schemaVersion', 'documentDescription_schemaVersion' - The schema version.
--
-- 'sha1', 'documentDescription_sha1' - The SHA1 hash of the document, which you can use for verification.
--
-- 'reviewStatus', 'documentDescription_reviewStatus' - The current status of the review.
--
-- 'attachmentsInformation', 'documentDescription_attachmentsInformation' - Details about the document attachments, including names, locations,
-- sizes, and so on.
--
-- 'defaultVersion', 'documentDescription_defaultVersion' - The default version.
--
-- 'targetType', 'documentDescription_targetType' - The target type which defines the kinds of resources the document can
-- run on. For example, @\/AWS::EC2::Instance@. For a list of valid
-- resource types, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html Amazon Web Services resource and property types reference>
-- in the /CloudFormation User Guide/.
--
-- 'owner', 'documentDescription_owner' - The Amazon Web Services user account that created the document.
--
-- 'platformTypes', 'documentDescription_platformTypes' - The list of OS platforms compatible with this SSM document.
--
-- 'createdDate', 'documentDescription_createdDate' - The date when the document was created.
--
-- 'documentFormat', 'documentDescription_documentFormat' - The document format, either JSON or YAML.
--
-- 'pendingReviewVersion', 'documentDescription_pendingReviewVersion' - The version of the document that is currently under review.
--
-- 'name', 'documentDescription_name' - The name of the SSM document.
--
-- 'hashType', 'documentDescription_hashType' - The hash type of the document. Valid values include @Sha256@ or @Sha1@.
--
-- Sha1 hashes have been deprecated.
--
-- 'parameters', 'documentDescription_parameters' - A description of the parameters for a document.
--
-- 'documentVersion', 'documentDescription_documentVersion' - The document version.
--
-- 'author', 'documentDescription_author' - The user in your organization who created the document.
--
-- 'displayName', 'documentDescription_displayName' - The friendly name of the SSM document. This value can differ for each
-- version of the document. If you want to update this value, see
-- UpdateDocument.
--
-- 'statusInformation', 'documentDescription_statusInformation' - A message returned by Amazon Web Services Systems Manager that explains
-- the @Status@ value. For example, a @Failed@ status might be explained by
-- the @StatusInformation@ message, \"The specified S3 bucket doesn\'t
-- exist. Verify that the URL of the S3 bucket is correct.\"
--
-- 'description', 'documentDescription_description' - A description of the document.
--
-- 'requires', 'documentDescription_requires' - A list of SSM documents required by a document. For example, an
-- @ApplicationConfiguration@ document requires an
-- @ApplicationConfigurationSchema@ document.
--
-- 'reviewInformation', 'documentDescription_reviewInformation' - Details about the review of a document.
--
-- 'tags', 'documentDescription_tags' - The tags, or metadata, that have been applied to the document.
--
-- 'latestVersion', 'documentDescription_latestVersion' - The latest version of the document.
--
-- 'approvedVersion', 'documentDescription_approvedVersion' - The version of the document currently approved for use in the
-- organization.
newDocumentDescription ::
  DocumentDescription
newDocumentDescription =
  DocumentDescription'
    { status = Prelude.Nothing,
      documentType = Prelude.Nothing,
      hash = Prelude.Nothing,
      versionName = Prelude.Nothing,
      schemaVersion = Prelude.Nothing,
      sha1 = Prelude.Nothing,
      reviewStatus = Prelude.Nothing,
      attachmentsInformation = Prelude.Nothing,
      defaultVersion = Prelude.Nothing,
      targetType = Prelude.Nothing,
      owner = Prelude.Nothing,
      platformTypes = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      documentFormat = Prelude.Nothing,
      pendingReviewVersion = Prelude.Nothing,
      name = Prelude.Nothing,
      hashType = Prelude.Nothing,
      parameters = Prelude.Nothing,
      documentVersion = Prelude.Nothing,
      author = Prelude.Nothing,
      displayName = Prelude.Nothing,
      statusInformation = Prelude.Nothing,
      description = Prelude.Nothing,
      requires = Prelude.Nothing,
      reviewInformation = Prelude.Nothing,
      tags = Prelude.Nothing,
      latestVersion = Prelude.Nothing,
      approvedVersion = Prelude.Nothing
    }

-- | The status of the SSM document.
documentDescription_status :: Lens.Lens' DocumentDescription (Prelude.Maybe DocumentStatus)
documentDescription_status = Lens.lens (\DocumentDescription' {status} -> status) (\s@DocumentDescription' {} a -> s {status = a} :: DocumentDescription)

-- | The type of document.
documentDescription_documentType :: Lens.Lens' DocumentDescription (Prelude.Maybe DocumentType)
documentDescription_documentType = Lens.lens (\DocumentDescription' {documentType} -> documentType) (\s@DocumentDescription' {} a -> s {documentType = a} :: DocumentDescription)

-- | The Sha256 or Sha1 hash created by the system when the document was
-- created.
--
-- Sha1 hashes have been deprecated.
documentDescription_hash :: Lens.Lens' DocumentDescription (Prelude.Maybe Prelude.Text)
documentDescription_hash = Lens.lens (\DocumentDescription' {hash} -> hash) (\s@DocumentDescription' {} a -> s {hash = a} :: DocumentDescription)

-- | The version of the artifact associated with the document.
documentDescription_versionName :: Lens.Lens' DocumentDescription (Prelude.Maybe Prelude.Text)
documentDescription_versionName = Lens.lens (\DocumentDescription' {versionName} -> versionName) (\s@DocumentDescription' {} a -> s {versionName = a} :: DocumentDescription)

-- | The schema version.
documentDescription_schemaVersion :: Lens.Lens' DocumentDescription (Prelude.Maybe Prelude.Text)
documentDescription_schemaVersion = Lens.lens (\DocumentDescription' {schemaVersion} -> schemaVersion) (\s@DocumentDescription' {} a -> s {schemaVersion = a} :: DocumentDescription)

-- | The SHA1 hash of the document, which you can use for verification.
documentDescription_sha1 :: Lens.Lens' DocumentDescription (Prelude.Maybe Prelude.Text)
documentDescription_sha1 = Lens.lens (\DocumentDescription' {sha1} -> sha1) (\s@DocumentDescription' {} a -> s {sha1 = a} :: DocumentDescription)

-- | The current status of the review.
documentDescription_reviewStatus :: Lens.Lens' DocumentDescription (Prelude.Maybe ReviewStatus)
documentDescription_reviewStatus = Lens.lens (\DocumentDescription' {reviewStatus} -> reviewStatus) (\s@DocumentDescription' {} a -> s {reviewStatus = a} :: DocumentDescription)

-- | Details about the document attachments, including names, locations,
-- sizes, and so on.
documentDescription_attachmentsInformation :: Lens.Lens' DocumentDescription (Prelude.Maybe [AttachmentInformation])
documentDescription_attachmentsInformation = Lens.lens (\DocumentDescription' {attachmentsInformation} -> attachmentsInformation) (\s@DocumentDescription' {} a -> s {attachmentsInformation = a} :: DocumentDescription) Prelude.. Lens.mapping Lens.coerced

-- | The default version.
documentDescription_defaultVersion :: Lens.Lens' DocumentDescription (Prelude.Maybe Prelude.Text)
documentDescription_defaultVersion = Lens.lens (\DocumentDescription' {defaultVersion} -> defaultVersion) (\s@DocumentDescription' {} a -> s {defaultVersion = a} :: DocumentDescription)

-- | The target type which defines the kinds of resources the document can
-- run on. For example, @\/AWS::EC2::Instance@. For a list of valid
-- resource types, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html Amazon Web Services resource and property types reference>
-- in the /CloudFormation User Guide/.
documentDescription_targetType :: Lens.Lens' DocumentDescription (Prelude.Maybe Prelude.Text)
documentDescription_targetType = Lens.lens (\DocumentDescription' {targetType} -> targetType) (\s@DocumentDescription' {} a -> s {targetType = a} :: DocumentDescription)

-- | The Amazon Web Services user account that created the document.
documentDescription_owner :: Lens.Lens' DocumentDescription (Prelude.Maybe Prelude.Text)
documentDescription_owner = Lens.lens (\DocumentDescription' {owner} -> owner) (\s@DocumentDescription' {} a -> s {owner = a} :: DocumentDescription)

-- | The list of OS platforms compatible with this SSM document.
documentDescription_platformTypes :: Lens.Lens' DocumentDescription (Prelude.Maybe [PlatformType])
documentDescription_platformTypes = Lens.lens (\DocumentDescription' {platformTypes} -> platformTypes) (\s@DocumentDescription' {} a -> s {platformTypes = a} :: DocumentDescription) Prelude.. Lens.mapping Lens.coerced

-- | The date when the document was created.
documentDescription_createdDate :: Lens.Lens' DocumentDescription (Prelude.Maybe Prelude.UTCTime)
documentDescription_createdDate = Lens.lens (\DocumentDescription' {createdDate} -> createdDate) (\s@DocumentDescription' {} a -> s {createdDate = a} :: DocumentDescription) Prelude.. Lens.mapping Core._Time

-- | The document format, either JSON or YAML.
documentDescription_documentFormat :: Lens.Lens' DocumentDescription (Prelude.Maybe DocumentFormat)
documentDescription_documentFormat = Lens.lens (\DocumentDescription' {documentFormat} -> documentFormat) (\s@DocumentDescription' {} a -> s {documentFormat = a} :: DocumentDescription)

-- | The version of the document that is currently under review.
documentDescription_pendingReviewVersion :: Lens.Lens' DocumentDescription (Prelude.Maybe Prelude.Text)
documentDescription_pendingReviewVersion = Lens.lens (\DocumentDescription' {pendingReviewVersion} -> pendingReviewVersion) (\s@DocumentDescription' {} a -> s {pendingReviewVersion = a} :: DocumentDescription)

-- | The name of the SSM document.
documentDescription_name :: Lens.Lens' DocumentDescription (Prelude.Maybe Prelude.Text)
documentDescription_name = Lens.lens (\DocumentDescription' {name} -> name) (\s@DocumentDescription' {} a -> s {name = a} :: DocumentDescription)

-- | The hash type of the document. Valid values include @Sha256@ or @Sha1@.
--
-- Sha1 hashes have been deprecated.
documentDescription_hashType :: Lens.Lens' DocumentDescription (Prelude.Maybe DocumentHashType)
documentDescription_hashType = Lens.lens (\DocumentDescription' {hashType} -> hashType) (\s@DocumentDescription' {} a -> s {hashType = a} :: DocumentDescription)

-- | A description of the parameters for a document.
documentDescription_parameters :: Lens.Lens' DocumentDescription (Prelude.Maybe [DocumentParameter])
documentDescription_parameters = Lens.lens (\DocumentDescription' {parameters} -> parameters) (\s@DocumentDescription' {} a -> s {parameters = a} :: DocumentDescription) Prelude.. Lens.mapping Lens.coerced

-- | The document version.
documentDescription_documentVersion :: Lens.Lens' DocumentDescription (Prelude.Maybe Prelude.Text)
documentDescription_documentVersion = Lens.lens (\DocumentDescription' {documentVersion} -> documentVersion) (\s@DocumentDescription' {} a -> s {documentVersion = a} :: DocumentDescription)

-- | The user in your organization who created the document.
documentDescription_author :: Lens.Lens' DocumentDescription (Prelude.Maybe Prelude.Text)
documentDescription_author = Lens.lens (\DocumentDescription' {author} -> author) (\s@DocumentDescription' {} a -> s {author = a} :: DocumentDescription)

-- | The friendly name of the SSM document. This value can differ for each
-- version of the document. If you want to update this value, see
-- UpdateDocument.
documentDescription_displayName :: Lens.Lens' DocumentDescription (Prelude.Maybe Prelude.Text)
documentDescription_displayName = Lens.lens (\DocumentDescription' {displayName} -> displayName) (\s@DocumentDescription' {} a -> s {displayName = a} :: DocumentDescription)

-- | A message returned by Amazon Web Services Systems Manager that explains
-- the @Status@ value. For example, a @Failed@ status might be explained by
-- the @StatusInformation@ message, \"The specified S3 bucket doesn\'t
-- exist. Verify that the URL of the S3 bucket is correct.\"
documentDescription_statusInformation :: Lens.Lens' DocumentDescription (Prelude.Maybe Prelude.Text)
documentDescription_statusInformation = Lens.lens (\DocumentDescription' {statusInformation} -> statusInformation) (\s@DocumentDescription' {} a -> s {statusInformation = a} :: DocumentDescription)

-- | A description of the document.
documentDescription_description :: Lens.Lens' DocumentDescription (Prelude.Maybe Prelude.Text)
documentDescription_description = Lens.lens (\DocumentDescription' {description} -> description) (\s@DocumentDescription' {} a -> s {description = a} :: DocumentDescription)

-- | A list of SSM documents required by a document. For example, an
-- @ApplicationConfiguration@ document requires an
-- @ApplicationConfigurationSchema@ document.
documentDescription_requires :: Lens.Lens' DocumentDescription (Prelude.Maybe (Prelude.NonEmpty DocumentRequires))
documentDescription_requires = Lens.lens (\DocumentDescription' {requires} -> requires) (\s@DocumentDescription' {} a -> s {requires = a} :: DocumentDescription) Prelude.. Lens.mapping Lens.coerced

-- | Details about the review of a document.
documentDescription_reviewInformation :: Lens.Lens' DocumentDescription (Prelude.Maybe (Prelude.NonEmpty ReviewInformation))
documentDescription_reviewInformation = Lens.lens (\DocumentDescription' {reviewInformation} -> reviewInformation) (\s@DocumentDescription' {} a -> s {reviewInformation = a} :: DocumentDescription) Prelude.. Lens.mapping Lens.coerced

-- | The tags, or metadata, that have been applied to the document.
documentDescription_tags :: Lens.Lens' DocumentDescription (Prelude.Maybe [Tag])
documentDescription_tags = Lens.lens (\DocumentDescription' {tags} -> tags) (\s@DocumentDescription' {} a -> s {tags = a} :: DocumentDescription) Prelude.. Lens.mapping Lens.coerced

-- | The latest version of the document.
documentDescription_latestVersion :: Lens.Lens' DocumentDescription (Prelude.Maybe Prelude.Text)
documentDescription_latestVersion = Lens.lens (\DocumentDescription' {latestVersion} -> latestVersion) (\s@DocumentDescription' {} a -> s {latestVersion = a} :: DocumentDescription)

-- | The version of the document currently approved for use in the
-- organization.
documentDescription_approvedVersion :: Lens.Lens' DocumentDescription (Prelude.Maybe Prelude.Text)
documentDescription_approvedVersion = Lens.lens (\DocumentDescription' {approvedVersion} -> approvedVersion) (\s@DocumentDescription' {} a -> s {approvedVersion = a} :: DocumentDescription)

instance Core.FromJSON DocumentDescription where
  parseJSON =
    Core.withObject
      "DocumentDescription"
      ( \x ->
          DocumentDescription'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "DocumentType")
            Prelude.<*> (x Core..:? "Hash")
            Prelude.<*> (x Core..:? "VersionName")
            Prelude.<*> (x Core..:? "SchemaVersion")
            Prelude.<*> (x Core..:? "Sha1")
            Prelude.<*> (x Core..:? "ReviewStatus")
            Prelude.<*> ( x Core..:? "AttachmentsInformation"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "DefaultVersion")
            Prelude.<*> (x Core..:? "TargetType")
            Prelude.<*> (x Core..:? "Owner")
            Prelude.<*> (x Core..:? "PlatformTypes" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "CreatedDate")
            Prelude.<*> (x Core..:? "DocumentFormat")
            Prelude.<*> (x Core..:? "PendingReviewVersion")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "HashType")
            Prelude.<*> (x Core..:? "Parameters" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "DocumentVersion")
            Prelude.<*> (x Core..:? "Author")
            Prelude.<*> (x Core..:? "DisplayName")
            Prelude.<*> (x Core..:? "StatusInformation")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "Requires")
            Prelude.<*> (x Core..:? "ReviewInformation")
            Prelude.<*> (x Core..:? "Tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "LatestVersion")
            Prelude.<*> (x Core..:? "ApprovedVersion")
      )

instance Prelude.Hashable DocumentDescription

instance Prelude.NFData DocumentDescription
