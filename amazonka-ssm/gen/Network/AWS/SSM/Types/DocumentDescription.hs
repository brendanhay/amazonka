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
-- Module      : Network.AWS.SSM.Types.DocumentDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.DocumentDescription where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SSM.Types.AttachmentInformation
import Network.AWS.SSM.Types.DocumentFormat
import Network.AWS.SSM.Types.DocumentHashType
import Network.AWS.SSM.Types.DocumentParameter
import Network.AWS.SSM.Types.DocumentRequires
import Network.AWS.SSM.Types.DocumentStatus
import Network.AWS.SSM.Types.DocumentType
import Network.AWS.SSM.Types.PlatformType
import Network.AWS.SSM.Types.ReviewInformation
import Network.AWS.SSM.Types.ReviewStatus
import Network.AWS.SSM.Types.Tag

-- | Describes a Systems Manager document.
--
-- /See:/ 'newDocumentDescription' smart constructor.
data DocumentDescription = DocumentDescription'
  { -- | The type of document.
    documentType :: Core.Maybe DocumentType,
    -- | The status of the Systems Manager document.
    status :: Core.Maybe DocumentStatus,
    -- | The date when the document was created.
    createdDate :: Core.Maybe Core.POSIX,
    -- | The list of OS platforms compatible with this Systems Manager document.
    platformTypes :: Core.Maybe [PlatformType],
    -- | The default version.
    defaultVersion :: Core.Maybe Core.Text,
    -- | The latest version of the document.
    latestVersion :: Core.Maybe Core.Text,
    -- | The target type which defines the kinds of resources the document can
    -- run on. For example, \/AWS::EC2::Instance. For a list of valid resource
    -- types, see
    -- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS resource and property types reference>
    -- in the /AWS CloudFormation User Guide/.
    targetType :: Core.Maybe Core.Text,
    -- | The version of the document currently approved for use in the
    -- organization.
    approvedVersion :: Core.Maybe Core.Text,
    -- | A list of SSM documents required by a document. For example, an
    -- @ApplicationConfiguration@ document requires an
    -- @ApplicationConfigurationSchema@ document.
    requires :: Core.Maybe (Core.NonEmpty DocumentRequires),
    -- | The SHA1 hash of the document, which you can use for verification.
    sha1 :: Core.Maybe Core.Text,
    -- | A message returned by AWS Systems Manager that explains the @Status@
    -- value. For example, a @Failed@ status might be explained by the
    -- @StatusInformation@ message, \"The specified S3 bucket does not exist.
    -- Verify that the URL of the S3 bucket is correct.\"
    statusInformation :: Core.Maybe Core.Text,
    -- | The version of the artifact associated with the document.
    versionName :: Core.Maybe Core.Text,
    -- | The user in your organization who created the document.
    author :: Core.Maybe Core.Text,
    -- | The Sha256 or Sha1 hash created by the system when the document was
    -- created.
    --
    -- Sha1 hashes have been deprecated.
    hash :: Core.Maybe Core.Text,
    -- | The version of the document that is currently under review.
    pendingReviewVersion :: Core.Maybe Core.Text,
    -- | The name of the Systems Manager document.
    name :: Core.Maybe Core.Text,
    -- | The document format, either JSON or YAML.
    documentFormat :: Core.Maybe DocumentFormat,
    -- | The tags, or metadata, that have been applied to the document.
    tags :: Core.Maybe [Tag],
    -- | The AWS user account that created the document.
    owner :: Core.Maybe Core.Text,
    -- | The current status of the review.
    reviewStatus :: Core.Maybe ReviewStatus,
    -- | Details about the review of a document.
    reviewInformation :: Core.Maybe (Core.NonEmpty ReviewInformation),
    -- | Details about the document attachments, including names, locations,
    -- sizes, and so on.
    attachmentsInformation :: Core.Maybe [AttachmentInformation],
    -- | A description of the document.
    description :: Core.Maybe Core.Text,
    -- | The schema version.
    schemaVersion :: Core.Maybe Core.Text,
    -- | The document version.
    documentVersion :: Core.Maybe Core.Text,
    -- | A description of the parameters for a document.
    parameters :: Core.Maybe [DocumentParameter],
    -- | The hash type of the document. Valid values include @Sha256@ or @Sha1@.
    --
    -- Sha1 hashes have been deprecated.
    hashType :: Core.Maybe DocumentHashType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DocumentDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'documentType', 'documentDescription_documentType' - The type of document.
--
-- 'status', 'documentDescription_status' - The status of the Systems Manager document.
--
-- 'createdDate', 'documentDescription_createdDate' - The date when the document was created.
--
-- 'platformTypes', 'documentDescription_platformTypes' - The list of OS platforms compatible with this Systems Manager document.
--
-- 'defaultVersion', 'documentDescription_defaultVersion' - The default version.
--
-- 'latestVersion', 'documentDescription_latestVersion' - The latest version of the document.
--
-- 'targetType', 'documentDescription_targetType' - The target type which defines the kinds of resources the document can
-- run on. For example, \/AWS::EC2::Instance. For a list of valid resource
-- types, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS resource and property types reference>
-- in the /AWS CloudFormation User Guide/.
--
-- 'approvedVersion', 'documentDescription_approvedVersion' - The version of the document currently approved for use in the
-- organization.
--
-- 'requires', 'documentDescription_requires' - A list of SSM documents required by a document. For example, an
-- @ApplicationConfiguration@ document requires an
-- @ApplicationConfigurationSchema@ document.
--
-- 'sha1', 'documentDescription_sha1' - The SHA1 hash of the document, which you can use for verification.
--
-- 'statusInformation', 'documentDescription_statusInformation' - A message returned by AWS Systems Manager that explains the @Status@
-- value. For example, a @Failed@ status might be explained by the
-- @StatusInformation@ message, \"The specified S3 bucket does not exist.
-- Verify that the URL of the S3 bucket is correct.\"
--
-- 'versionName', 'documentDescription_versionName' - The version of the artifact associated with the document.
--
-- 'author', 'documentDescription_author' - The user in your organization who created the document.
--
-- 'hash', 'documentDescription_hash' - The Sha256 or Sha1 hash created by the system when the document was
-- created.
--
-- Sha1 hashes have been deprecated.
--
-- 'pendingReviewVersion', 'documentDescription_pendingReviewVersion' - The version of the document that is currently under review.
--
-- 'name', 'documentDescription_name' - The name of the Systems Manager document.
--
-- 'documentFormat', 'documentDescription_documentFormat' - The document format, either JSON or YAML.
--
-- 'tags', 'documentDescription_tags' - The tags, or metadata, that have been applied to the document.
--
-- 'owner', 'documentDescription_owner' - The AWS user account that created the document.
--
-- 'reviewStatus', 'documentDescription_reviewStatus' - The current status of the review.
--
-- 'reviewInformation', 'documentDescription_reviewInformation' - Details about the review of a document.
--
-- 'attachmentsInformation', 'documentDescription_attachmentsInformation' - Details about the document attachments, including names, locations,
-- sizes, and so on.
--
-- 'description', 'documentDescription_description' - A description of the document.
--
-- 'schemaVersion', 'documentDescription_schemaVersion' - The schema version.
--
-- 'documentVersion', 'documentDescription_documentVersion' - The document version.
--
-- 'parameters', 'documentDescription_parameters' - A description of the parameters for a document.
--
-- 'hashType', 'documentDescription_hashType' - The hash type of the document. Valid values include @Sha256@ or @Sha1@.
--
-- Sha1 hashes have been deprecated.
newDocumentDescription ::
  DocumentDescription
newDocumentDescription =
  DocumentDescription'
    { documentType = Core.Nothing,
      status = Core.Nothing,
      createdDate = Core.Nothing,
      platformTypes = Core.Nothing,
      defaultVersion = Core.Nothing,
      latestVersion = Core.Nothing,
      targetType = Core.Nothing,
      approvedVersion = Core.Nothing,
      requires = Core.Nothing,
      sha1 = Core.Nothing,
      statusInformation = Core.Nothing,
      versionName = Core.Nothing,
      author = Core.Nothing,
      hash = Core.Nothing,
      pendingReviewVersion = Core.Nothing,
      name = Core.Nothing,
      documentFormat = Core.Nothing,
      tags = Core.Nothing,
      owner = Core.Nothing,
      reviewStatus = Core.Nothing,
      reviewInformation = Core.Nothing,
      attachmentsInformation = Core.Nothing,
      description = Core.Nothing,
      schemaVersion = Core.Nothing,
      documentVersion = Core.Nothing,
      parameters = Core.Nothing,
      hashType = Core.Nothing
    }

-- | The type of document.
documentDescription_documentType :: Lens.Lens' DocumentDescription (Core.Maybe DocumentType)
documentDescription_documentType = Lens.lens (\DocumentDescription' {documentType} -> documentType) (\s@DocumentDescription' {} a -> s {documentType = a} :: DocumentDescription)

-- | The status of the Systems Manager document.
documentDescription_status :: Lens.Lens' DocumentDescription (Core.Maybe DocumentStatus)
documentDescription_status = Lens.lens (\DocumentDescription' {status} -> status) (\s@DocumentDescription' {} a -> s {status = a} :: DocumentDescription)

-- | The date when the document was created.
documentDescription_createdDate :: Lens.Lens' DocumentDescription (Core.Maybe Core.UTCTime)
documentDescription_createdDate = Lens.lens (\DocumentDescription' {createdDate} -> createdDate) (\s@DocumentDescription' {} a -> s {createdDate = a} :: DocumentDescription) Core.. Lens.mapping Core._Time

-- | The list of OS platforms compatible with this Systems Manager document.
documentDescription_platformTypes :: Lens.Lens' DocumentDescription (Core.Maybe [PlatformType])
documentDescription_platformTypes = Lens.lens (\DocumentDescription' {platformTypes} -> platformTypes) (\s@DocumentDescription' {} a -> s {platformTypes = a} :: DocumentDescription) Core.. Lens.mapping Lens._Coerce

-- | The default version.
documentDescription_defaultVersion :: Lens.Lens' DocumentDescription (Core.Maybe Core.Text)
documentDescription_defaultVersion = Lens.lens (\DocumentDescription' {defaultVersion} -> defaultVersion) (\s@DocumentDescription' {} a -> s {defaultVersion = a} :: DocumentDescription)

-- | The latest version of the document.
documentDescription_latestVersion :: Lens.Lens' DocumentDescription (Core.Maybe Core.Text)
documentDescription_latestVersion = Lens.lens (\DocumentDescription' {latestVersion} -> latestVersion) (\s@DocumentDescription' {} a -> s {latestVersion = a} :: DocumentDescription)

-- | The target type which defines the kinds of resources the document can
-- run on. For example, \/AWS::EC2::Instance. For a list of valid resource
-- types, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS resource and property types reference>
-- in the /AWS CloudFormation User Guide/.
documentDescription_targetType :: Lens.Lens' DocumentDescription (Core.Maybe Core.Text)
documentDescription_targetType = Lens.lens (\DocumentDescription' {targetType} -> targetType) (\s@DocumentDescription' {} a -> s {targetType = a} :: DocumentDescription)

-- | The version of the document currently approved for use in the
-- organization.
documentDescription_approvedVersion :: Lens.Lens' DocumentDescription (Core.Maybe Core.Text)
documentDescription_approvedVersion = Lens.lens (\DocumentDescription' {approvedVersion} -> approvedVersion) (\s@DocumentDescription' {} a -> s {approvedVersion = a} :: DocumentDescription)

-- | A list of SSM documents required by a document. For example, an
-- @ApplicationConfiguration@ document requires an
-- @ApplicationConfigurationSchema@ document.
documentDescription_requires :: Lens.Lens' DocumentDescription (Core.Maybe (Core.NonEmpty DocumentRequires))
documentDescription_requires = Lens.lens (\DocumentDescription' {requires} -> requires) (\s@DocumentDescription' {} a -> s {requires = a} :: DocumentDescription) Core.. Lens.mapping Lens._Coerce

-- | The SHA1 hash of the document, which you can use for verification.
documentDescription_sha1 :: Lens.Lens' DocumentDescription (Core.Maybe Core.Text)
documentDescription_sha1 = Lens.lens (\DocumentDescription' {sha1} -> sha1) (\s@DocumentDescription' {} a -> s {sha1 = a} :: DocumentDescription)

-- | A message returned by AWS Systems Manager that explains the @Status@
-- value. For example, a @Failed@ status might be explained by the
-- @StatusInformation@ message, \"The specified S3 bucket does not exist.
-- Verify that the URL of the S3 bucket is correct.\"
documentDescription_statusInformation :: Lens.Lens' DocumentDescription (Core.Maybe Core.Text)
documentDescription_statusInformation = Lens.lens (\DocumentDescription' {statusInformation} -> statusInformation) (\s@DocumentDescription' {} a -> s {statusInformation = a} :: DocumentDescription)

-- | The version of the artifact associated with the document.
documentDescription_versionName :: Lens.Lens' DocumentDescription (Core.Maybe Core.Text)
documentDescription_versionName = Lens.lens (\DocumentDescription' {versionName} -> versionName) (\s@DocumentDescription' {} a -> s {versionName = a} :: DocumentDescription)

-- | The user in your organization who created the document.
documentDescription_author :: Lens.Lens' DocumentDescription (Core.Maybe Core.Text)
documentDescription_author = Lens.lens (\DocumentDescription' {author} -> author) (\s@DocumentDescription' {} a -> s {author = a} :: DocumentDescription)

-- | The Sha256 or Sha1 hash created by the system when the document was
-- created.
--
-- Sha1 hashes have been deprecated.
documentDescription_hash :: Lens.Lens' DocumentDescription (Core.Maybe Core.Text)
documentDescription_hash = Lens.lens (\DocumentDescription' {hash} -> hash) (\s@DocumentDescription' {} a -> s {hash = a} :: DocumentDescription)

-- | The version of the document that is currently under review.
documentDescription_pendingReviewVersion :: Lens.Lens' DocumentDescription (Core.Maybe Core.Text)
documentDescription_pendingReviewVersion = Lens.lens (\DocumentDescription' {pendingReviewVersion} -> pendingReviewVersion) (\s@DocumentDescription' {} a -> s {pendingReviewVersion = a} :: DocumentDescription)

-- | The name of the Systems Manager document.
documentDescription_name :: Lens.Lens' DocumentDescription (Core.Maybe Core.Text)
documentDescription_name = Lens.lens (\DocumentDescription' {name} -> name) (\s@DocumentDescription' {} a -> s {name = a} :: DocumentDescription)

-- | The document format, either JSON or YAML.
documentDescription_documentFormat :: Lens.Lens' DocumentDescription (Core.Maybe DocumentFormat)
documentDescription_documentFormat = Lens.lens (\DocumentDescription' {documentFormat} -> documentFormat) (\s@DocumentDescription' {} a -> s {documentFormat = a} :: DocumentDescription)

-- | The tags, or metadata, that have been applied to the document.
documentDescription_tags :: Lens.Lens' DocumentDescription (Core.Maybe [Tag])
documentDescription_tags = Lens.lens (\DocumentDescription' {tags} -> tags) (\s@DocumentDescription' {} a -> s {tags = a} :: DocumentDescription) Core.. Lens.mapping Lens._Coerce

-- | The AWS user account that created the document.
documentDescription_owner :: Lens.Lens' DocumentDescription (Core.Maybe Core.Text)
documentDescription_owner = Lens.lens (\DocumentDescription' {owner} -> owner) (\s@DocumentDescription' {} a -> s {owner = a} :: DocumentDescription)

-- | The current status of the review.
documentDescription_reviewStatus :: Lens.Lens' DocumentDescription (Core.Maybe ReviewStatus)
documentDescription_reviewStatus = Lens.lens (\DocumentDescription' {reviewStatus} -> reviewStatus) (\s@DocumentDescription' {} a -> s {reviewStatus = a} :: DocumentDescription)

-- | Details about the review of a document.
documentDescription_reviewInformation :: Lens.Lens' DocumentDescription (Core.Maybe (Core.NonEmpty ReviewInformation))
documentDescription_reviewInformation = Lens.lens (\DocumentDescription' {reviewInformation} -> reviewInformation) (\s@DocumentDescription' {} a -> s {reviewInformation = a} :: DocumentDescription) Core.. Lens.mapping Lens._Coerce

-- | Details about the document attachments, including names, locations,
-- sizes, and so on.
documentDescription_attachmentsInformation :: Lens.Lens' DocumentDescription (Core.Maybe [AttachmentInformation])
documentDescription_attachmentsInformation = Lens.lens (\DocumentDescription' {attachmentsInformation} -> attachmentsInformation) (\s@DocumentDescription' {} a -> s {attachmentsInformation = a} :: DocumentDescription) Core.. Lens.mapping Lens._Coerce

-- | A description of the document.
documentDescription_description :: Lens.Lens' DocumentDescription (Core.Maybe Core.Text)
documentDescription_description = Lens.lens (\DocumentDescription' {description} -> description) (\s@DocumentDescription' {} a -> s {description = a} :: DocumentDescription)

-- | The schema version.
documentDescription_schemaVersion :: Lens.Lens' DocumentDescription (Core.Maybe Core.Text)
documentDescription_schemaVersion = Lens.lens (\DocumentDescription' {schemaVersion} -> schemaVersion) (\s@DocumentDescription' {} a -> s {schemaVersion = a} :: DocumentDescription)

-- | The document version.
documentDescription_documentVersion :: Lens.Lens' DocumentDescription (Core.Maybe Core.Text)
documentDescription_documentVersion = Lens.lens (\DocumentDescription' {documentVersion} -> documentVersion) (\s@DocumentDescription' {} a -> s {documentVersion = a} :: DocumentDescription)

-- | A description of the parameters for a document.
documentDescription_parameters :: Lens.Lens' DocumentDescription (Core.Maybe [DocumentParameter])
documentDescription_parameters = Lens.lens (\DocumentDescription' {parameters} -> parameters) (\s@DocumentDescription' {} a -> s {parameters = a} :: DocumentDescription) Core.. Lens.mapping Lens._Coerce

-- | The hash type of the document. Valid values include @Sha256@ or @Sha1@.
--
-- Sha1 hashes have been deprecated.
documentDescription_hashType :: Lens.Lens' DocumentDescription (Core.Maybe DocumentHashType)
documentDescription_hashType = Lens.lens (\DocumentDescription' {hashType} -> hashType) (\s@DocumentDescription' {} a -> s {hashType = a} :: DocumentDescription)

instance Core.FromJSON DocumentDescription where
  parseJSON =
    Core.withObject
      "DocumentDescription"
      ( \x ->
          DocumentDescription'
            Core.<$> (x Core..:? "DocumentType")
            Core.<*> (x Core..:? "Status")
            Core.<*> (x Core..:? "CreatedDate")
            Core.<*> (x Core..:? "PlatformTypes" Core..!= Core.mempty)
            Core.<*> (x Core..:? "DefaultVersion")
            Core.<*> (x Core..:? "LatestVersion")
            Core.<*> (x Core..:? "TargetType")
            Core.<*> (x Core..:? "ApprovedVersion")
            Core.<*> (x Core..:? "Requires")
            Core.<*> (x Core..:? "Sha1")
            Core.<*> (x Core..:? "StatusInformation")
            Core.<*> (x Core..:? "VersionName")
            Core.<*> (x Core..:? "Author")
            Core.<*> (x Core..:? "Hash")
            Core.<*> (x Core..:? "PendingReviewVersion")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "DocumentFormat")
            Core.<*> (x Core..:? "Tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Owner")
            Core.<*> (x Core..:? "ReviewStatus")
            Core.<*> (x Core..:? "ReviewInformation")
            Core.<*> ( x Core..:? "AttachmentsInformation"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "SchemaVersion")
            Core.<*> (x Core..:? "DocumentVersion")
            Core.<*> (x Core..:? "Parameters" Core..!= Core.mempty)
            Core.<*> (x Core..:? "HashType")
      )

instance Core.Hashable DocumentDescription

instance Core.NFData DocumentDescription
