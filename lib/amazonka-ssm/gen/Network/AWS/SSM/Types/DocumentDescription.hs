{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.DocumentDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.DocumentDescription
  ( DocumentDescription (..),

    -- * Smart constructor
    mkDocumentDescription,

    -- * Lenses
    ddAttachmentsInformation,
    ddCreatedDate,
    ddDefaultVersion,
    ddDescription,
    ddDocumentFormat,
    ddDocumentType,
    ddDocumentVersion,
    ddHash,
    ddHashType,
    ddLatestVersion,
    ddName,
    ddOwner,
    ddParameters,
    ddPlatformTypes,
    ddRequires,
    ddSchemaVersion,
    ddSha1,
    ddStatus,
    ddStatusInformation,
    ddTags,
    ddTargetType,
    ddVersionName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.AttachmentInformation as Types
import qualified Network.AWS.SSM.Types.DefaultVersion as Types
import qualified Network.AWS.SSM.Types.Description as Types
import qualified Network.AWS.SSM.Types.DocumentARN as Types
import qualified Network.AWS.SSM.Types.DocumentFormat as Types
import qualified Network.AWS.SSM.Types.DocumentHashType as Types
import qualified Network.AWS.SSM.Types.DocumentParameter as Types
import qualified Network.AWS.SSM.Types.DocumentRequires as Types
import qualified Network.AWS.SSM.Types.DocumentSchemaVersion as Types
import qualified Network.AWS.SSM.Types.DocumentStatus as Types
import qualified Network.AWS.SSM.Types.DocumentType as Types
import qualified Network.AWS.SSM.Types.DocumentVersion as Types
import qualified Network.AWS.SSM.Types.DocumentVersionName as Types
import qualified Network.AWS.SSM.Types.Hash as Types
import qualified Network.AWS.SSM.Types.LatestVersion as Types
import qualified Network.AWS.SSM.Types.Owner as Types
import qualified Network.AWS.SSM.Types.PlatformType as Types
import qualified Network.AWS.SSM.Types.Sha1 as Types
import qualified Network.AWS.SSM.Types.StatusInformation as Types
import qualified Network.AWS.SSM.Types.Tag as Types
import qualified Network.AWS.SSM.Types.TargetType as Types

-- | Describes a Systems Manager document.
--
-- /See:/ 'mkDocumentDescription' smart constructor.
data DocumentDescription = DocumentDescription'
  { -- | Details about the document attachments, including names, locations, sizes, and so on.
    attachmentsInformation :: Core.Maybe [Types.AttachmentInformation],
    -- | The date when the document was created.
    createdDate :: Core.Maybe Core.NominalDiffTime,
    -- | The default version.
    defaultVersion :: Core.Maybe Types.DefaultVersion,
    -- | A description of the document.
    description :: Core.Maybe Types.Description,
    -- | The document format, either JSON or YAML.
    documentFormat :: Core.Maybe Types.DocumentFormat,
    -- | The type of document.
    documentType :: Core.Maybe Types.DocumentType,
    -- | The document version.
    documentVersion :: Core.Maybe Types.DocumentVersion,
    -- | The Sha256 or Sha1 hash created by the system when the document was created.
    hash :: Core.Maybe Types.Hash,
    -- | The hash type of the document. Valid values include @Sha256@ or @Sha1@ .
    hashType :: Core.Maybe Types.DocumentHashType,
    -- | The latest version of the document.
    latestVersion :: Core.Maybe Types.LatestVersion,
    -- | The name of the Systems Manager document.
    name :: Core.Maybe Types.DocumentARN,
    -- | The AWS user account that created the document.
    owner :: Core.Maybe Types.Owner,
    -- | A description of the parameters for a document.
    parameters :: Core.Maybe [Types.DocumentParameter],
    -- | The list of OS platforms compatible with this Systems Manager document.
    platformTypes :: Core.Maybe [Types.PlatformType],
    -- | A list of SSM documents required by a document. For example, an @ApplicationConfiguration@ document requires an @ApplicationConfigurationSchema@ document.
    requires :: Core.Maybe (Core.NonEmpty Types.DocumentRequires),
    -- | The schema version.
    schemaVersion :: Core.Maybe Types.DocumentSchemaVersion,
    -- | The SHA1 hash of the document, which you can use for verification.
    sha1 :: Core.Maybe Types.Sha1,
    -- | The status of the Systems Manager document.
    status :: Core.Maybe Types.DocumentStatus,
    -- | A message returned by AWS Systems Manager that explains the @Status@ value. For example, a @Failed@ status might be explained by the @StatusInformation@ message, "The specified S3 bucket does not exist. Verify that the URL of the S3 bucket is correct."
    statusInformation :: Core.Maybe Types.StatusInformation,
    -- | The tags, or metadata, that have been applied to the document.
    tags :: Core.Maybe [Types.Tag],
    -- | The target type which defines the kinds of resources the document can run on. For example, /AWS::EC2::Instance. For a list of valid resource types, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS resource and property types reference> in the /AWS CloudFormation User Guide/ .
    targetType :: Core.Maybe Types.TargetType,
    -- | The version of the artifact associated with the document.
    versionName :: Core.Maybe Types.DocumentVersionName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DocumentDescription' value with any optional fields omitted.
mkDocumentDescription ::
  DocumentDescription
mkDocumentDescription =
  DocumentDescription'
    { attachmentsInformation = Core.Nothing,
      createdDate = Core.Nothing,
      defaultVersion = Core.Nothing,
      description = Core.Nothing,
      documentFormat = Core.Nothing,
      documentType = Core.Nothing,
      documentVersion = Core.Nothing,
      hash = Core.Nothing,
      hashType = Core.Nothing,
      latestVersion = Core.Nothing,
      name = Core.Nothing,
      owner = Core.Nothing,
      parameters = Core.Nothing,
      platformTypes = Core.Nothing,
      requires = Core.Nothing,
      schemaVersion = Core.Nothing,
      sha1 = Core.Nothing,
      status = Core.Nothing,
      statusInformation = Core.Nothing,
      tags = Core.Nothing,
      targetType = Core.Nothing,
      versionName = Core.Nothing
    }

-- | Details about the document attachments, including names, locations, sizes, and so on.
--
-- /Note:/ Consider using 'attachmentsInformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddAttachmentsInformation :: Lens.Lens' DocumentDescription (Core.Maybe [Types.AttachmentInformation])
ddAttachmentsInformation = Lens.field @"attachmentsInformation"
{-# DEPRECATED ddAttachmentsInformation "Use generic-lens or generic-optics with 'attachmentsInformation' instead." #-}

-- | The date when the document was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddCreatedDate :: Lens.Lens' DocumentDescription (Core.Maybe Core.NominalDiffTime)
ddCreatedDate = Lens.field @"createdDate"
{-# DEPRECATED ddCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | The default version.
--
-- /Note:/ Consider using 'defaultVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDefaultVersion :: Lens.Lens' DocumentDescription (Core.Maybe Types.DefaultVersion)
ddDefaultVersion = Lens.field @"defaultVersion"
{-# DEPRECATED ddDefaultVersion "Use generic-lens or generic-optics with 'defaultVersion' instead." #-}

-- | A description of the document.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDescription :: Lens.Lens' DocumentDescription (Core.Maybe Types.Description)
ddDescription = Lens.field @"description"
{-# DEPRECATED ddDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The document format, either JSON or YAML.
--
-- /Note:/ Consider using 'documentFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDocumentFormat :: Lens.Lens' DocumentDescription (Core.Maybe Types.DocumentFormat)
ddDocumentFormat = Lens.field @"documentFormat"
{-# DEPRECATED ddDocumentFormat "Use generic-lens or generic-optics with 'documentFormat' instead." #-}

-- | The type of document.
--
-- /Note:/ Consider using 'documentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDocumentType :: Lens.Lens' DocumentDescription (Core.Maybe Types.DocumentType)
ddDocumentType = Lens.field @"documentType"
{-# DEPRECATED ddDocumentType "Use generic-lens or generic-optics with 'documentType' instead." #-}

-- | The document version.
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDocumentVersion :: Lens.Lens' DocumentDescription (Core.Maybe Types.DocumentVersion)
ddDocumentVersion = Lens.field @"documentVersion"
{-# DEPRECATED ddDocumentVersion "Use generic-lens or generic-optics with 'documentVersion' instead." #-}

-- | The Sha256 or Sha1 hash created by the system when the document was created.
--
-- /Note:/ Consider using 'hash' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddHash :: Lens.Lens' DocumentDescription (Core.Maybe Types.Hash)
ddHash = Lens.field @"hash"
{-# DEPRECATED ddHash "Use generic-lens or generic-optics with 'hash' instead." #-}

-- | The hash type of the document. Valid values include @Sha256@ or @Sha1@ .
--
-- /Note:/ Consider using 'hashType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddHashType :: Lens.Lens' DocumentDescription (Core.Maybe Types.DocumentHashType)
ddHashType = Lens.field @"hashType"
{-# DEPRECATED ddHashType "Use generic-lens or generic-optics with 'hashType' instead." #-}

-- | The latest version of the document.
--
-- /Note:/ Consider using 'latestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddLatestVersion :: Lens.Lens' DocumentDescription (Core.Maybe Types.LatestVersion)
ddLatestVersion = Lens.field @"latestVersion"
{-# DEPRECATED ddLatestVersion "Use generic-lens or generic-optics with 'latestVersion' instead." #-}

-- | The name of the Systems Manager document.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddName :: Lens.Lens' DocumentDescription (Core.Maybe Types.DocumentARN)
ddName = Lens.field @"name"
{-# DEPRECATED ddName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The AWS user account that created the document.
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddOwner :: Lens.Lens' DocumentDescription (Core.Maybe Types.Owner)
ddOwner = Lens.field @"owner"
{-# DEPRECATED ddOwner "Use generic-lens or generic-optics with 'owner' instead." #-}

-- | A description of the parameters for a document.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddParameters :: Lens.Lens' DocumentDescription (Core.Maybe [Types.DocumentParameter])
ddParameters = Lens.field @"parameters"
{-# DEPRECATED ddParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The list of OS platforms compatible with this Systems Manager document.
--
-- /Note:/ Consider using 'platformTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddPlatformTypes :: Lens.Lens' DocumentDescription (Core.Maybe [Types.PlatformType])
ddPlatformTypes = Lens.field @"platformTypes"
{-# DEPRECATED ddPlatformTypes "Use generic-lens or generic-optics with 'platformTypes' instead." #-}

-- | A list of SSM documents required by a document. For example, an @ApplicationConfiguration@ document requires an @ApplicationConfigurationSchema@ document.
--
-- /Note:/ Consider using 'requires' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddRequires :: Lens.Lens' DocumentDescription (Core.Maybe (Core.NonEmpty Types.DocumentRequires))
ddRequires = Lens.field @"requires"
{-# DEPRECATED ddRequires "Use generic-lens or generic-optics with 'requires' instead." #-}

-- | The schema version.
--
-- /Note:/ Consider using 'schemaVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddSchemaVersion :: Lens.Lens' DocumentDescription (Core.Maybe Types.DocumentSchemaVersion)
ddSchemaVersion = Lens.field @"schemaVersion"
{-# DEPRECATED ddSchemaVersion "Use generic-lens or generic-optics with 'schemaVersion' instead." #-}

-- | The SHA1 hash of the document, which you can use for verification.
--
-- /Note:/ Consider using 'sha1' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddSha1 :: Lens.Lens' DocumentDescription (Core.Maybe Types.Sha1)
ddSha1 = Lens.field @"sha1"
{-# DEPRECATED ddSha1 "Use generic-lens or generic-optics with 'sha1' instead." #-}

-- | The status of the Systems Manager document.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddStatus :: Lens.Lens' DocumentDescription (Core.Maybe Types.DocumentStatus)
ddStatus = Lens.field @"status"
{-# DEPRECATED ddStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | A message returned by AWS Systems Manager that explains the @Status@ value. For example, a @Failed@ status might be explained by the @StatusInformation@ message, "The specified S3 bucket does not exist. Verify that the URL of the S3 bucket is correct."
--
-- /Note:/ Consider using 'statusInformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddStatusInformation :: Lens.Lens' DocumentDescription (Core.Maybe Types.StatusInformation)
ddStatusInformation = Lens.field @"statusInformation"
{-# DEPRECATED ddStatusInformation "Use generic-lens or generic-optics with 'statusInformation' instead." #-}

-- | The tags, or metadata, that have been applied to the document.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddTags :: Lens.Lens' DocumentDescription (Core.Maybe [Types.Tag])
ddTags = Lens.field @"tags"
{-# DEPRECATED ddTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The target type which defines the kinds of resources the document can run on. For example, /AWS::EC2::Instance. For a list of valid resource types, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS resource and property types reference> in the /AWS CloudFormation User Guide/ .
--
-- /Note:/ Consider using 'targetType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddTargetType :: Lens.Lens' DocumentDescription (Core.Maybe Types.TargetType)
ddTargetType = Lens.field @"targetType"
{-# DEPRECATED ddTargetType "Use generic-lens or generic-optics with 'targetType' instead." #-}

-- | The version of the artifact associated with the document.
--
-- /Note:/ Consider using 'versionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddVersionName :: Lens.Lens' DocumentDescription (Core.Maybe Types.DocumentVersionName)
ddVersionName = Lens.field @"versionName"
{-# DEPRECATED ddVersionName "Use generic-lens or generic-optics with 'versionName' instead." #-}

instance Core.FromJSON DocumentDescription where
  parseJSON =
    Core.withObject "DocumentDescription" Core.$
      \x ->
        DocumentDescription'
          Core.<$> (x Core..:? "AttachmentsInformation")
          Core.<*> (x Core..:? "CreatedDate")
          Core.<*> (x Core..:? "DefaultVersion")
          Core.<*> (x Core..:? "Description")
          Core.<*> (x Core..:? "DocumentFormat")
          Core.<*> (x Core..:? "DocumentType")
          Core.<*> (x Core..:? "DocumentVersion")
          Core.<*> (x Core..:? "Hash")
          Core.<*> (x Core..:? "HashType")
          Core.<*> (x Core..:? "LatestVersion")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "Owner")
          Core.<*> (x Core..:? "Parameters")
          Core.<*> (x Core..:? "PlatformTypes")
          Core.<*> (x Core..:? "Requires")
          Core.<*> (x Core..:? "SchemaVersion")
          Core.<*> (x Core..:? "Sha1")
          Core.<*> (x Core..:? "Status")
          Core.<*> (x Core..:? "StatusInformation")
          Core.<*> (x Core..:? "Tags")
          Core.<*> (x Core..:? "TargetType")
          Core.<*> (x Core..:? "VersionName")
