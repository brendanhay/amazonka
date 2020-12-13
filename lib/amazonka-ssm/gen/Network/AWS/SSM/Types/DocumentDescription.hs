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
    ddStatus,
    ddDocumentType,
    ddHash,
    ddVersionName,
    ddSchemaVersion,
    ddSha1,
    ddAttachmentsInformation,
    ddDefaultVersion,
    ddTargetType,
    ddOwner,
    ddPlatformTypes,
    ddCreatedDate,
    ddDocumentFormat,
    ddName,
    ddHashType,
    ddParameters,
    ddDocumentVersion,
    ddStatusInformation,
    ddDescription,
    ddRequires,
    ddTags,
    ddLatestVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.AttachmentInformation
import Network.AWS.SSM.Types.DocumentFormat
import Network.AWS.SSM.Types.DocumentHashType
import Network.AWS.SSM.Types.DocumentParameter
import Network.AWS.SSM.Types.DocumentRequires
import Network.AWS.SSM.Types.DocumentStatus
import Network.AWS.SSM.Types.DocumentType
import Network.AWS.SSM.Types.PlatformType
import Network.AWS.SSM.Types.Tag

-- | Describes a Systems Manager document.
--
-- /See:/ 'mkDocumentDescription' smart constructor.
data DocumentDescription = DocumentDescription'
  { -- | The status of the Systems Manager document.
    status :: Lude.Maybe DocumentStatus,
    -- | The type of document.
    documentType :: Lude.Maybe DocumentType,
    -- | The Sha256 or Sha1 hash created by the system when the document was created.
    hash :: Lude.Maybe Lude.Text,
    -- | The version of the artifact associated with the document.
    versionName :: Lude.Maybe Lude.Text,
    -- | The schema version.
    schemaVersion :: Lude.Maybe Lude.Text,
    -- | The SHA1 hash of the document, which you can use for verification.
    sha1 :: Lude.Maybe Lude.Text,
    -- | Details about the document attachments, including names, locations, sizes, and so on.
    attachmentsInformation :: Lude.Maybe [AttachmentInformation],
    -- | The default version.
    defaultVersion :: Lude.Maybe Lude.Text,
    -- | The target type which defines the kinds of resources the document can run on. For example, /AWS::EC2::Instance. For a list of valid resource types, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS resource and property types reference> in the /AWS CloudFormation User Guide/ .
    targetType :: Lude.Maybe Lude.Text,
    -- | The AWS user account that created the document.
    owner :: Lude.Maybe Lude.Text,
    -- | The list of OS platforms compatible with this Systems Manager document.
    platformTypes :: Lude.Maybe [PlatformType],
    -- | The date when the document was created.
    createdDate :: Lude.Maybe Lude.Timestamp,
    -- | The document format, either JSON or YAML.
    documentFormat :: Lude.Maybe DocumentFormat,
    -- | The name of the Systems Manager document.
    name :: Lude.Maybe Lude.Text,
    -- | The hash type of the document. Valid values include @Sha256@ or @Sha1@ .
    hashType :: Lude.Maybe DocumentHashType,
    -- | A description of the parameters for a document.
    parameters :: Lude.Maybe [DocumentParameter],
    -- | The document version.
    documentVersion :: Lude.Maybe Lude.Text,
    -- | A message returned by AWS Systems Manager that explains the @Status@ value. For example, a @Failed@ status might be explained by the @StatusInformation@ message, "The specified S3 bucket does not exist. Verify that the URL of the S3 bucket is correct."
    statusInformation :: Lude.Maybe Lude.Text,
    -- | A description of the document.
    description :: Lude.Maybe Lude.Text,
    -- | A list of SSM documents required by a document. For example, an @ApplicationConfiguration@ document requires an @ApplicationConfigurationSchema@ document.
    requires :: Lude.Maybe (Lude.NonEmpty DocumentRequires),
    -- | The tags, or metadata, that have been applied to the document.
    tags :: Lude.Maybe [Tag],
    -- | The latest version of the document.
    latestVersion :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DocumentDescription' with the minimum fields required to make a request.
--
-- * 'status' - The status of the Systems Manager document.
-- * 'documentType' - The type of document.
-- * 'hash' - The Sha256 or Sha1 hash created by the system when the document was created.
-- * 'versionName' - The version of the artifact associated with the document.
-- * 'schemaVersion' - The schema version.
-- * 'sha1' - The SHA1 hash of the document, which you can use for verification.
-- * 'attachmentsInformation' - Details about the document attachments, including names, locations, sizes, and so on.
-- * 'defaultVersion' - The default version.
-- * 'targetType' - The target type which defines the kinds of resources the document can run on. For example, /AWS::EC2::Instance. For a list of valid resource types, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS resource and property types reference> in the /AWS CloudFormation User Guide/ .
-- * 'owner' - The AWS user account that created the document.
-- * 'platformTypes' - The list of OS platforms compatible with this Systems Manager document.
-- * 'createdDate' - The date when the document was created.
-- * 'documentFormat' - The document format, either JSON or YAML.
-- * 'name' - The name of the Systems Manager document.
-- * 'hashType' - The hash type of the document. Valid values include @Sha256@ or @Sha1@ .
-- * 'parameters' - A description of the parameters for a document.
-- * 'documentVersion' - The document version.
-- * 'statusInformation' - A message returned by AWS Systems Manager that explains the @Status@ value. For example, a @Failed@ status might be explained by the @StatusInformation@ message, "The specified S3 bucket does not exist. Verify that the URL of the S3 bucket is correct."
-- * 'description' - A description of the document.
-- * 'requires' - A list of SSM documents required by a document. For example, an @ApplicationConfiguration@ document requires an @ApplicationConfigurationSchema@ document.
-- * 'tags' - The tags, or metadata, that have been applied to the document.
-- * 'latestVersion' - The latest version of the document.
mkDocumentDescription ::
  DocumentDescription
mkDocumentDescription =
  DocumentDescription'
    { status = Lude.Nothing,
      documentType = Lude.Nothing,
      hash = Lude.Nothing,
      versionName = Lude.Nothing,
      schemaVersion = Lude.Nothing,
      sha1 = Lude.Nothing,
      attachmentsInformation = Lude.Nothing,
      defaultVersion = Lude.Nothing,
      targetType = Lude.Nothing,
      owner = Lude.Nothing,
      platformTypes = Lude.Nothing,
      createdDate = Lude.Nothing,
      documentFormat = Lude.Nothing,
      name = Lude.Nothing,
      hashType = Lude.Nothing,
      parameters = Lude.Nothing,
      documentVersion = Lude.Nothing,
      statusInformation = Lude.Nothing,
      description = Lude.Nothing,
      requires = Lude.Nothing,
      tags = Lude.Nothing,
      latestVersion = Lude.Nothing
    }

-- | The status of the Systems Manager document.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddStatus :: Lens.Lens' DocumentDescription (Lude.Maybe DocumentStatus)
ddStatus = Lens.lens (status :: DocumentDescription -> Lude.Maybe DocumentStatus) (\s a -> s {status = a} :: DocumentDescription)
{-# DEPRECATED ddStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The type of document.
--
-- /Note:/ Consider using 'documentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDocumentType :: Lens.Lens' DocumentDescription (Lude.Maybe DocumentType)
ddDocumentType = Lens.lens (documentType :: DocumentDescription -> Lude.Maybe DocumentType) (\s a -> s {documentType = a} :: DocumentDescription)
{-# DEPRECATED ddDocumentType "Use generic-lens or generic-optics with 'documentType' instead." #-}

-- | The Sha256 or Sha1 hash created by the system when the document was created.
--
-- /Note:/ Consider using 'hash' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddHash :: Lens.Lens' DocumentDescription (Lude.Maybe Lude.Text)
ddHash = Lens.lens (hash :: DocumentDescription -> Lude.Maybe Lude.Text) (\s a -> s {hash = a} :: DocumentDescription)
{-# DEPRECATED ddHash "Use generic-lens or generic-optics with 'hash' instead." #-}

-- | The version of the artifact associated with the document.
--
-- /Note:/ Consider using 'versionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddVersionName :: Lens.Lens' DocumentDescription (Lude.Maybe Lude.Text)
ddVersionName = Lens.lens (versionName :: DocumentDescription -> Lude.Maybe Lude.Text) (\s a -> s {versionName = a} :: DocumentDescription)
{-# DEPRECATED ddVersionName "Use generic-lens or generic-optics with 'versionName' instead." #-}

-- | The schema version.
--
-- /Note:/ Consider using 'schemaVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddSchemaVersion :: Lens.Lens' DocumentDescription (Lude.Maybe Lude.Text)
ddSchemaVersion = Lens.lens (schemaVersion :: DocumentDescription -> Lude.Maybe Lude.Text) (\s a -> s {schemaVersion = a} :: DocumentDescription)
{-# DEPRECATED ddSchemaVersion "Use generic-lens or generic-optics with 'schemaVersion' instead." #-}

-- | The SHA1 hash of the document, which you can use for verification.
--
-- /Note:/ Consider using 'sha1' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddSha1 :: Lens.Lens' DocumentDescription (Lude.Maybe Lude.Text)
ddSha1 = Lens.lens (sha1 :: DocumentDescription -> Lude.Maybe Lude.Text) (\s a -> s {sha1 = a} :: DocumentDescription)
{-# DEPRECATED ddSha1 "Use generic-lens or generic-optics with 'sha1' instead." #-}

-- | Details about the document attachments, including names, locations, sizes, and so on.
--
-- /Note:/ Consider using 'attachmentsInformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddAttachmentsInformation :: Lens.Lens' DocumentDescription (Lude.Maybe [AttachmentInformation])
ddAttachmentsInformation = Lens.lens (attachmentsInformation :: DocumentDescription -> Lude.Maybe [AttachmentInformation]) (\s a -> s {attachmentsInformation = a} :: DocumentDescription)
{-# DEPRECATED ddAttachmentsInformation "Use generic-lens or generic-optics with 'attachmentsInformation' instead." #-}

-- | The default version.
--
-- /Note:/ Consider using 'defaultVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDefaultVersion :: Lens.Lens' DocumentDescription (Lude.Maybe Lude.Text)
ddDefaultVersion = Lens.lens (defaultVersion :: DocumentDescription -> Lude.Maybe Lude.Text) (\s a -> s {defaultVersion = a} :: DocumentDescription)
{-# DEPRECATED ddDefaultVersion "Use generic-lens or generic-optics with 'defaultVersion' instead." #-}

-- | The target type which defines the kinds of resources the document can run on. For example, /AWS::EC2::Instance. For a list of valid resource types, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS resource and property types reference> in the /AWS CloudFormation User Guide/ .
--
-- /Note:/ Consider using 'targetType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddTargetType :: Lens.Lens' DocumentDescription (Lude.Maybe Lude.Text)
ddTargetType = Lens.lens (targetType :: DocumentDescription -> Lude.Maybe Lude.Text) (\s a -> s {targetType = a} :: DocumentDescription)
{-# DEPRECATED ddTargetType "Use generic-lens or generic-optics with 'targetType' instead." #-}

-- | The AWS user account that created the document.
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddOwner :: Lens.Lens' DocumentDescription (Lude.Maybe Lude.Text)
ddOwner = Lens.lens (owner :: DocumentDescription -> Lude.Maybe Lude.Text) (\s a -> s {owner = a} :: DocumentDescription)
{-# DEPRECATED ddOwner "Use generic-lens or generic-optics with 'owner' instead." #-}

-- | The list of OS platforms compatible with this Systems Manager document.
--
-- /Note:/ Consider using 'platformTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddPlatformTypes :: Lens.Lens' DocumentDescription (Lude.Maybe [PlatformType])
ddPlatformTypes = Lens.lens (platformTypes :: DocumentDescription -> Lude.Maybe [PlatformType]) (\s a -> s {platformTypes = a} :: DocumentDescription)
{-# DEPRECATED ddPlatformTypes "Use generic-lens or generic-optics with 'platformTypes' instead." #-}

-- | The date when the document was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddCreatedDate :: Lens.Lens' DocumentDescription (Lude.Maybe Lude.Timestamp)
ddCreatedDate = Lens.lens (createdDate :: DocumentDescription -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdDate = a} :: DocumentDescription)
{-# DEPRECATED ddCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | The document format, either JSON or YAML.
--
-- /Note:/ Consider using 'documentFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDocumentFormat :: Lens.Lens' DocumentDescription (Lude.Maybe DocumentFormat)
ddDocumentFormat = Lens.lens (documentFormat :: DocumentDescription -> Lude.Maybe DocumentFormat) (\s a -> s {documentFormat = a} :: DocumentDescription)
{-# DEPRECATED ddDocumentFormat "Use generic-lens or generic-optics with 'documentFormat' instead." #-}

-- | The name of the Systems Manager document.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddName :: Lens.Lens' DocumentDescription (Lude.Maybe Lude.Text)
ddName = Lens.lens (name :: DocumentDescription -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DocumentDescription)
{-# DEPRECATED ddName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The hash type of the document. Valid values include @Sha256@ or @Sha1@ .
--
-- /Note:/ Consider using 'hashType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddHashType :: Lens.Lens' DocumentDescription (Lude.Maybe DocumentHashType)
ddHashType = Lens.lens (hashType :: DocumentDescription -> Lude.Maybe DocumentHashType) (\s a -> s {hashType = a} :: DocumentDescription)
{-# DEPRECATED ddHashType "Use generic-lens or generic-optics with 'hashType' instead." #-}

-- | A description of the parameters for a document.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddParameters :: Lens.Lens' DocumentDescription (Lude.Maybe [DocumentParameter])
ddParameters = Lens.lens (parameters :: DocumentDescription -> Lude.Maybe [DocumentParameter]) (\s a -> s {parameters = a} :: DocumentDescription)
{-# DEPRECATED ddParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The document version.
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDocumentVersion :: Lens.Lens' DocumentDescription (Lude.Maybe Lude.Text)
ddDocumentVersion = Lens.lens (documentVersion :: DocumentDescription -> Lude.Maybe Lude.Text) (\s a -> s {documentVersion = a} :: DocumentDescription)
{-# DEPRECATED ddDocumentVersion "Use generic-lens or generic-optics with 'documentVersion' instead." #-}

-- | A message returned by AWS Systems Manager that explains the @Status@ value. For example, a @Failed@ status might be explained by the @StatusInformation@ message, "The specified S3 bucket does not exist. Verify that the URL of the S3 bucket is correct."
--
-- /Note:/ Consider using 'statusInformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddStatusInformation :: Lens.Lens' DocumentDescription (Lude.Maybe Lude.Text)
ddStatusInformation = Lens.lens (statusInformation :: DocumentDescription -> Lude.Maybe Lude.Text) (\s a -> s {statusInformation = a} :: DocumentDescription)
{-# DEPRECATED ddStatusInformation "Use generic-lens or generic-optics with 'statusInformation' instead." #-}

-- | A description of the document.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDescription :: Lens.Lens' DocumentDescription (Lude.Maybe Lude.Text)
ddDescription = Lens.lens (description :: DocumentDescription -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: DocumentDescription)
{-# DEPRECATED ddDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A list of SSM documents required by a document. For example, an @ApplicationConfiguration@ document requires an @ApplicationConfigurationSchema@ document.
--
-- /Note:/ Consider using 'requires' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddRequires :: Lens.Lens' DocumentDescription (Lude.Maybe (Lude.NonEmpty DocumentRequires))
ddRequires = Lens.lens (requires :: DocumentDescription -> Lude.Maybe (Lude.NonEmpty DocumentRequires)) (\s a -> s {requires = a} :: DocumentDescription)
{-# DEPRECATED ddRequires "Use generic-lens or generic-optics with 'requires' instead." #-}

-- | The tags, or metadata, that have been applied to the document.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddTags :: Lens.Lens' DocumentDescription (Lude.Maybe [Tag])
ddTags = Lens.lens (tags :: DocumentDescription -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: DocumentDescription)
{-# DEPRECATED ddTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The latest version of the document.
--
-- /Note:/ Consider using 'latestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddLatestVersion :: Lens.Lens' DocumentDescription (Lude.Maybe Lude.Text)
ddLatestVersion = Lens.lens (latestVersion :: DocumentDescription -> Lude.Maybe Lude.Text) (\s a -> s {latestVersion = a} :: DocumentDescription)
{-# DEPRECATED ddLatestVersion "Use generic-lens or generic-optics with 'latestVersion' instead." #-}

instance Lude.FromJSON DocumentDescription where
  parseJSON =
    Lude.withObject
      "DocumentDescription"
      ( \x ->
          DocumentDescription'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "DocumentType")
            Lude.<*> (x Lude..:? "Hash")
            Lude.<*> (x Lude..:? "VersionName")
            Lude.<*> (x Lude..:? "SchemaVersion")
            Lude.<*> (x Lude..:? "Sha1")
            Lude.<*> (x Lude..:? "AttachmentsInformation" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "DefaultVersion")
            Lude.<*> (x Lude..:? "TargetType")
            Lude.<*> (x Lude..:? "Owner")
            Lude.<*> (x Lude..:? "PlatformTypes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "CreatedDate")
            Lude.<*> (x Lude..:? "DocumentFormat")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "HashType")
            Lude.<*> (x Lude..:? "Parameters" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "DocumentVersion")
            Lude.<*> (x Lude..:? "StatusInformation")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "Requires")
            Lude.<*> (x Lude..:? "Tags" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "LatestVersion")
      )
