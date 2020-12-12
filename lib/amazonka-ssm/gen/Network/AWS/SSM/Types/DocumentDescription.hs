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
    dStatus,
    dDocumentType,
    dHash,
    dVersionName,
    dSchemaVersion,
    dSha1,
    dAttachmentsInformation,
    dDefaultVersion,
    dTargetType,
    dOwner,
    dPlatformTypes,
    dCreatedDate,
    dDocumentFormat,
    dName,
    dHashType,
    dParameters,
    dDocumentVersion,
    dStatusInformation,
    dDescription,
    dRequires,
    dTags,
    dLatestVersion,
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
  { status ::
      Lude.Maybe DocumentStatus,
    documentType :: Lude.Maybe DocumentType,
    hash :: Lude.Maybe Lude.Text,
    versionName :: Lude.Maybe Lude.Text,
    schemaVersion :: Lude.Maybe Lude.Text,
    sha1 :: Lude.Maybe Lude.Text,
    attachmentsInformation ::
      Lude.Maybe [AttachmentInformation],
    defaultVersion :: Lude.Maybe Lude.Text,
    targetType :: Lude.Maybe Lude.Text,
    owner :: Lude.Maybe Lude.Text,
    platformTypes :: Lude.Maybe [PlatformType],
    createdDate :: Lude.Maybe Lude.Timestamp,
    documentFormat :: Lude.Maybe DocumentFormat,
    name :: Lude.Maybe Lude.Text,
    hashType :: Lude.Maybe DocumentHashType,
    parameters :: Lude.Maybe [DocumentParameter],
    documentVersion :: Lude.Maybe Lude.Text,
    statusInformation :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    requires ::
      Lude.Maybe (Lude.NonEmpty DocumentRequires),
    tags :: Lude.Maybe [Tag],
    latestVersion :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DocumentDescription' with the minimum fields required to make a request.
--
-- * 'attachmentsInformation' - Details about the document attachments, including names, locations, sizes, and so on.
-- * 'createdDate' - The date when the document was created.
-- * 'defaultVersion' - The default version.
-- * 'description' - A description of the document.
-- * 'documentFormat' - The document format, either JSON or YAML.
-- * 'documentType' - The type of document.
-- * 'documentVersion' - The document version.
-- * 'hash' - The Sha256 or Sha1 hash created by the system when the document was created.
-- * 'hashType' - The hash type of the document. Valid values include @Sha256@ or @Sha1@ .
-- * 'latestVersion' - The latest version of the document.
-- * 'name' - The name of the Systems Manager document.
-- * 'owner' - The AWS user account that created the document.
-- * 'parameters' - A description of the parameters for a document.
-- * 'platformTypes' - The list of OS platforms compatible with this Systems Manager document.
-- * 'requires' - A list of SSM documents required by a document. For example, an @ApplicationConfiguration@ document requires an @ApplicationConfigurationSchema@ document.
-- * 'schemaVersion' - The schema version.
-- * 'sha1' - The SHA1 hash of the document, which you can use for verification.
-- * 'status' - The status of the Systems Manager document.
-- * 'statusInformation' - A message returned by AWS Systems Manager that explains the @Status@ value. For example, a @Failed@ status might be explained by the @StatusInformation@ message, "The specified S3 bucket does not exist. Verify that the URL of the S3 bucket is correct."
-- * 'tags' - The tags, or metadata, that have been applied to the document.
-- * 'targetType' - The target type which defines the kinds of resources the document can run on. For example, /AWS::EC2::Instance. For a list of valid resource types, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS resource and property types reference> in the /AWS CloudFormation User Guide/ .
-- * 'versionName' - The version of the artifact associated with the document.
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
dStatus :: Lens.Lens' DocumentDescription (Lude.Maybe DocumentStatus)
dStatus = Lens.lens (status :: DocumentDescription -> Lude.Maybe DocumentStatus) (\s a -> s {status = a} :: DocumentDescription)
{-# DEPRECATED dStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The type of document.
--
-- /Note:/ Consider using 'documentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDocumentType :: Lens.Lens' DocumentDescription (Lude.Maybe DocumentType)
dDocumentType = Lens.lens (documentType :: DocumentDescription -> Lude.Maybe DocumentType) (\s a -> s {documentType = a} :: DocumentDescription)
{-# DEPRECATED dDocumentType "Use generic-lens or generic-optics with 'documentType' instead." #-}

-- | The Sha256 or Sha1 hash created by the system when the document was created.
--
-- /Note:/ Consider using 'hash' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dHash :: Lens.Lens' DocumentDescription (Lude.Maybe Lude.Text)
dHash = Lens.lens (hash :: DocumentDescription -> Lude.Maybe Lude.Text) (\s a -> s {hash = a} :: DocumentDescription)
{-# DEPRECATED dHash "Use generic-lens or generic-optics with 'hash' instead." #-}

-- | The version of the artifact associated with the document.
--
-- /Note:/ Consider using 'versionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dVersionName :: Lens.Lens' DocumentDescription (Lude.Maybe Lude.Text)
dVersionName = Lens.lens (versionName :: DocumentDescription -> Lude.Maybe Lude.Text) (\s a -> s {versionName = a} :: DocumentDescription)
{-# DEPRECATED dVersionName "Use generic-lens or generic-optics with 'versionName' instead." #-}

-- | The schema version.
--
-- /Note:/ Consider using 'schemaVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dSchemaVersion :: Lens.Lens' DocumentDescription (Lude.Maybe Lude.Text)
dSchemaVersion = Lens.lens (schemaVersion :: DocumentDescription -> Lude.Maybe Lude.Text) (\s a -> s {schemaVersion = a} :: DocumentDescription)
{-# DEPRECATED dSchemaVersion "Use generic-lens or generic-optics with 'schemaVersion' instead." #-}

-- | The SHA1 hash of the document, which you can use for verification.
--
-- /Note:/ Consider using 'sha1' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dSha1 :: Lens.Lens' DocumentDescription (Lude.Maybe Lude.Text)
dSha1 = Lens.lens (sha1 :: DocumentDescription -> Lude.Maybe Lude.Text) (\s a -> s {sha1 = a} :: DocumentDescription)
{-# DEPRECATED dSha1 "Use generic-lens or generic-optics with 'sha1' instead." #-}

-- | Details about the document attachments, including names, locations, sizes, and so on.
--
-- /Note:/ Consider using 'attachmentsInformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAttachmentsInformation :: Lens.Lens' DocumentDescription (Lude.Maybe [AttachmentInformation])
dAttachmentsInformation = Lens.lens (attachmentsInformation :: DocumentDescription -> Lude.Maybe [AttachmentInformation]) (\s a -> s {attachmentsInformation = a} :: DocumentDescription)
{-# DEPRECATED dAttachmentsInformation "Use generic-lens or generic-optics with 'attachmentsInformation' instead." #-}

-- | The default version.
--
-- /Note:/ Consider using 'defaultVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDefaultVersion :: Lens.Lens' DocumentDescription (Lude.Maybe Lude.Text)
dDefaultVersion = Lens.lens (defaultVersion :: DocumentDescription -> Lude.Maybe Lude.Text) (\s a -> s {defaultVersion = a} :: DocumentDescription)
{-# DEPRECATED dDefaultVersion "Use generic-lens or generic-optics with 'defaultVersion' instead." #-}

-- | The target type which defines the kinds of resources the document can run on. For example, /AWS::EC2::Instance. For a list of valid resource types, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS resource and property types reference> in the /AWS CloudFormation User Guide/ .
--
-- /Note:/ Consider using 'targetType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dTargetType :: Lens.Lens' DocumentDescription (Lude.Maybe Lude.Text)
dTargetType = Lens.lens (targetType :: DocumentDescription -> Lude.Maybe Lude.Text) (\s a -> s {targetType = a} :: DocumentDescription)
{-# DEPRECATED dTargetType "Use generic-lens or generic-optics with 'targetType' instead." #-}

-- | The AWS user account that created the document.
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dOwner :: Lens.Lens' DocumentDescription (Lude.Maybe Lude.Text)
dOwner = Lens.lens (owner :: DocumentDescription -> Lude.Maybe Lude.Text) (\s a -> s {owner = a} :: DocumentDescription)
{-# DEPRECATED dOwner "Use generic-lens or generic-optics with 'owner' instead." #-}

-- | The list of OS platforms compatible with this Systems Manager document.
--
-- /Note:/ Consider using 'platformTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dPlatformTypes :: Lens.Lens' DocumentDescription (Lude.Maybe [PlatformType])
dPlatformTypes = Lens.lens (platformTypes :: DocumentDescription -> Lude.Maybe [PlatformType]) (\s a -> s {platformTypes = a} :: DocumentDescription)
{-# DEPRECATED dPlatformTypes "Use generic-lens or generic-optics with 'platformTypes' instead." #-}

-- | The date when the document was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCreatedDate :: Lens.Lens' DocumentDescription (Lude.Maybe Lude.Timestamp)
dCreatedDate = Lens.lens (createdDate :: DocumentDescription -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdDate = a} :: DocumentDescription)
{-# DEPRECATED dCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | The document format, either JSON or YAML.
--
-- /Note:/ Consider using 'documentFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDocumentFormat :: Lens.Lens' DocumentDescription (Lude.Maybe DocumentFormat)
dDocumentFormat = Lens.lens (documentFormat :: DocumentDescription -> Lude.Maybe DocumentFormat) (\s a -> s {documentFormat = a} :: DocumentDescription)
{-# DEPRECATED dDocumentFormat "Use generic-lens or generic-optics with 'documentFormat' instead." #-}

-- | The name of the Systems Manager document.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dName :: Lens.Lens' DocumentDescription (Lude.Maybe Lude.Text)
dName = Lens.lens (name :: DocumentDescription -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DocumentDescription)
{-# DEPRECATED dName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The hash type of the document. Valid values include @Sha256@ or @Sha1@ .
--
-- /Note:/ Consider using 'hashType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dHashType :: Lens.Lens' DocumentDescription (Lude.Maybe DocumentHashType)
dHashType = Lens.lens (hashType :: DocumentDescription -> Lude.Maybe DocumentHashType) (\s a -> s {hashType = a} :: DocumentDescription)
{-# DEPRECATED dHashType "Use generic-lens or generic-optics with 'hashType' instead." #-}

-- | A description of the parameters for a document.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dParameters :: Lens.Lens' DocumentDescription (Lude.Maybe [DocumentParameter])
dParameters = Lens.lens (parameters :: DocumentDescription -> Lude.Maybe [DocumentParameter]) (\s a -> s {parameters = a} :: DocumentDescription)
{-# DEPRECATED dParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The document version.
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDocumentVersion :: Lens.Lens' DocumentDescription (Lude.Maybe Lude.Text)
dDocumentVersion = Lens.lens (documentVersion :: DocumentDescription -> Lude.Maybe Lude.Text) (\s a -> s {documentVersion = a} :: DocumentDescription)
{-# DEPRECATED dDocumentVersion "Use generic-lens or generic-optics with 'documentVersion' instead." #-}

-- | A message returned by AWS Systems Manager that explains the @Status@ value. For example, a @Failed@ status might be explained by the @StatusInformation@ message, "The specified S3 bucket does not exist. Verify that the URL of the S3 bucket is correct."
--
-- /Note:/ Consider using 'statusInformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dStatusInformation :: Lens.Lens' DocumentDescription (Lude.Maybe Lude.Text)
dStatusInformation = Lens.lens (statusInformation :: DocumentDescription -> Lude.Maybe Lude.Text) (\s a -> s {statusInformation = a} :: DocumentDescription)
{-# DEPRECATED dStatusInformation "Use generic-lens or generic-optics with 'statusInformation' instead." #-}

-- | A description of the document.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDescription :: Lens.Lens' DocumentDescription (Lude.Maybe Lude.Text)
dDescription = Lens.lens (description :: DocumentDescription -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: DocumentDescription)
{-# DEPRECATED dDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A list of SSM documents required by a document. For example, an @ApplicationConfiguration@ document requires an @ApplicationConfigurationSchema@ document.
--
-- /Note:/ Consider using 'requires' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRequires :: Lens.Lens' DocumentDescription (Lude.Maybe (Lude.NonEmpty DocumentRequires))
dRequires = Lens.lens (requires :: DocumentDescription -> Lude.Maybe (Lude.NonEmpty DocumentRequires)) (\s a -> s {requires = a} :: DocumentDescription)
{-# DEPRECATED dRequires "Use generic-lens or generic-optics with 'requires' instead." #-}

-- | The tags, or metadata, that have been applied to the document.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dTags :: Lens.Lens' DocumentDescription (Lude.Maybe [Tag])
dTags = Lens.lens (tags :: DocumentDescription -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: DocumentDescription)
{-# DEPRECATED dTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The latest version of the document.
--
-- /Note:/ Consider using 'latestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dLatestVersion :: Lens.Lens' DocumentDescription (Lude.Maybe Lude.Text)
dLatestVersion = Lens.lens (latestVersion :: DocumentDescription -> Lude.Maybe Lude.Text) (\s a -> s {latestVersion = a} :: DocumentDescription)
{-# DEPRECATED dLatestVersion "Use generic-lens or generic-optics with 'latestVersion' instead." #-}

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
