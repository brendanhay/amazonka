-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.DocumentIdentifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.DocumentIdentifier
  ( DocumentIdentifier (..),

    -- * Smart constructor
    mkDocumentIdentifier,

    -- * Lenses
    diDocumentType,
    diVersionName,
    diSchemaVersion,
    diTargetType,
    diOwner,
    diPlatformTypes,
    diDocumentFormat,
    diName,
    diDocumentVersion,
    diRequires,
    diTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.DocumentFormat
import Network.AWS.SSM.Types.DocumentRequires
import Network.AWS.SSM.Types.DocumentType
import Network.AWS.SSM.Types.PlatformType
import Network.AWS.SSM.Types.Tag

-- | Describes the name of a Systems Manager document.
--
-- /See:/ 'mkDocumentIdentifier' smart constructor.
data DocumentIdentifier = DocumentIdentifier'
  { documentType ::
      Lude.Maybe DocumentType,
    versionName :: Lude.Maybe Lude.Text,
    schemaVersion :: Lude.Maybe Lude.Text,
    targetType :: Lude.Maybe Lude.Text,
    owner :: Lude.Maybe Lude.Text,
    platformTypes :: Lude.Maybe [PlatformType],
    documentFormat :: Lude.Maybe DocumentFormat,
    name :: Lude.Maybe Lude.Text,
    documentVersion :: Lude.Maybe Lude.Text,
    requires ::
      Lude.Maybe (Lude.NonEmpty DocumentRequires),
    tags :: Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DocumentIdentifier' with the minimum fields required to make a request.
--
-- * 'documentFormat' - The document format, either JSON or YAML.
-- * 'documentType' - The document type.
-- * 'documentVersion' - The document version.
-- * 'name' - The name of the Systems Manager document.
-- * 'owner' - The AWS user account that created the document.
-- * 'platformTypes' - The operating system platform.
-- * 'requires' - A list of SSM documents required by a document. For example, an @ApplicationConfiguration@ document requires an @ApplicationConfigurationSchema@ document.
-- * 'schemaVersion' - The schema version.
-- * 'tags' - The tags, or metadata, that have been applied to the document.
-- * 'targetType' - The target type which defines the kinds of resources the document can run on. For example, /AWS::EC2::Instance. For a list of valid resource types, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS resource and property types reference> in the /AWS CloudFormation User Guide/ .
-- * 'versionName' - An optional field specifying the version of the artifact associated with the document. For example, "Release 12, Update 6". This value is unique across all versions of a document, and cannot be changed.
mkDocumentIdentifier ::
  DocumentIdentifier
mkDocumentIdentifier =
  DocumentIdentifier'
    { documentType = Lude.Nothing,
      versionName = Lude.Nothing,
      schemaVersion = Lude.Nothing,
      targetType = Lude.Nothing,
      owner = Lude.Nothing,
      platformTypes = Lude.Nothing,
      documentFormat = Lude.Nothing,
      name = Lude.Nothing,
      documentVersion = Lude.Nothing,
      requires = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The document type.
--
-- /Note:/ Consider using 'documentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diDocumentType :: Lens.Lens' DocumentIdentifier (Lude.Maybe DocumentType)
diDocumentType = Lens.lens (documentType :: DocumentIdentifier -> Lude.Maybe DocumentType) (\s a -> s {documentType = a} :: DocumentIdentifier)
{-# DEPRECATED diDocumentType "Use generic-lens or generic-optics with 'documentType' instead." #-}

-- | An optional field specifying the version of the artifact associated with the document. For example, "Release 12, Update 6". This value is unique across all versions of a document, and cannot be changed.
--
-- /Note:/ Consider using 'versionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diVersionName :: Lens.Lens' DocumentIdentifier (Lude.Maybe Lude.Text)
diVersionName = Lens.lens (versionName :: DocumentIdentifier -> Lude.Maybe Lude.Text) (\s a -> s {versionName = a} :: DocumentIdentifier)
{-# DEPRECATED diVersionName "Use generic-lens or generic-optics with 'versionName' instead." #-}

-- | The schema version.
--
-- /Note:/ Consider using 'schemaVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diSchemaVersion :: Lens.Lens' DocumentIdentifier (Lude.Maybe Lude.Text)
diSchemaVersion = Lens.lens (schemaVersion :: DocumentIdentifier -> Lude.Maybe Lude.Text) (\s a -> s {schemaVersion = a} :: DocumentIdentifier)
{-# DEPRECATED diSchemaVersion "Use generic-lens or generic-optics with 'schemaVersion' instead." #-}

-- | The target type which defines the kinds of resources the document can run on. For example, /AWS::EC2::Instance. For a list of valid resource types, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS resource and property types reference> in the /AWS CloudFormation User Guide/ .
--
-- /Note:/ Consider using 'targetType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diTargetType :: Lens.Lens' DocumentIdentifier (Lude.Maybe Lude.Text)
diTargetType = Lens.lens (targetType :: DocumentIdentifier -> Lude.Maybe Lude.Text) (\s a -> s {targetType = a} :: DocumentIdentifier)
{-# DEPRECATED diTargetType "Use generic-lens or generic-optics with 'targetType' instead." #-}

-- | The AWS user account that created the document.
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diOwner :: Lens.Lens' DocumentIdentifier (Lude.Maybe Lude.Text)
diOwner = Lens.lens (owner :: DocumentIdentifier -> Lude.Maybe Lude.Text) (\s a -> s {owner = a} :: DocumentIdentifier)
{-# DEPRECATED diOwner "Use generic-lens or generic-optics with 'owner' instead." #-}

-- | The operating system platform.
--
-- /Note:/ Consider using 'platformTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diPlatformTypes :: Lens.Lens' DocumentIdentifier (Lude.Maybe [PlatformType])
diPlatformTypes = Lens.lens (platformTypes :: DocumentIdentifier -> Lude.Maybe [PlatformType]) (\s a -> s {platformTypes = a} :: DocumentIdentifier)
{-# DEPRECATED diPlatformTypes "Use generic-lens or generic-optics with 'platformTypes' instead." #-}

-- | The document format, either JSON or YAML.
--
-- /Note:/ Consider using 'documentFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diDocumentFormat :: Lens.Lens' DocumentIdentifier (Lude.Maybe DocumentFormat)
diDocumentFormat = Lens.lens (documentFormat :: DocumentIdentifier -> Lude.Maybe DocumentFormat) (\s a -> s {documentFormat = a} :: DocumentIdentifier)
{-# DEPRECATED diDocumentFormat "Use generic-lens or generic-optics with 'documentFormat' instead." #-}

-- | The name of the Systems Manager document.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diName :: Lens.Lens' DocumentIdentifier (Lude.Maybe Lude.Text)
diName = Lens.lens (name :: DocumentIdentifier -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DocumentIdentifier)
{-# DEPRECATED diName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The document version.
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diDocumentVersion :: Lens.Lens' DocumentIdentifier (Lude.Maybe Lude.Text)
diDocumentVersion = Lens.lens (documentVersion :: DocumentIdentifier -> Lude.Maybe Lude.Text) (\s a -> s {documentVersion = a} :: DocumentIdentifier)
{-# DEPRECATED diDocumentVersion "Use generic-lens or generic-optics with 'documentVersion' instead." #-}

-- | A list of SSM documents required by a document. For example, an @ApplicationConfiguration@ document requires an @ApplicationConfigurationSchema@ document.
--
-- /Note:/ Consider using 'requires' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diRequires :: Lens.Lens' DocumentIdentifier (Lude.Maybe (Lude.NonEmpty DocumentRequires))
diRequires = Lens.lens (requires :: DocumentIdentifier -> Lude.Maybe (Lude.NonEmpty DocumentRequires)) (\s a -> s {requires = a} :: DocumentIdentifier)
{-# DEPRECATED diRequires "Use generic-lens or generic-optics with 'requires' instead." #-}

-- | The tags, or metadata, that have been applied to the document.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diTags :: Lens.Lens' DocumentIdentifier (Lude.Maybe [Tag])
diTags = Lens.lens (tags :: DocumentIdentifier -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: DocumentIdentifier)
{-# DEPRECATED diTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON DocumentIdentifier where
  parseJSON =
    Lude.withObject
      "DocumentIdentifier"
      ( \x ->
          DocumentIdentifier'
            Lude.<$> (x Lude..:? "DocumentType")
            Lude.<*> (x Lude..:? "VersionName")
            Lude.<*> (x Lude..:? "SchemaVersion")
            Lude.<*> (x Lude..:? "TargetType")
            Lude.<*> (x Lude..:? "Owner")
            Lude.<*> (x Lude..:? "PlatformTypes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "DocumentFormat")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "DocumentVersion")
            Lude.<*> (x Lude..:? "Requires")
            Lude.<*> (x Lude..:? "Tags" Lude..!= Lude.mempty)
      )
