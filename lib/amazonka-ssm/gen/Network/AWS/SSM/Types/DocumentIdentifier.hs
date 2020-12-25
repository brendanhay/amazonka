{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    diDocumentFormat,
    diDocumentType,
    diDocumentVersion,
    diName,
    diOwner,
    diPlatformTypes,
    diRequires,
    diSchemaVersion,
    diTags,
    diTargetType,
    diVersionName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.DocumentARN as Types
import qualified Network.AWS.SSM.Types.DocumentFormat as Types
import qualified Network.AWS.SSM.Types.DocumentRequires as Types
import qualified Network.AWS.SSM.Types.DocumentSchemaVersion as Types
import qualified Network.AWS.SSM.Types.DocumentType as Types
import qualified Network.AWS.SSM.Types.DocumentVersion as Types
import qualified Network.AWS.SSM.Types.DocumentVersionName as Types
import qualified Network.AWS.SSM.Types.Owner as Types
import qualified Network.AWS.SSM.Types.PlatformType as Types
import qualified Network.AWS.SSM.Types.Tag as Types
import qualified Network.AWS.SSM.Types.TargetType as Types

-- | Describes the name of a Systems Manager document.
--
-- /See:/ 'mkDocumentIdentifier' smart constructor.
data DocumentIdentifier = DocumentIdentifier'
  { -- | The document format, either JSON or YAML.
    documentFormat :: Core.Maybe Types.DocumentFormat,
    -- | The document type.
    documentType :: Core.Maybe Types.DocumentType,
    -- | The document version.
    documentVersion :: Core.Maybe Types.DocumentVersion,
    -- | The name of the Systems Manager document.
    name :: Core.Maybe Types.DocumentARN,
    -- | The AWS user account that created the document.
    owner :: Core.Maybe Types.Owner,
    -- | The operating system platform.
    platformTypes :: Core.Maybe [Types.PlatformType],
    -- | A list of SSM documents required by a document. For example, an @ApplicationConfiguration@ document requires an @ApplicationConfigurationSchema@ document.
    requires :: Core.Maybe (Core.NonEmpty Types.DocumentRequires),
    -- | The schema version.
    schemaVersion :: Core.Maybe Types.DocumentSchemaVersion,
    -- | The tags, or metadata, that have been applied to the document.
    tags :: Core.Maybe [Types.Tag],
    -- | The target type which defines the kinds of resources the document can run on. For example, /AWS::EC2::Instance. For a list of valid resource types, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS resource and property types reference> in the /AWS CloudFormation User Guide/ .
    targetType :: Core.Maybe Types.TargetType,
    -- | An optional field specifying the version of the artifact associated with the document. For example, "Release 12, Update 6". This value is unique across all versions of a document, and cannot be changed.
    versionName :: Core.Maybe Types.DocumentVersionName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DocumentIdentifier' value with any optional fields omitted.
mkDocumentIdentifier ::
  DocumentIdentifier
mkDocumentIdentifier =
  DocumentIdentifier'
    { documentFormat = Core.Nothing,
      documentType = Core.Nothing,
      documentVersion = Core.Nothing,
      name = Core.Nothing,
      owner = Core.Nothing,
      platformTypes = Core.Nothing,
      requires = Core.Nothing,
      schemaVersion = Core.Nothing,
      tags = Core.Nothing,
      targetType = Core.Nothing,
      versionName = Core.Nothing
    }

-- | The document format, either JSON or YAML.
--
-- /Note:/ Consider using 'documentFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diDocumentFormat :: Lens.Lens' DocumentIdentifier (Core.Maybe Types.DocumentFormat)
diDocumentFormat = Lens.field @"documentFormat"
{-# DEPRECATED diDocumentFormat "Use generic-lens or generic-optics with 'documentFormat' instead." #-}

-- | The document type.
--
-- /Note:/ Consider using 'documentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diDocumentType :: Lens.Lens' DocumentIdentifier (Core.Maybe Types.DocumentType)
diDocumentType = Lens.field @"documentType"
{-# DEPRECATED diDocumentType "Use generic-lens or generic-optics with 'documentType' instead." #-}

-- | The document version.
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diDocumentVersion :: Lens.Lens' DocumentIdentifier (Core.Maybe Types.DocumentVersion)
diDocumentVersion = Lens.field @"documentVersion"
{-# DEPRECATED diDocumentVersion "Use generic-lens or generic-optics with 'documentVersion' instead." #-}

-- | The name of the Systems Manager document.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diName :: Lens.Lens' DocumentIdentifier (Core.Maybe Types.DocumentARN)
diName = Lens.field @"name"
{-# DEPRECATED diName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The AWS user account that created the document.
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diOwner :: Lens.Lens' DocumentIdentifier (Core.Maybe Types.Owner)
diOwner = Lens.field @"owner"
{-# DEPRECATED diOwner "Use generic-lens or generic-optics with 'owner' instead." #-}

-- | The operating system platform.
--
-- /Note:/ Consider using 'platformTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diPlatformTypes :: Lens.Lens' DocumentIdentifier (Core.Maybe [Types.PlatformType])
diPlatformTypes = Lens.field @"platformTypes"
{-# DEPRECATED diPlatformTypes "Use generic-lens or generic-optics with 'platformTypes' instead." #-}

-- | A list of SSM documents required by a document. For example, an @ApplicationConfiguration@ document requires an @ApplicationConfigurationSchema@ document.
--
-- /Note:/ Consider using 'requires' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diRequires :: Lens.Lens' DocumentIdentifier (Core.Maybe (Core.NonEmpty Types.DocumentRequires))
diRequires = Lens.field @"requires"
{-# DEPRECATED diRequires "Use generic-lens or generic-optics with 'requires' instead." #-}

-- | The schema version.
--
-- /Note:/ Consider using 'schemaVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diSchemaVersion :: Lens.Lens' DocumentIdentifier (Core.Maybe Types.DocumentSchemaVersion)
diSchemaVersion = Lens.field @"schemaVersion"
{-# DEPRECATED diSchemaVersion "Use generic-lens or generic-optics with 'schemaVersion' instead." #-}

-- | The tags, or metadata, that have been applied to the document.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diTags :: Lens.Lens' DocumentIdentifier (Core.Maybe [Types.Tag])
diTags = Lens.field @"tags"
{-# DEPRECATED diTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The target type which defines the kinds of resources the document can run on. For example, /AWS::EC2::Instance. For a list of valid resource types, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS resource and property types reference> in the /AWS CloudFormation User Guide/ .
--
-- /Note:/ Consider using 'targetType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diTargetType :: Lens.Lens' DocumentIdentifier (Core.Maybe Types.TargetType)
diTargetType = Lens.field @"targetType"
{-# DEPRECATED diTargetType "Use generic-lens or generic-optics with 'targetType' instead." #-}

-- | An optional field specifying the version of the artifact associated with the document. For example, "Release 12, Update 6". This value is unique across all versions of a document, and cannot be changed.
--
-- /Note:/ Consider using 'versionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diVersionName :: Lens.Lens' DocumentIdentifier (Core.Maybe Types.DocumentVersionName)
diVersionName = Lens.field @"versionName"
{-# DEPRECATED diVersionName "Use generic-lens or generic-optics with 'versionName' instead." #-}

instance Core.FromJSON DocumentIdentifier where
  parseJSON =
    Core.withObject "DocumentIdentifier" Core.$
      \x ->
        DocumentIdentifier'
          Core.<$> (x Core..:? "DocumentFormat")
          Core.<*> (x Core..:? "DocumentType")
          Core.<*> (x Core..:? "DocumentVersion")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "Owner")
          Core.<*> (x Core..:? "PlatformTypes")
          Core.<*> (x Core..:? "Requires")
          Core.<*> (x Core..:? "SchemaVersion")
          Core.<*> (x Core..:? "Tags")
          Core.<*> (x Core..:? "TargetType")
          Core.<*> (x Core..:? "VersionName")
