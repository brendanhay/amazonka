{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.DocumentVersionInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.DocumentVersionInfo
  ( DocumentVersionInfo (..)
  -- * Smart constructor
  , mkDocumentVersionInfo
  -- * Lenses
  , dviCreatedDate
  , dviDocumentFormat
  , dviDocumentVersion
  , dviIsDefaultVersion
  , dviName
  , dviStatus
  , dviStatusInformation
  , dviVersionName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.DocumentFormat as Types
import qualified Network.AWS.SSM.Types.DocumentName as Types
import qualified Network.AWS.SSM.Types.DocumentStatus as Types
import qualified Network.AWS.SSM.Types.DocumentStatusInformation as Types
import qualified Network.AWS.SSM.Types.DocumentVersion as Types
import qualified Network.AWS.SSM.Types.DocumentVersionName as Types

-- | Version information about the document.
--
-- /See:/ 'mkDocumentVersionInfo' smart constructor.
data DocumentVersionInfo = DocumentVersionInfo'
  { createdDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date the document was created.
  , documentFormat :: Core.Maybe Types.DocumentFormat
    -- ^ The document format, either JSON or YAML.
  , documentVersion :: Core.Maybe Types.DocumentVersion
    -- ^ The document version.
  , isDefaultVersion :: Core.Maybe Core.Bool
    -- ^ An identifier for the default version of the document.
  , name :: Core.Maybe Types.DocumentName
    -- ^ The document name.
  , status :: Core.Maybe Types.DocumentStatus
    -- ^ The status of the Systems Manager document, such as @Creating@ , @Active@ , @Failed@ , and @Deleting@ .
  , statusInformation :: Core.Maybe Types.DocumentStatusInformation
    -- ^ A message returned by AWS Systems Manager that explains the @Status@ value. For example, a @Failed@ status might be explained by the @StatusInformation@ message, "The specified S3 bucket does not exist. Verify that the URL of the S3 bucket is correct."
  , versionName :: Core.Maybe Types.DocumentVersionName
    -- ^ The version of the artifact associated with the document. For example, "Release 12, Update 6". This value is unique across all versions of a document, and cannot be changed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DocumentVersionInfo' value with any optional fields omitted.
mkDocumentVersionInfo
    :: DocumentVersionInfo
mkDocumentVersionInfo
  = DocumentVersionInfo'{createdDate = Core.Nothing,
                         documentFormat = Core.Nothing, documentVersion = Core.Nothing,
                         isDefaultVersion = Core.Nothing, name = Core.Nothing,
                         status = Core.Nothing, statusInformation = Core.Nothing,
                         versionName = Core.Nothing}

-- | The date the document was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dviCreatedDate :: Lens.Lens' DocumentVersionInfo (Core.Maybe Core.NominalDiffTime)
dviCreatedDate = Lens.field @"createdDate"
{-# INLINEABLE dviCreatedDate #-}
{-# DEPRECATED createdDate "Use generic-lens or generic-optics with 'createdDate' instead"  #-}

-- | The document format, either JSON or YAML.
--
-- /Note:/ Consider using 'documentFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dviDocumentFormat :: Lens.Lens' DocumentVersionInfo (Core.Maybe Types.DocumentFormat)
dviDocumentFormat = Lens.field @"documentFormat"
{-# INLINEABLE dviDocumentFormat #-}
{-# DEPRECATED documentFormat "Use generic-lens or generic-optics with 'documentFormat' instead"  #-}

-- | The document version.
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dviDocumentVersion :: Lens.Lens' DocumentVersionInfo (Core.Maybe Types.DocumentVersion)
dviDocumentVersion = Lens.field @"documentVersion"
{-# INLINEABLE dviDocumentVersion #-}
{-# DEPRECATED documentVersion "Use generic-lens or generic-optics with 'documentVersion' instead"  #-}

-- | An identifier for the default version of the document.
--
-- /Note:/ Consider using 'isDefaultVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dviIsDefaultVersion :: Lens.Lens' DocumentVersionInfo (Core.Maybe Core.Bool)
dviIsDefaultVersion = Lens.field @"isDefaultVersion"
{-# INLINEABLE dviIsDefaultVersion #-}
{-# DEPRECATED isDefaultVersion "Use generic-lens or generic-optics with 'isDefaultVersion' instead"  #-}

-- | The document name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dviName :: Lens.Lens' DocumentVersionInfo (Core.Maybe Types.DocumentName)
dviName = Lens.field @"name"
{-# INLINEABLE dviName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The status of the Systems Manager document, such as @Creating@ , @Active@ , @Failed@ , and @Deleting@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dviStatus :: Lens.Lens' DocumentVersionInfo (Core.Maybe Types.DocumentStatus)
dviStatus = Lens.field @"status"
{-# INLINEABLE dviStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | A message returned by AWS Systems Manager that explains the @Status@ value. For example, a @Failed@ status might be explained by the @StatusInformation@ message, "The specified S3 bucket does not exist. Verify that the URL of the S3 bucket is correct."
--
-- /Note:/ Consider using 'statusInformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dviStatusInformation :: Lens.Lens' DocumentVersionInfo (Core.Maybe Types.DocumentStatusInformation)
dviStatusInformation = Lens.field @"statusInformation"
{-# INLINEABLE dviStatusInformation #-}
{-# DEPRECATED statusInformation "Use generic-lens or generic-optics with 'statusInformation' instead"  #-}

-- | The version of the artifact associated with the document. For example, "Release 12, Update 6". This value is unique across all versions of a document, and cannot be changed.
--
-- /Note:/ Consider using 'versionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dviVersionName :: Lens.Lens' DocumentVersionInfo (Core.Maybe Types.DocumentVersionName)
dviVersionName = Lens.field @"versionName"
{-# INLINEABLE dviVersionName #-}
{-# DEPRECATED versionName "Use generic-lens or generic-optics with 'versionName' instead"  #-}

instance Core.FromJSON DocumentVersionInfo where
        parseJSON
          = Core.withObject "DocumentVersionInfo" Core.$
              \ x ->
                DocumentVersionInfo' Core.<$>
                  (x Core..:? "CreatedDate") Core.<*> x Core..:? "DocumentFormat"
                    Core.<*> x Core..:? "DocumentVersion"
                    Core.<*> x Core..:? "IsDefaultVersion"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "Status"
                    Core.<*> x Core..:? "StatusInformation"
                    Core.<*> x Core..:? "VersionName"
