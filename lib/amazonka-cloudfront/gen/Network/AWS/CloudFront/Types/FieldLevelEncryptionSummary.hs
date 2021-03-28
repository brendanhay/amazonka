{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.FieldLevelEncryptionSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.FieldLevelEncryptionSummary
  ( FieldLevelEncryptionSummary (..)
  -- * Smart constructor
  , mkFieldLevelEncryptionSummary
  -- * Lenses
  , flesId
  , flesLastModifiedTime
  , flesComment
  , flesContentTypeProfileConfig
  , flesQueryArgProfileConfig
  ) where

import qualified Network.AWS.CloudFront.Types.ContentTypeProfileConfig as Types
import qualified Network.AWS.CloudFront.Types.QueryArgProfileConfig as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A summary of a field-level encryption item.
--
-- /See:/ 'mkFieldLevelEncryptionSummary' smart constructor.
data FieldLevelEncryptionSummary = FieldLevelEncryptionSummary'
  { id :: Core.Text
    -- ^ The unique ID of a field-level encryption item.
  , lastModifiedTime :: Core.UTCTime
    -- ^ The last time that the summary of field-level encryption items was modified.
  , comment :: Core.Maybe Core.Text
    -- ^ An optional comment about the field-level encryption item.
  , contentTypeProfileConfig :: Core.Maybe Types.ContentTypeProfileConfig
    -- ^ A summary of a content type-profile mapping. 
  , queryArgProfileConfig :: Core.Maybe Types.QueryArgProfileConfig
    -- ^ A summary of a query argument-profile mapping. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'FieldLevelEncryptionSummary' value with any optional fields omitted.
mkFieldLevelEncryptionSummary
    :: Core.Text -- ^ 'id'
    -> Core.UTCTime -- ^ 'lastModifiedTime'
    -> FieldLevelEncryptionSummary
mkFieldLevelEncryptionSummary id lastModifiedTime
  = FieldLevelEncryptionSummary'{id, lastModifiedTime,
                                 comment = Core.Nothing, contentTypeProfileConfig = Core.Nothing,
                                 queryArgProfileConfig = Core.Nothing}

-- | The unique ID of a field-level encryption item.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flesId :: Lens.Lens' FieldLevelEncryptionSummary Core.Text
flesId = Lens.field @"id"
{-# INLINEABLE flesId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The last time that the summary of field-level encryption items was modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flesLastModifiedTime :: Lens.Lens' FieldLevelEncryptionSummary Core.UTCTime
flesLastModifiedTime = Lens.field @"lastModifiedTime"
{-# INLINEABLE flesLastModifiedTime #-}
{-# DEPRECATED lastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead"  #-}

-- | An optional comment about the field-level encryption item.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flesComment :: Lens.Lens' FieldLevelEncryptionSummary (Core.Maybe Core.Text)
flesComment = Lens.field @"comment"
{-# INLINEABLE flesComment #-}
{-# DEPRECATED comment "Use generic-lens or generic-optics with 'comment' instead"  #-}

-- | A summary of a content type-profile mapping. 
--
-- /Note:/ Consider using 'contentTypeProfileConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flesContentTypeProfileConfig :: Lens.Lens' FieldLevelEncryptionSummary (Core.Maybe Types.ContentTypeProfileConfig)
flesContentTypeProfileConfig = Lens.field @"contentTypeProfileConfig"
{-# INLINEABLE flesContentTypeProfileConfig #-}
{-# DEPRECATED contentTypeProfileConfig "Use generic-lens or generic-optics with 'contentTypeProfileConfig' instead"  #-}

-- | A summary of a query argument-profile mapping. 
--
-- /Note:/ Consider using 'queryArgProfileConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flesQueryArgProfileConfig :: Lens.Lens' FieldLevelEncryptionSummary (Core.Maybe Types.QueryArgProfileConfig)
flesQueryArgProfileConfig = Lens.field @"queryArgProfileConfig"
{-# INLINEABLE flesQueryArgProfileConfig #-}
{-# DEPRECATED queryArgProfileConfig "Use generic-lens or generic-optics with 'queryArgProfileConfig' instead"  #-}

instance Core.FromXML FieldLevelEncryptionSummary where
        parseXML x
          = FieldLevelEncryptionSummary' Core.<$>
              (x Core..@ "Id") Core.<*> x Core..@ "LastModifiedTime" Core.<*>
                x Core..@? "Comment"
                Core.<*> x Core..@? "ContentTypeProfileConfig"
                Core.<*> x Core..@? "QueryArgProfileConfig"
