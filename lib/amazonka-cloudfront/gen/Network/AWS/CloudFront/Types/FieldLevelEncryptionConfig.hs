{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.FieldLevelEncryptionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.FieldLevelEncryptionConfig
  ( FieldLevelEncryptionConfig (..)
  -- * Smart constructor
  , mkFieldLevelEncryptionConfig
  -- * Lenses
  , flecCallerReference
  , flecComment
  , flecContentTypeProfileConfig
  , flecQueryArgProfileConfig
  ) where

import qualified Network.AWS.CloudFront.Types.ContentTypeProfileConfig as Types
import qualified Network.AWS.CloudFront.Types.QueryArgProfileConfig as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A complex data type that includes the profile configurations specified for field-level encryption. 
--
-- /See:/ 'mkFieldLevelEncryptionConfig' smart constructor.
data FieldLevelEncryptionConfig = FieldLevelEncryptionConfig'
  { callerReference :: Core.Text
    -- ^ A unique number that ensures the request can't be replayed.
  , comment :: Core.Maybe Core.Text
    -- ^ An optional comment about the configuration.
  , contentTypeProfileConfig :: Core.Maybe Types.ContentTypeProfileConfig
    -- ^ A complex data type that specifies when to forward content if a content type isn't recognized and profiles to use as by default in a request if a query argument doesn't specify a profile to use.
  , queryArgProfileConfig :: Core.Maybe Types.QueryArgProfileConfig
    -- ^ A complex data type that specifies when to forward content if a profile isn't found and the profile that can be provided as a query argument in a request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FieldLevelEncryptionConfig' value with any optional fields omitted.
mkFieldLevelEncryptionConfig
    :: Core.Text -- ^ 'callerReference'
    -> FieldLevelEncryptionConfig
mkFieldLevelEncryptionConfig callerReference
  = FieldLevelEncryptionConfig'{callerReference,
                                comment = Core.Nothing, contentTypeProfileConfig = Core.Nothing,
                                queryArgProfileConfig = Core.Nothing}

-- | A unique number that ensures the request can't be replayed.
--
-- /Note:/ Consider using 'callerReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flecCallerReference :: Lens.Lens' FieldLevelEncryptionConfig Core.Text
flecCallerReference = Lens.field @"callerReference"
{-# INLINEABLE flecCallerReference #-}
{-# DEPRECATED callerReference "Use generic-lens or generic-optics with 'callerReference' instead"  #-}

-- | An optional comment about the configuration.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flecComment :: Lens.Lens' FieldLevelEncryptionConfig (Core.Maybe Core.Text)
flecComment = Lens.field @"comment"
{-# INLINEABLE flecComment #-}
{-# DEPRECATED comment "Use generic-lens or generic-optics with 'comment' instead"  #-}

-- | A complex data type that specifies when to forward content if a content type isn't recognized and profiles to use as by default in a request if a query argument doesn't specify a profile to use.
--
-- /Note:/ Consider using 'contentTypeProfileConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flecContentTypeProfileConfig :: Lens.Lens' FieldLevelEncryptionConfig (Core.Maybe Types.ContentTypeProfileConfig)
flecContentTypeProfileConfig = Lens.field @"contentTypeProfileConfig"
{-# INLINEABLE flecContentTypeProfileConfig #-}
{-# DEPRECATED contentTypeProfileConfig "Use generic-lens or generic-optics with 'contentTypeProfileConfig' instead"  #-}

-- | A complex data type that specifies when to forward content if a profile isn't found and the profile that can be provided as a query argument in a request.
--
-- /Note:/ Consider using 'queryArgProfileConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flecQueryArgProfileConfig :: Lens.Lens' FieldLevelEncryptionConfig (Core.Maybe Types.QueryArgProfileConfig)
flecQueryArgProfileConfig = Lens.field @"queryArgProfileConfig"
{-# INLINEABLE flecQueryArgProfileConfig #-}
{-# DEPRECATED queryArgProfileConfig "Use generic-lens or generic-optics with 'queryArgProfileConfig' instead"  #-}

instance Core.ToXML FieldLevelEncryptionConfig where
        toXML FieldLevelEncryptionConfig{..}
          = Core.toXMLElement "CallerReference" callerReference Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "Comment") comment
              Core.<>
              Core.maybe Core.mempty
                (Core.toXMLElement "ContentTypeProfileConfig")
                contentTypeProfileConfig
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "QueryArgProfileConfig")
                queryArgProfileConfig

instance Core.FromXML FieldLevelEncryptionConfig where
        parseXML x
          = FieldLevelEncryptionConfig' Core.<$>
              (x Core..@ "CallerReference") Core.<*> x Core..@? "Comment"
                Core.<*> x Core..@? "ContentTypeProfileConfig"
                Core.<*> x Core..@? "QueryArgProfileConfig"
