{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.StreamingDistributionConfigWithTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.StreamingDistributionConfigWithTags
  ( StreamingDistributionConfigWithTags (..)
  -- * Smart constructor
  , mkStreamingDistributionConfigWithTags
  -- * Lenses
  , sdcwtStreamingDistributionConfig
  , sdcwtTags
  ) where

import qualified Network.AWS.CloudFront.Types.StreamingDistributionConfig as Types
import qualified Network.AWS.CloudFront.Types.Tags as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A streaming distribution Configuration and a list of tags to be associated with the streaming distribution.
--
-- /See:/ 'mkStreamingDistributionConfigWithTags' smart constructor.
data StreamingDistributionConfigWithTags = StreamingDistributionConfigWithTags'
  { streamingDistributionConfig :: Types.StreamingDistributionConfig
    -- ^ A streaming distribution Configuration.
  , tags :: Types.Tags
    -- ^ A complex type that contains zero or more @Tag@ elements.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StreamingDistributionConfigWithTags' value with any optional fields omitted.
mkStreamingDistributionConfigWithTags
    :: Types.StreamingDistributionConfig -- ^ 'streamingDistributionConfig'
    -> Types.Tags -- ^ 'tags'
    -> StreamingDistributionConfigWithTags
mkStreamingDistributionConfigWithTags streamingDistributionConfig
  tags
  = StreamingDistributionConfigWithTags'{streamingDistributionConfig,
                                         tags}

-- | A streaming distribution Configuration.
--
-- /Note:/ Consider using 'streamingDistributionConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcwtStreamingDistributionConfig :: Lens.Lens' StreamingDistributionConfigWithTags Types.StreamingDistributionConfig
sdcwtStreamingDistributionConfig = Lens.field @"streamingDistributionConfig"
{-# INLINEABLE sdcwtStreamingDistributionConfig #-}
{-# DEPRECATED streamingDistributionConfig "Use generic-lens or generic-optics with 'streamingDistributionConfig' instead"  #-}

-- | A complex type that contains zero or more @Tag@ elements.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcwtTags :: Lens.Lens' StreamingDistributionConfigWithTags Types.Tags
sdcwtTags = Lens.field @"tags"
{-# INLINEABLE sdcwtTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToXML StreamingDistributionConfigWithTags where
        toXML StreamingDistributionConfigWithTags{..}
          = Core.toXMLElement "StreamingDistributionConfig"
              streamingDistributionConfig
              Core.<> Core.toXMLElement "Tags" tags
