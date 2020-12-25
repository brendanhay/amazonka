{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.StreamingDistributionConfigWithTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.StreamingDistributionConfigWithTags
  ( StreamingDistributionConfigWithTags (..),

    -- * Smart constructor
    mkStreamingDistributionConfigWithTags,

    -- * Lenses
    sdcwtStreamingDistributionConfig,
    sdcwtTags,
  )
where

import qualified Network.AWS.CloudFront.Types.StreamingDistributionConfig as Types
import qualified Network.AWS.CloudFront.Types.Tags as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A streaming distribution Configuration and a list of tags to be associated with the streaming distribution.
--
-- /See:/ 'mkStreamingDistributionConfigWithTags' smart constructor.
data StreamingDistributionConfigWithTags = StreamingDistributionConfigWithTags'
  { -- | A streaming distribution Configuration.
    streamingDistributionConfig :: Types.StreamingDistributionConfig,
    -- | A complex type that contains zero or more @Tag@ elements.
    tags :: Types.Tags
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StreamingDistributionConfigWithTags' value with any optional fields omitted.
mkStreamingDistributionConfigWithTags ::
  -- | 'streamingDistributionConfig'
  Types.StreamingDistributionConfig ->
  -- | 'tags'
  Types.Tags ->
  StreamingDistributionConfigWithTags
mkStreamingDistributionConfigWithTags
  streamingDistributionConfig
  tags =
    StreamingDistributionConfigWithTags'
      { streamingDistributionConfig,
        tags
      }

-- | A streaming distribution Configuration.
--
-- /Note:/ Consider using 'streamingDistributionConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcwtStreamingDistributionConfig :: Lens.Lens' StreamingDistributionConfigWithTags Types.StreamingDistributionConfig
sdcwtStreamingDistributionConfig = Lens.field @"streamingDistributionConfig"
{-# DEPRECATED sdcwtStreamingDistributionConfig "Use generic-lens or generic-optics with 'streamingDistributionConfig' instead." #-}

-- | A complex type that contains zero or more @Tag@ elements.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcwtTags :: Lens.Lens' StreamingDistributionConfigWithTags Types.Tags
sdcwtTags = Lens.field @"tags"
{-# DEPRECATED sdcwtTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.ToXML StreamingDistributionConfigWithTags where
  toXML StreamingDistributionConfigWithTags {..} =
    Core.toXMLNode
      "StreamingDistributionConfig"
      streamingDistributionConfig
      Core.<> Core.toXMLNode "Tags" tags
