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

import Network.AWS.CloudFront.Types.StreamingDistributionConfig
import Network.AWS.CloudFront.Types.Tags
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A streaming distribution Configuration and a list of tags to be associated with the streaming distribution.
--
-- /See:/ 'mkStreamingDistributionConfigWithTags' smart constructor.
data StreamingDistributionConfigWithTags = StreamingDistributionConfigWithTags'
  { -- | A streaming distribution Configuration.
    streamingDistributionConfig :: StreamingDistributionConfig,
    -- | A complex type that contains zero or more @Tag@ elements.
    tags :: Tags
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StreamingDistributionConfigWithTags' with the minimum fields required to make a request.
--
-- * 'streamingDistributionConfig' - A streaming distribution Configuration.
-- * 'tags' - A complex type that contains zero or more @Tag@ elements.
mkStreamingDistributionConfigWithTags ::
  -- | 'streamingDistributionConfig'
  StreamingDistributionConfig ->
  -- | 'tags'
  Tags ->
  StreamingDistributionConfigWithTags
mkStreamingDistributionConfigWithTags
  pStreamingDistributionConfig_
  pTags_ =
    StreamingDistributionConfigWithTags'
      { streamingDistributionConfig =
          pStreamingDistributionConfig_,
        tags = pTags_
      }

-- | A streaming distribution Configuration.
--
-- /Note:/ Consider using 'streamingDistributionConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcwtStreamingDistributionConfig :: Lens.Lens' StreamingDistributionConfigWithTags StreamingDistributionConfig
sdcwtStreamingDistributionConfig = Lens.lens (streamingDistributionConfig :: StreamingDistributionConfigWithTags -> StreamingDistributionConfig) (\s a -> s {streamingDistributionConfig = a} :: StreamingDistributionConfigWithTags)
{-# DEPRECATED sdcwtStreamingDistributionConfig "Use generic-lens or generic-optics with 'streamingDistributionConfig' instead." #-}

-- | A complex type that contains zero or more @Tag@ elements.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcwtTags :: Lens.Lens' StreamingDistributionConfigWithTags Tags
sdcwtTags = Lens.lens (tags :: StreamingDistributionConfigWithTags -> Tags) (\s a -> s {tags = a} :: StreamingDistributionConfigWithTags)
{-# DEPRECATED sdcwtTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.ToXML StreamingDistributionConfigWithTags where
  toXML StreamingDistributionConfigWithTags' {..} =
    Lude.mconcat
      [ "StreamingDistributionConfig" Lude.@= streamingDistributionConfig,
        "Tags" Lude.@= tags
      ]
