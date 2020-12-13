{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.DistributionConfigWithTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.DistributionConfigWithTags
  ( DistributionConfigWithTags (..),

    -- * Smart constructor
    mkDistributionConfigWithTags,

    -- * Lenses
    dcwtDistributionConfig,
    dcwtTags,
  )
where

import Network.AWS.CloudFront.Types.DistributionConfig
import Network.AWS.CloudFront.Types.Tags
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A distribution Configuration and a list of tags to be associated with the distribution.
--
-- /See:/ 'mkDistributionConfigWithTags' smart constructor.
data DistributionConfigWithTags = DistributionConfigWithTags'
  { -- | A distribution configuration.
    distributionConfig :: DistributionConfig,
    -- | A complex type that contains zero or more @Tag@ elements.
    tags :: Tags
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DistributionConfigWithTags' with the minimum fields required to make a request.
--
-- * 'distributionConfig' - A distribution configuration.
-- * 'tags' - A complex type that contains zero or more @Tag@ elements.
mkDistributionConfigWithTags ::
  -- | 'distributionConfig'
  DistributionConfig ->
  -- | 'tags'
  Tags ->
  DistributionConfigWithTags
mkDistributionConfigWithTags pDistributionConfig_ pTags_ =
  DistributionConfigWithTags'
    { distributionConfig =
        pDistributionConfig_,
      tags = pTags_
    }

-- | A distribution configuration.
--
-- /Note:/ Consider using 'distributionConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcwtDistributionConfig :: Lens.Lens' DistributionConfigWithTags DistributionConfig
dcwtDistributionConfig = Lens.lens (distributionConfig :: DistributionConfigWithTags -> DistributionConfig) (\s a -> s {distributionConfig = a} :: DistributionConfigWithTags)
{-# DEPRECATED dcwtDistributionConfig "Use generic-lens or generic-optics with 'distributionConfig' instead." #-}

-- | A complex type that contains zero or more @Tag@ elements.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcwtTags :: Lens.Lens' DistributionConfigWithTags Tags
dcwtTags = Lens.lens (tags :: DistributionConfigWithTags -> Tags) (\s a -> s {tags = a} :: DistributionConfigWithTags)
{-# DEPRECATED dcwtTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.ToXML DistributionConfigWithTags where
  toXML DistributionConfigWithTags' {..} =
    Lude.mconcat
      [ "DistributionConfig" Lude.@= distributionConfig,
        "Tags" Lude.@= tags
      ]
