{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ResourceTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ResourceTags
  ( ResourceTags (..),

    -- * Smart constructor
    mkResourceTags,

    -- * Lenses
    rtARN,
    rtTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The Amazon Resource Name (ARN) and tags for an AWS Elemental MediaConvert resource.
--
-- /See:/ 'mkResourceTags' smart constructor.
data ResourceTags = ResourceTags'
  { arn :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceTags' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the resource.
-- * 'tags' - The tags for the resource.
mkResourceTags ::
  ResourceTags
mkResourceTags =
  ResourceTags' {arn = Lude.Nothing, tags = Lude.Nothing}

-- | The Amazon Resource Name (ARN) of the resource.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtARN :: Lens.Lens' ResourceTags (Lude.Maybe Lude.Text)
rtARN = Lens.lens (arn :: ResourceTags -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: ResourceTags)
{-# DEPRECATED rtARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The tags for the resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtTags :: Lens.Lens' ResourceTags (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
rtTags = Lens.lens (tags :: ResourceTags -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: ResourceTags)
{-# DEPRECATED rtTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON ResourceTags where
  parseJSON =
    Lude.withObject
      "ResourceTags"
      ( \x ->
          ResourceTags'
            Lude.<$> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
      )
