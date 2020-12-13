{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains.Types.Bucket
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearchDomains.Types.Bucket
  ( Bucket (..),

    -- * Smart constructor
    mkBucket,

    -- * Lenses
    bValue,
    bCount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A container for facet information.
--
-- /See:/ 'mkBucket' smart constructor.
data Bucket = Bucket'
  { -- | The facet value being counted.
    value :: Lude.Maybe Lude.Text,
    -- | The number of hits that contain the facet value in the specified facet field.
    count :: Lude.Maybe Lude.Integer
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Bucket' with the minimum fields required to make a request.
--
-- * 'value' - The facet value being counted.
-- * 'count' - The number of hits that contain the facet value in the specified facet field.
mkBucket ::
  Bucket
mkBucket = Bucket' {value = Lude.Nothing, count = Lude.Nothing}

-- | The facet value being counted.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bValue :: Lens.Lens' Bucket (Lude.Maybe Lude.Text)
bValue = Lens.lens (value :: Bucket -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: Bucket)
{-# DEPRECATED bValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The number of hits that contain the facet value in the specified facet field.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bCount :: Lens.Lens' Bucket (Lude.Maybe Lude.Integer)
bCount = Lens.lens (count :: Bucket -> Lude.Maybe Lude.Integer) (\s a -> s {count = a} :: Bucket)
{-# DEPRECATED bCount "Use generic-lens or generic-optics with 'count' instead." #-}

instance Lude.FromJSON Bucket where
  parseJSON =
    Lude.withObject
      "Bucket"
      ( \x ->
          Bucket'
            Lude.<$> (x Lude..:? "value") Lude.<*> (x Lude..:? "count")
      )
