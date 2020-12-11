-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.InstanceCountLimits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.InstanceCountLimits
  ( InstanceCountLimits (..),

    -- * Smart constructor
    mkInstanceCountLimits,

    -- * Lenses
    iclMaximumInstanceCount,
    iclMinimumInstanceCount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | InstanceCountLimits represents the limits on number of instances that be created in Amazon Elasticsearch for given InstanceType.
--
-- /See:/ 'mkInstanceCountLimits' smart constructor.
data InstanceCountLimits = InstanceCountLimits'
  { maximumInstanceCount ::
      Lude.Maybe Lude.Int,
    minimumInstanceCount :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceCountLimits' with the minimum fields required to make a request.
--
-- * 'maximumInstanceCount' - Undocumented field.
-- * 'minimumInstanceCount' - Undocumented field.
mkInstanceCountLimits ::
  InstanceCountLimits
mkInstanceCountLimits =
  InstanceCountLimits'
    { maximumInstanceCount = Lude.Nothing,
      minimumInstanceCount = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'maximumInstanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iclMaximumInstanceCount :: Lens.Lens' InstanceCountLimits (Lude.Maybe Lude.Int)
iclMaximumInstanceCount = Lens.lens (maximumInstanceCount :: InstanceCountLimits -> Lude.Maybe Lude.Int) (\s a -> s {maximumInstanceCount = a} :: InstanceCountLimits)
{-# DEPRECATED iclMaximumInstanceCount "Use generic-lens or generic-optics with 'maximumInstanceCount' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'minimumInstanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iclMinimumInstanceCount :: Lens.Lens' InstanceCountLimits (Lude.Maybe Lude.Int)
iclMinimumInstanceCount = Lens.lens (minimumInstanceCount :: InstanceCountLimits -> Lude.Maybe Lude.Int) (\s a -> s {minimumInstanceCount = a} :: InstanceCountLimits)
{-# DEPRECATED iclMinimumInstanceCount "Use generic-lens or generic-optics with 'minimumInstanceCount' instead." #-}

instance Lude.FromJSON InstanceCountLimits where
  parseJSON =
    Lude.withObject
      "InstanceCountLimits"
      ( \x ->
          InstanceCountLimits'
            Lude.<$> (x Lude..:? "MaximumInstanceCount")
            Lude.<*> (x Lude..:? "MinimumInstanceCount")
      )
