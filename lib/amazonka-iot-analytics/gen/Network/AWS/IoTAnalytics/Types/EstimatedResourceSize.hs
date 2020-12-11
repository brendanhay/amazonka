-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.EstimatedResourceSize
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.EstimatedResourceSize
  ( EstimatedResourceSize (..),

    -- * Smart constructor
    mkEstimatedResourceSize,

    -- * Lenses
    ersEstimatedOn,
    ersEstimatedSizeInBytes,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The estimated size of the resource.
--
-- /See:/ 'mkEstimatedResourceSize' smart constructor.
data EstimatedResourceSize = EstimatedResourceSize'
  { estimatedOn ::
      Lude.Maybe Lude.Timestamp,
    estimatedSizeInBytes :: Lude.Maybe Lude.Double
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EstimatedResourceSize' with the minimum fields required to make a request.
--
-- * 'estimatedOn' - The time when the estimate of the size of the resource was made.
-- * 'estimatedSizeInBytes' - The estimated size of the resource, in bytes.
mkEstimatedResourceSize ::
  EstimatedResourceSize
mkEstimatedResourceSize =
  EstimatedResourceSize'
    { estimatedOn = Lude.Nothing,
      estimatedSizeInBytes = Lude.Nothing
    }

-- | The time when the estimate of the size of the resource was made.
--
-- /Note:/ Consider using 'estimatedOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ersEstimatedOn :: Lens.Lens' EstimatedResourceSize (Lude.Maybe Lude.Timestamp)
ersEstimatedOn = Lens.lens (estimatedOn :: EstimatedResourceSize -> Lude.Maybe Lude.Timestamp) (\s a -> s {estimatedOn = a} :: EstimatedResourceSize)
{-# DEPRECATED ersEstimatedOn "Use generic-lens or generic-optics with 'estimatedOn' instead." #-}

-- | The estimated size of the resource, in bytes.
--
-- /Note:/ Consider using 'estimatedSizeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ersEstimatedSizeInBytes :: Lens.Lens' EstimatedResourceSize (Lude.Maybe Lude.Double)
ersEstimatedSizeInBytes = Lens.lens (estimatedSizeInBytes :: EstimatedResourceSize -> Lude.Maybe Lude.Double) (\s a -> s {estimatedSizeInBytes = a} :: EstimatedResourceSize)
{-# DEPRECATED ersEstimatedSizeInBytes "Use generic-lens or generic-optics with 'estimatedSizeInBytes' instead." #-}

instance Lude.FromJSON EstimatedResourceSize where
  parseJSON =
    Lude.withObject
      "EstimatedResourceSize"
      ( \x ->
          EstimatedResourceSize'
            Lude.<$> (x Lude..:? "estimatedOn")
            Lude.<*> (x Lude..:? "estimatedSizeInBytes")
      )
