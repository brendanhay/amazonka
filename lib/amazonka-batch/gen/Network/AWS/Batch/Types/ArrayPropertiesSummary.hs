{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.ArrayPropertiesSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.ArrayPropertiesSummary
  ( ArrayPropertiesSummary (..),

    -- * Smart constructor
    mkArrayPropertiesSummary,

    -- * Lenses
    apsSize,
    apsIndex,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object representing the array properties of a job.
--
-- /See:/ 'mkArrayPropertiesSummary' smart constructor.
data ArrayPropertiesSummary = ArrayPropertiesSummary'
  { size ::
      Lude.Maybe Lude.Int,
    index :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ArrayPropertiesSummary' with the minimum fields required to make a request.
--
-- * 'index' - The job index within the array that is associated with this job. This parameter is returned for children of array jobs.
-- * 'size' - The size of the array job. This parameter is returned for parent array jobs.
mkArrayPropertiesSummary ::
  ArrayPropertiesSummary
mkArrayPropertiesSummary =
  ArrayPropertiesSummary'
    { size = Lude.Nothing,
      index = Lude.Nothing
    }

-- | The size of the array job. This parameter is returned for parent array jobs.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apsSize :: Lens.Lens' ArrayPropertiesSummary (Lude.Maybe Lude.Int)
apsSize = Lens.lens (size :: ArrayPropertiesSummary -> Lude.Maybe Lude.Int) (\s a -> s {size = a} :: ArrayPropertiesSummary)
{-# DEPRECATED apsSize "Use generic-lens or generic-optics with 'size' instead." #-}

-- | The job index within the array that is associated with this job. This parameter is returned for children of array jobs.
--
-- /Note:/ Consider using 'index' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apsIndex :: Lens.Lens' ArrayPropertiesSummary (Lude.Maybe Lude.Int)
apsIndex = Lens.lens (index :: ArrayPropertiesSummary -> Lude.Maybe Lude.Int) (\s a -> s {index = a} :: ArrayPropertiesSummary)
{-# DEPRECATED apsIndex "Use generic-lens or generic-optics with 'index' instead." #-}

instance Lude.FromJSON ArrayPropertiesSummary where
  parseJSON =
    Lude.withObject
      "ArrayPropertiesSummary"
      ( \x ->
          ArrayPropertiesSummary'
            Lude.<$> (x Lude..:? "size") Lude.<*> (x Lude..:? "index")
      )
