-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.ArrayPropertiesDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.ArrayPropertiesDetail
  ( ArrayPropertiesDetail (..),

    -- * Smart constructor
    mkArrayPropertiesDetail,

    -- * Lenses
    apdSize,
    apdStatusSummary,
    apdIndex,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object representing the array properties of a job.
--
-- /See:/ 'mkArrayPropertiesDetail' smart constructor.
data ArrayPropertiesDetail = ArrayPropertiesDetail'
  { size ::
      Lude.Maybe Lude.Int,
    statusSummary ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Int)),
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

-- | Creates a value of 'ArrayPropertiesDetail' with the minimum fields required to make a request.
--
-- * 'index' - The job index within the array that is associated with this job. This parameter is returned for array job children.
-- * 'size' - The size of the array job. This parameter is returned for parent array jobs.
-- * 'statusSummary' - A summary of the number of array job children in each available job status. This parameter is returned for parent array jobs.
mkArrayPropertiesDetail ::
  ArrayPropertiesDetail
mkArrayPropertiesDetail =
  ArrayPropertiesDetail'
    { size = Lude.Nothing,
      statusSummary = Lude.Nothing,
      index = Lude.Nothing
    }

-- | The size of the array job. This parameter is returned for parent array jobs.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apdSize :: Lens.Lens' ArrayPropertiesDetail (Lude.Maybe Lude.Int)
apdSize = Lens.lens (size :: ArrayPropertiesDetail -> Lude.Maybe Lude.Int) (\s a -> s {size = a} :: ArrayPropertiesDetail)
{-# DEPRECATED apdSize "Use generic-lens or generic-optics with 'size' instead." #-}

-- | A summary of the number of array job children in each available job status. This parameter is returned for parent array jobs.
--
-- /Note:/ Consider using 'statusSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apdStatusSummary :: Lens.Lens' ArrayPropertiesDetail (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Int)))
apdStatusSummary = Lens.lens (statusSummary :: ArrayPropertiesDetail -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Int))) (\s a -> s {statusSummary = a} :: ArrayPropertiesDetail)
{-# DEPRECATED apdStatusSummary "Use generic-lens or generic-optics with 'statusSummary' instead." #-}

-- | The job index within the array that is associated with this job. This parameter is returned for array job children.
--
-- /Note:/ Consider using 'index' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apdIndex :: Lens.Lens' ArrayPropertiesDetail (Lude.Maybe Lude.Int)
apdIndex = Lens.lens (index :: ArrayPropertiesDetail -> Lude.Maybe Lude.Int) (\s a -> s {index = a} :: ArrayPropertiesDetail)
{-# DEPRECATED apdIndex "Use generic-lens or generic-optics with 'index' instead." #-}

instance Lude.FromJSON ArrayPropertiesDetail where
  parseJSON =
    Lude.withObject
      "ArrayPropertiesDetail"
      ( \x ->
          ArrayPropertiesDetail'
            Lude.<$> (x Lude..:? "size")
            Lude.<*> (x Lude..:? "statusSummary" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "index")
      )
