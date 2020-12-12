{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.BatchArrayProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.BatchArrayProperties
  ( BatchArrayProperties (..),

    -- * Smart constructor
    mkBatchArrayProperties,

    -- * Lenses
    bapSize,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The array properties for the submitted job, such as the size of the array. The array size can be between 2 and 10,000. If you specify array properties for a job, it becomes an array job. This parameter is used only if the target is an AWS Batch job.
--
-- /See:/ 'mkBatchArrayProperties' smart constructor.
newtype BatchArrayProperties = BatchArrayProperties'
  { size ::
      Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchArrayProperties' with the minimum fields required to make a request.
--
-- * 'size' - The size of the array, if this is an array batch job. Valid values are integers between 2 and 10,000.
mkBatchArrayProperties ::
  BatchArrayProperties
mkBatchArrayProperties = BatchArrayProperties' {size = Lude.Nothing}

-- | The size of the array, if this is an array batch job. Valid values are integers between 2 and 10,000.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bapSize :: Lens.Lens' BatchArrayProperties (Lude.Maybe Lude.Int)
bapSize = Lens.lens (size :: BatchArrayProperties -> Lude.Maybe Lude.Int) (\s a -> s {size = a} :: BatchArrayProperties)
{-# DEPRECATED bapSize "Use generic-lens or generic-optics with 'size' instead." #-}

instance Lude.FromJSON BatchArrayProperties where
  parseJSON =
    Lude.withObject
      "BatchArrayProperties"
      (\x -> BatchArrayProperties' Lude.<$> (x Lude..:? "Size"))

instance Lude.ToJSON BatchArrayProperties where
  toJSON BatchArrayProperties' {..} =
    Lude.object (Lude.catMaybes [("Size" Lude..=) Lude.<$> size])
