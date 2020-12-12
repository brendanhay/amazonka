{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.ArrayProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.ArrayProperties
  ( ArrayProperties (..),

    -- * Smart constructor
    mkArrayProperties,

    -- * Lenses
    apSize,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object representing an AWS Batch array job.
--
-- /See:/ 'mkArrayProperties' smart constructor.
newtype ArrayProperties = ArrayProperties'
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

-- | Creates a value of 'ArrayProperties' with the minimum fields required to make a request.
--
-- * 'size' - The size of the array job.
mkArrayProperties ::
  ArrayProperties
mkArrayProperties = ArrayProperties' {size = Lude.Nothing}

-- | The size of the array job.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apSize :: Lens.Lens' ArrayProperties (Lude.Maybe Lude.Int)
apSize = Lens.lens (size :: ArrayProperties -> Lude.Maybe Lude.Int) (\s a -> s {size = a} :: ArrayProperties)
{-# DEPRECATED apSize "Use generic-lens or generic-optics with 'size' instead." #-}

instance Lude.ToJSON ArrayProperties where
  toJSON ArrayProperties' {..} =
    Lude.object (Lude.catMaybes [("size" Lude..=) Lude.<$> size])
