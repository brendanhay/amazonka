{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Transition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Transition
  ( Transition (..),

    -- * Smart constructor
    mkTransition,

    -- * Lenses
    traDays,
    traDate,
    traStorageClass,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.TransitionStorageClass

-- | Specifies when an object transitions to a specified storage class. For more information about Amazon S3 lifecycle configuration rules, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/lifecycle-transition-general-considerations.html Transitioning Objects Using Amazon S3 Lifecycle> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /See:/ 'mkTransition' smart constructor.
data Transition = Transition'
  { days :: Lude.Maybe Lude.Int,
    date :: Lude.Maybe Lude.DateTime,
    storageClass :: Lude.Maybe TransitionStorageClass
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Transition' with the minimum fields required to make a request.
--
-- * 'date' - Indicates when objects are transitioned to the specified storage class. The date value must be in ISO 8601 format. The time is always midnight UTC.
-- * 'days' - Indicates the number of days after creation when objects are transitioned to the specified storage class. The value must be a positive integer.
-- * 'storageClass' - The storage class to which you want the object to transition.
mkTransition ::
  Transition
mkTransition =
  Transition'
    { days = Lude.Nothing,
      date = Lude.Nothing,
      storageClass = Lude.Nothing
    }

-- | Indicates the number of days after creation when objects are transitioned to the specified storage class. The value must be a positive integer.
--
-- /Note:/ Consider using 'days' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
traDays :: Lens.Lens' Transition (Lude.Maybe Lude.Int)
traDays = Lens.lens (days :: Transition -> Lude.Maybe Lude.Int) (\s a -> s {days = a} :: Transition)
{-# DEPRECATED traDays "Use generic-lens or generic-optics with 'days' instead." #-}

-- | Indicates when objects are transitioned to the specified storage class. The date value must be in ISO 8601 format. The time is always midnight UTC.
--
-- /Note:/ Consider using 'date' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
traDate :: Lens.Lens' Transition (Lude.Maybe Lude.DateTime)
traDate = Lens.lens (date :: Transition -> Lude.Maybe Lude.DateTime) (\s a -> s {date = a} :: Transition)
{-# DEPRECATED traDate "Use generic-lens or generic-optics with 'date' instead." #-}

-- | The storage class to which you want the object to transition.
--
-- /Note:/ Consider using 'storageClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
traStorageClass :: Lens.Lens' Transition (Lude.Maybe TransitionStorageClass)
traStorageClass = Lens.lens (storageClass :: Transition -> Lude.Maybe TransitionStorageClass) (\s a -> s {storageClass = a} :: Transition)
{-# DEPRECATED traStorageClass "Use generic-lens or generic-optics with 'storageClass' instead." #-}

instance Lude.FromXML Transition where
  parseXML x =
    Transition'
      Lude.<$> (x Lude..@? "Days")
      Lude.<*> (x Lude..@? "Date")
      Lude.<*> (x Lude..@? "StorageClass")

instance Lude.ToXML Transition where
  toXML Transition' {..} =
    Lude.mconcat
      [ "Days" Lude.@= days,
        "Date" Lude.@= date,
        "StorageClass" Lude.@= storageClass
      ]
