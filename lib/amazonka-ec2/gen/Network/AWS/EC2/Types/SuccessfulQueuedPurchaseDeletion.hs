{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SuccessfulQueuedPurchaseDeletion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SuccessfulQueuedPurchaseDeletion
  ( SuccessfulQueuedPurchaseDeletion (..),

    -- * Smart constructor
    mkSuccessfulQueuedPurchaseDeletion,

    -- * Lenses
    sqpdReservedInstancesId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a Reserved Instance whose queued purchase was successfully deleted.
--
-- /See:/ 'mkSuccessfulQueuedPurchaseDeletion' smart constructor.
newtype SuccessfulQueuedPurchaseDeletion = SuccessfulQueuedPurchaseDeletion'
  { -- | The ID of the Reserved Instance.
    reservedInstancesId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SuccessfulQueuedPurchaseDeletion' with the minimum fields required to make a request.
--
-- * 'reservedInstancesId' - The ID of the Reserved Instance.
mkSuccessfulQueuedPurchaseDeletion ::
  SuccessfulQueuedPurchaseDeletion
mkSuccessfulQueuedPurchaseDeletion =
  SuccessfulQueuedPurchaseDeletion'
    { reservedInstancesId =
        Lude.Nothing
    }

-- | The ID of the Reserved Instance.
--
-- /Note:/ Consider using 'reservedInstancesId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sqpdReservedInstancesId :: Lens.Lens' SuccessfulQueuedPurchaseDeletion (Lude.Maybe Lude.Text)
sqpdReservedInstancesId = Lens.lens (reservedInstancesId :: SuccessfulQueuedPurchaseDeletion -> Lude.Maybe Lude.Text) (\s a -> s {reservedInstancesId = a} :: SuccessfulQueuedPurchaseDeletion)
{-# DEPRECATED sqpdReservedInstancesId "Use generic-lens or generic-optics with 'reservedInstancesId' instead." #-}

instance Lude.FromXML SuccessfulQueuedPurchaseDeletion where
  parseXML x =
    SuccessfulQueuedPurchaseDeletion'
      Lude.<$> (x Lude..@? "reservedInstancesId")
