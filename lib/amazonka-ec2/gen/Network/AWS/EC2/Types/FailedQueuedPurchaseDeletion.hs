-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FailedQueuedPurchaseDeletion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FailedQueuedPurchaseDeletion
  ( FailedQueuedPurchaseDeletion (..),

    -- * Smart constructor
    mkFailedQueuedPurchaseDeletion,

    -- * Lenses
    fqpdError,
    fqpdReservedInstancesId,
  )
where

import Network.AWS.EC2.Types.DeleteQueuedReservedInstancesError
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a Reserved Instance whose queued purchase was not deleted.
--
-- /See:/ 'mkFailedQueuedPurchaseDeletion' smart constructor.
data FailedQueuedPurchaseDeletion = FailedQueuedPurchaseDeletion'
  { error ::
      Lude.Maybe
        DeleteQueuedReservedInstancesError,
    reservedInstancesId ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FailedQueuedPurchaseDeletion' with the minimum fields required to make a request.
--
-- * 'error' - The error.
-- * 'reservedInstancesId' - The ID of the Reserved Instance.
mkFailedQueuedPurchaseDeletion ::
  FailedQueuedPurchaseDeletion
mkFailedQueuedPurchaseDeletion =
  FailedQueuedPurchaseDeletion'
    { error = Lude.Nothing,
      reservedInstancesId = Lude.Nothing
    }

-- | The error.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fqpdError :: Lens.Lens' FailedQueuedPurchaseDeletion (Lude.Maybe DeleteQueuedReservedInstancesError)
fqpdError = Lens.lens (error :: FailedQueuedPurchaseDeletion -> Lude.Maybe DeleteQueuedReservedInstancesError) (\s a -> s {error = a} :: FailedQueuedPurchaseDeletion)
{-# DEPRECATED fqpdError "Use generic-lens or generic-optics with 'error' instead." #-}

-- | The ID of the Reserved Instance.
--
-- /Note:/ Consider using 'reservedInstancesId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fqpdReservedInstancesId :: Lens.Lens' FailedQueuedPurchaseDeletion (Lude.Maybe Lude.Text)
fqpdReservedInstancesId = Lens.lens (reservedInstancesId :: FailedQueuedPurchaseDeletion -> Lude.Maybe Lude.Text) (\s a -> s {reservedInstancesId = a} :: FailedQueuedPurchaseDeletion)
{-# DEPRECATED fqpdReservedInstancesId "Use generic-lens or generic-optics with 'reservedInstancesId' instead." #-}

instance Lude.FromXML FailedQueuedPurchaseDeletion where
  parseXML x =
    FailedQueuedPurchaseDeletion'
      Lude.<$> (x Lude..@? "error") Lude.<*> (x Lude..@? "reservedInstancesId")
