{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteQueuedReservedInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the queued purchases for the specified Reserved Instances.
module Network.AWS.EC2.DeleteQueuedReservedInstances
  ( -- * Creating a request
    DeleteQueuedReservedInstances (..),
    mkDeleteQueuedReservedInstances,

    -- ** Request lenses
    dqriDryRun,
    dqriReservedInstancesIds,

    -- * Destructuring the response
    DeleteQueuedReservedInstancesResponse (..),
    mkDeleteQueuedReservedInstancesResponse,

    -- ** Response lenses
    dqrirsFailedQueuedPurchaseDeletions,
    dqrirsSuccessfulQueuedPurchaseDeletions,
    dqrirsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteQueuedReservedInstances' smart constructor.
data DeleteQueuedReservedInstances = DeleteQueuedReservedInstances'
  { dryRun ::
      Lude.Maybe Lude.Bool,
    reservedInstancesIds ::
      Lude.NonEmpty Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteQueuedReservedInstances' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'reservedInstancesIds' - The IDs of the Reserved Instances.
mkDeleteQueuedReservedInstances ::
  -- | 'reservedInstancesIds'
  Lude.NonEmpty Lude.Text ->
  DeleteQueuedReservedInstances
mkDeleteQueuedReservedInstances pReservedInstancesIds_ =
  DeleteQueuedReservedInstances'
    { dryRun = Lude.Nothing,
      reservedInstancesIds = pReservedInstancesIds_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqriDryRun :: Lens.Lens' DeleteQueuedReservedInstances (Lude.Maybe Lude.Bool)
dqriDryRun = Lens.lens (dryRun :: DeleteQueuedReservedInstances -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeleteQueuedReservedInstances)
{-# DEPRECATED dqriDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The IDs of the Reserved Instances.
--
-- /Note:/ Consider using 'reservedInstancesIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqriReservedInstancesIds :: Lens.Lens' DeleteQueuedReservedInstances (Lude.NonEmpty Lude.Text)
dqriReservedInstancesIds = Lens.lens (reservedInstancesIds :: DeleteQueuedReservedInstances -> Lude.NonEmpty Lude.Text) (\s a -> s {reservedInstancesIds = a} :: DeleteQueuedReservedInstances)
{-# DEPRECATED dqriReservedInstancesIds "Use generic-lens or generic-optics with 'reservedInstancesIds' instead." #-}

instance Lude.AWSRequest DeleteQueuedReservedInstances where
  type
    Rs DeleteQueuedReservedInstances =
      DeleteQueuedReservedInstancesResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DeleteQueuedReservedInstancesResponse'
            Lude.<$> ( x Lude..@? "failedQueuedPurchaseDeletionSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> ( x Lude..@? "successfulQueuedPurchaseDeletionSet"
                         Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteQueuedReservedInstances where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteQueuedReservedInstances where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteQueuedReservedInstances where
  toQuery DeleteQueuedReservedInstances' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DeleteQueuedReservedInstances" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        Lude.toQueryList "ReservedInstancesId" reservedInstancesIds
      ]

-- | /See:/ 'mkDeleteQueuedReservedInstancesResponse' smart constructor.
data DeleteQueuedReservedInstancesResponse = DeleteQueuedReservedInstancesResponse'
  { failedQueuedPurchaseDeletions ::
      Lude.Maybe
        [FailedQueuedPurchaseDeletion],
    successfulQueuedPurchaseDeletions ::
      Lude.Maybe
        [SuccessfulQueuedPurchaseDeletion],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteQueuedReservedInstancesResponse' with the minimum fields required to make a request.
--
-- * 'failedQueuedPurchaseDeletions' - Information about the queued purchases that could not be deleted.
-- * 'responseStatus' - The response status code.
-- * 'successfulQueuedPurchaseDeletions' - Information about the queued purchases that were successfully deleted.
mkDeleteQueuedReservedInstancesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteQueuedReservedInstancesResponse
mkDeleteQueuedReservedInstancesResponse pResponseStatus_ =
  DeleteQueuedReservedInstancesResponse'
    { failedQueuedPurchaseDeletions =
        Lude.Nothing,
      successfulQueuedPurchaseDeletions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the queued purchases that could not be deleted.
--
-- /Note:/ Consider using 'failedQueuedPurchaseDeletions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqrirsFailedQueuedPurchaseDeletions :: Lens.Lens' DeleteQueuedReservedInstancesResponse (Lude.Maybe [FailedQueuedPurchaseDeletion])
dqrirsFailedQueuedPurchaseDeletions = Lens.lens (failedQueuedPurchaseDeletions :: DeleteQueuedReservedInstancesResponse -> Lude.Maybe [FailedQueuedPurchaseDeletion]) (\s a -> s {failedQueuedPurchaseDeletions = a} :: DeleteQueuedReservedInstancesResponse)
{-# DEPRECATED dqrirsFailedQueuedPurchaseDeletions "Use generic-lens or generic-optics with 'failedQueuedPurchaseDeletions' instead." #-}

-- | Information about the queued purchases that were successfully deleted.
--
-- /Note:/ Consider using 'successfulQueuedPurchaseDeletions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqrirsSuccessfulQueuedPurchaseDeletions :: Lens.Lens' DeleteQueuedReservedInstancesResponse (Lude.Maybe [SuccessfulQueuedPurchaseDeletion])
dqrirsSuccessfulQueuedPurchaseDeletions = Lens.lens (successfulQueuedPurchaseDeletions :: DeleteQueuedReservedInstancesResponse -> Lude.Maybe [SuccessfulQueuedPurchaseDeletion]) (\s a -> s {successfulQueuedPurchaseDeletions = a} :: DeleteQueuedReservedInstancesResponse)
{-# DEPRECATED dqrirsSuccessfulQueuedPurchaseDeletions "Use generic-lens or generic-optics with 'successfulQueuedPurchaseDeletions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqrirsResponseStatus :: Lens.Lens' DeleteQueuedReservedInstancesResponse Lude.Int
dqrirsResponseStatus = Lens.lens (responseStatus :: DeleteQueuedReservedInstancesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteQueuedReservedInstancesResponse)
{-# DEPRECATED dqrirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
