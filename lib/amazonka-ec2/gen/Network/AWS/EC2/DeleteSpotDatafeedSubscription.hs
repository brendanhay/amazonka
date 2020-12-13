{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteSpotDatafeedSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the data feed for Spot Instances.
module Network.AWS.EC2.DeleteSpotDatafeedSubscription
  ( -- * Creating a request
    DeleteSpotDatafeedSubscription (..),
    mkDeleteSpotDatafeedSubscription,

    -- ** Request lenses
    dsdsfDryRun,

    -- * Destructuring the response
    DeleteSpotDatafeedSubscriptionResponse (..),
    mkDeleteSpotDatafeedSubscriptionResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for DeleteSpotDatafeedSubscription.
--
-- /See:/ 'mkDeleteSpotDatafeedSubscription' smart constructor.
newtype DeleteSpotDatafeedSubscription = DeleteSpotDatafeedSubscription'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSpotDatafeedSubscription' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkDeleteSpotDatafeedSubscription ::
  DeleteSpotDatafeedSubscription
mkDeleteSpotDatafeedSubscription =
  DeleteSpotDatafeedSubscription' {dryRun = Lude.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsfDryRun :: Lens.Lens' DeleteSpotDatafeedSubscription (Lude.Maybe Lude.Bool)
dsdsfDryRun = Lens.lens (dryRun :: DeleteSpotDatafeedSubscription -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeleteSpotDatafeedSubscription)
{-# DEPRECATED dsdsfDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest DeleteSpotDatafeedSubscription where
  type
    Rs DeleteSpotDatafeedSubscription =
      DeleteSpotDatafeedSubscriptionResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull DeleteSpotDatafeedSubscriptionResponse'

instance Lude.ToHeaders DeleteSpotDatafeedSubscription where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteSpotDatafeedSubscription where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteSpotDatafeedSubscription where
  toQuery DeleteSpotDatafeedSubscription' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DeleteSpotDatafeedSubscription" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkDeleteSpotDatafeedSubscriptionResponse' smart constructor.
data DeleteSpotDatafeedSubscriptionResponse = DeleteSpotDatafeedSubscriptionResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSpotDatafeedSubscriptionResponse' with the minimum fields required to make a request.
mkDeleteSpotDatafeedSubscriptionResponse ::
  DeleteSpotDatafeedSubscriptionResponse
mkDeleteSpotDatafeedSubscriptionResponse =
  DeleteSpotDatafeedSubscriptionResponse'
