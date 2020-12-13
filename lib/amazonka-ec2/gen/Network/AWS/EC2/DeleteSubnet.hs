{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteSubnet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified subnet. You must terminate all running instances in the subnet before you can delete the subnet.
module Network.AWS.EC2.DeleteSubnet
  ( -- * Creating a request
    DeleteSubnet (..),
    mkDeleteSubnet,

    -- ** Request lenses
    dsfSubnetId,
    dsfDryRun,

    -- * Destructuring the response
    DeleteSubnetResponse (..),
    mkDeleteSubnetResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteSubnet' smart constructor.
data DeleteSubnet = DeleteSubnet'
  { -- | The ID of the subnet.
    subnetId :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSubnet' with the minimum fields required to make a request.
--
-- * 'subnetId' - The ID of the subnet.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkDeleteSubnet ::
  -- | 'subnetId'
  Lude.Text ->
  DeleteSubnet
mkDeleteSubnet pSubnetId_ =
  DeleteSubnet' {subnetId = pSubnetId_, dryRun = Lude.Nothing}

-- | The ID of the subnet.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfSubnetId :: Lens.Lens' DeleteSubnet Lude.Text
dsfSubnetId = Lens.lens (subnetId :: DeleteSubnet -> Lude.Text) (\s a -> s {subnetId = a} :: DeleteSubnet)
{-# DEPRECATED dsfSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfDryRun :: Lens.Lens' DeleteSubnet (Lude.Maybe Lude.Bool)
dsfDryRun = Lens.lens (dryRun :: DeleteSubnet -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeleteSubnet)
{-# DEPRECATED dsfDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest DeleteSubnet where
  type Rs DeleteSubnet = DeleteSubnetResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull DeleteSubnetResponse'

instance Lude.ToHeaders DeleteSubnet where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteSubnet where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteSubnet where
  toQuery DeleteSubnet' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteSubnet" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "SubnetId" Lude.=: subnetId,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkDeleteSubnetResponse' smart constructor.
data DeleteSubnetResponse = DeleteSubnetResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSubnetResponse' with the minimum fields required to make a request.
mkDeleteSubnetResponse ::
  DeleteSubnetResponse
mkDeleteSubnetResponse = DeleteSubnetResponse'
