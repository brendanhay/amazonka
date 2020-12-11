{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteVPC
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified VPC. You must detach or delete all gateways and resources that are associated with the VPC before you can delete it. For example, you must terminate all instances running in the VPC, delete all security groups associated with the VPC (except the default one), delete all route tables associated with the VPC (except the default one), and so on.
module Network.AWS.EC2.DeleteVPC
  ( -- * Creating a request
    DeleteVPC (..),
    mkDeleteVPC,

    -- ** Request lenses
    delDryRun,
    delVPCId,

    -- * Destructuring the response
    DeleteVPCResponse (..),
    mkDeleteVPCResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteVPC' smart constructor.
data DeleteVPC = DeleteVPC'
  { dryRun :: Lude.Maybe Lude.Bool,
    vpcId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteVPC' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'vpcId' - The ID of the VPC.
mkDeleteVPC ::
  -- | 'vpcId'
  Lude.Text ->
  DeleteVPC
mkDeleteVPC pVPCId_ =
  DeleteVPC' {dryRun = Lude.Nothing, vpcId = pVPCId_}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delDryRun :: Lens.Lens' DeleteVPC (Lude.Maybe Lude.Bool)
delDryRun = Lens.lens (dryRun :: DeleteVPC -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeleteVPC)
{-# DEPRECATED delDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delVPCId :: Lens.Lens' DeleteVPC Lude.Text
delVPCId = Lens.lens (vpcId :: DeleteVPC -> Lude.Text) (\s a -> s {vpcId = a} :: DeleteVPC)
{-# DEPRECATED delVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

instance Lude.AWSRequest DeleteVPC where
  type Rs DeleteVPC = DeleteVPCResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull DeleteVPCResponse'

instance Lude.ToHeaders DeleteVPC where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteVPC where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteVPC where
  toQuery DeleteVPC' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteVpc" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "VpcId" Lude.=: vpcId
      ]

-- | /See:/ 'mkDeleteVPCResponse' smart constructor.
data DeleteVPCResponse = DeleteVPCResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteVPCResponse' with the minimum fields required to make a request.
mkDeleteVPCResponse ::
  DeleteVPCResponse
mkDeleteVPCResponse = DeleteVPCResponse'
