{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteInternetGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified internet gateway. You must detach the internet gateway from the VPC before you can delete it.
module Network.AWS.EC2.DeleteInternetGateway
  ( -- * Creating a request
    DeleteInternetGateway (..),
    mkDeleteInternetGateway,

    -- ** Request lenses
    digiDryRun,
    digiInternetGatewayId,

    -- * Destructuring the response
    DeleteInternetGatewayResponse (..),
    mkDeleteInternetGatewayResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteInternetGateway' smart constructor.
data DeleteInternetGateway = DeleteInternetGateway'
  { dryRun ::
      Lude.Maybe Lude.Bool,
    internetGatewayId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteInternetGateway' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'internetGatewayId' - The ID of the internet gateway.
mkDeleteInternetGateway ::
  -- | 'internetGatewayId'
  Lude.Text ->
  DeleteInternetGateway
mkDeleteInternetGateway pInternetGatewayId_ =
  DeleteInternetGateway'
    { dryRun = Lude.Nothing,
      internetGatewayId = pInternetGatewayId_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digiDryRun :: Lens.Lens' DeleteInternetGateway (Lude.Maybe Lude.Bool)
digiDryRun = Lens.lens (dryRun :: DeleteInternetGateway -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeleteInternetGateway)
{-# DEPRECATED digiDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the internet gateway.
--
-- /Note:/ Consider using 'internetGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digiInternetGatewayId :: Lens.Lens' DeleteInternetGateway Lude.Text
digiInternetGatewayId = Lens.lens (internetGatewayId :: DeleteInternetGateway -> Lude.Text) (\s a -> s {internetGatewayId = a} :: DeleteInternetGateway)
{-# DEPRECATED digiInternetGatewayId "Use generic-lens or generic-optics with 'internetGatewayId' instead." #-}

instance Lude.AWSRequest DeleteInternetGateway where
  type Rs DeleteInternetGateway = DeleteInternetGatewayResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull DeleteInternetGatewayResponse'

instance Lude.ToHeaders DeleteInternetGateway where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteInternetGateway where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteInternetGateway where
  toQuery DeleteInternetGateway' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteInternetGateway" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "InternetGatewayId" Lude.=: internetGatewayId
      ]

-- | /See:/ 'mkDeleteInternetGatewayResponse' smart constructor.
data DeleteInternetGatewayResponse = DeleteInternetGatewayResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteInternetGatewayResponse' with the minimum fields required to make a request.
mkDeleteInternetGatewayResponse ::
  DeleteInternetGatewayResponse
mkDeleteInternetGatewayResponse = DeleteInternetGatewayResponse'
