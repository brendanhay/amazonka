{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyTransitGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified transit gateway. When you modify a transit gateway, the modified options are applied to new transit gateway attachments only. Your existing transit gateway attachments are not modified.
module Network.AWS.EC2.ModifyTransitGateway
  ( -- * Creating a request
    ModifyTransitGateway (..),
    mkModifyTransitGateway,

    -- ** Request lenses
    mtgTransitGatewayId,
    mtgOptions,
    mtgDescription,
    mtgDryRun,

    -- * Destructuring the response
    ModifyTransitGatewayResponse (..),
    mkModifyTransitGatewayResponse,

    -- ** Response lenses
    mtgrsTransitGateway,
    mtgrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyTransitGateway' smart constructor.
data ModifyTransitGateway = ModifyTransitGateway'
  { -- | The ID of the transit gateway.
    transitGatewayId :: Lude.Text,
    -- | The options to modify.
    options :: Lude.Maybe ModifyTransitGatewayOptions,
    -- | The description for the transit gateway.
    description :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyTransitGateway' with the minimum fields required to make a request.
--
-- * 'transitGatewayId' - The ID of the transit gateway.
-- * 'options' - The options to modify.
-- * 'description' - The description for the transit gateway.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkModifyTransitGateway ::
  -- | 'transitGatewayId'
  Lude.Text ->
  ModifyTransitGateway
mkModifyTransitGateway pTransitGatewayId_ =
  ModifyTransitGateway'
    { transitGatewayId = pTransitGatewayId_,
      options = Lude.Nothing,
      description = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | The ID of the transit gateway.
--
-- /Note:/ Consider using 'transitGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgTransitGatewayId :: Lens.Lens' ModifyTransitGateway Lude.Text
mtgTransitGatewayId = Lens.lens (transitGatewayId :: ModifyTransitGateway -> Lude.Text) (\s a -> s {transitGatewayId = a} :: ModifyTransitGateway)
{-# DEPRECATED mtgTransitGatewayId "Use generic-lens or generic-optics with 'transitGatewayId' instead." #-}

-- | The options to modify.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgOptions :: Lens.Lens' ModifyTransitGateway (Lude.Maybe ModifyTransitGatewayOptions)
mtgOptions = Lens.lens (options :: ModifyTransitGateway -> Lude.Maybe ModifyTransitGatewayOptions) (\s a -> s {options = a} :: ModifyTransitGateway)
{-# DEPRECATED mtgOptions "Use generic-lens or generic-optics with 'options' instead." #-}

-- | The description for the transit gateway.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgDescription :: Lens.Lens' ModifyTransitGateway (Lude.Maybe Lude.Text)
mtgDescription = Lens.lens (description :: ModifyTransitGateway -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ModifyTransitGateway)
{-# DEPRECATED mtgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgDryRun :: Lens.Lens' ModifyTransitGateway (Lude.Maybe Lude.Bool)
mtgDryRun = Lens.lens (dryRun :: ModifyTransitGateway -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ModifyTransitGateway)
{-# DEPRECATED mtgDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest ModifyTransitGateway where
  type Rs ModifyTransitGateway = ModifyTransitGatewayResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          ModifyTransitGatewayResponse'
            Lude.<$> (x Lude..@? "transitGateway")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyTransitGateway where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyTransitGateway where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyTransitGateway where
  toQuery ModifyTransitGateway' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifyTransitGateway" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "TransitGatewayId" Lude.=: transitGatewayId,
        "Options" Lude.=: options,
        "Description" Lude.=: description,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkModifyTransitGatewayResponse' smart constructor.
data ModifyTransitGatewayResponse = ModifyTransitGatewayResponse'
  { transitGateway :: Lude.Maybe TransitGateway,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyTransitGatewayResponse' with the minimum fields required to make a request.
--
-- * 'transitGateway' -
-- * 'responseStatus' - The response status code.
mkModifyTransitGatewayResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyTransitGatewayResponse
mkModifyTransitGatewayResponse pResponseStatus_ =
  ModifyTransitGatewayResponse'
    { transitGateway = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'transitGateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgrsTransitGateway :: Lens.Lens' ModifyTransitGatewayResponse (Lude.Maybe TransitGateway)
mtgrsTransitGateway = Lens.lens (transitGateway :: ModifyTransitGatewayResponse -> Lude.Maybe TransitGateway) (\s a -> s {transitGateway = a} :: ModifyTransitGatewayResponse)
{-# DEPRECATED mtgrsTransitGateway "Use generic-lens or generic-optics with 'transitGateway' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgrsResponseStatus :: Lens.Lens' ModifyTransitGatewayResponse Lude.Int
mtgrsResponseStatus = Lens.lens (responseStatus :: ModifyTransitGatewayResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyTransitGatewayResponse)
{-# DEPRECATED mtgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
