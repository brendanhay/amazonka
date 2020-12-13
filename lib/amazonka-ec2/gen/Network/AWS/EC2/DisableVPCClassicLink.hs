{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DisableVPCClassicLink
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables ClassicLink for a VPC. You cannot disable ClassicLink for a VPC that has EC2-Classic instances linked to it.
module Network.AWS.EC2.DisableVPCClassicLink
  ( -- * Creating a request
    DisableVPCClassicLink (..),
    mkDisableVPCClassicLink,

    -- ** Request lenses
    dvclVPCId,
    dvclDryRun,

    -- * Destructuring the response
    DisableVPCClassicLinkResponse (..),
    mkDisableVPCClassicLinkResponse,

    -- ** Response lenses
    dvclrsReturn,
    dvclrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisableVPCClassicLink' smart constructor.
data DisableVPCClassicLink = DisableVPCClassicLink'
  { -- | The ID of the VPC.
    vpcId :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisableVPCClassicLink' with the minimum fields required to make a request.
--
-- * 'vpcId' - The ID of the VPC.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkDisableVPCClassicLink ::
  -- | 'vpcId'
  Lude.Text ->
  DisableVPCClassicLink
mkDisableVPCClassicLink pVPCId_ =
  DisableVPCClassicLink' {vpcId = pVPCId_, dryRun = Lude.Nothing}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvclVPCId :: Lens.Lens' DisableVPCClassicLink Lude.Text
dvclVPCId = Lens.lens (vpcId :: DisableVPCClassicLink -> Lude.Text) (\s a -> s {vpcId = a} :: DisableVPCClassicLink)
{-# DEPRECATED dvclVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvclDryRun :: Lens.Lens' DisableVPCClassicLink (Lude.Maybe Lude.Bool)
dvclDryRun = Lens.lens (dryRun :: DisableVPCClassicLink -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DisableVPCClassicLink)
{-# DEPRECATED dvclDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest DisableVPCClassicLink where
  type Rs DisableVPCClassicLink = DisableVPCClassicLinkResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DisableVPCClassicLinkResponse'
            Lude.<$> (x Lude..@? "return") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisableVPCClassicLink where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DisableVPCClassicLink where
  toPath = Lude.const "/"

instance Lude.ToQuery DisableVPCClassicLink where
  toQuery DisableVPCClassicLink' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DisableVpcClassicLink" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "VpcId" Lude.=: vpcId,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkDisableVPCClassicLinkResponse' smart constructor.
data DisableVPCClassicLinkResponse = DisableVPCClassicLinkResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, it returns an error.
    return :: Lude.Maybe Lude.Bool,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisableVPCClassicLinkResponse' with the minimum fields required to make a request.
--
-- * 'return' - Returns @true@ if the request succeeds; otherwise, it returns an error.
-- * 'responseStatus' - The response status code.
mkDisableVPCClassicLinkResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisableVPCClassicLinkResponse
mkDisableVPCClassicLinkResponse pResponseStatus_ =
  DisableVPCClassicLinkResponse'
    { return = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvclrsReturn :: Lens.Lens' DisableVPCClassicLinkResponse (Lude.Maybe Lude.Bool)
dvclrsReturn = Lens.lens (return :: DisableVPCClassicLinkResponse -> Lude.Maybe Lude.Bool) (\s a -> s {return = a} :: DisableVPCClassicLinkResponse)
{-# DEPRECATED dvclrsReturn "Use generic-lens or generic-optics with 'return' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvclrsResponseStatus :: Lens.Lens' DisableVPCClassicLinkResponse Lude.Int
dvclrsResponseStatus = Lens.lens (responseStatus :: DisableVPCClassicLinkResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisableVPCClassicLinkResponse)
{-# DEPRECATED dvclrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
