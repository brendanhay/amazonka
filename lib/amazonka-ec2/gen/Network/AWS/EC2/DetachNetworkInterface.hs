{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DetachNetworkInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches a network interface from an instance.
module Network.AWS.EC2.DetachNetworkInterface
  ( -- * Creating a request
    DetachNetworkInterface (..),
    mkDetachNetworkInterface,

    -- ** Request lenses
    dnigForce,
    dnigAttachmentId,
    dnigDryRun,

    -- * Destructuring the response
    DetachNetworkInterfaceResponse (..),
    mkDetachNetworkInterfaceResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for DetachNetworkInterface.
--
-- /See:/ 'mkDetachNetworkInterface' smart constructor.
data DetachNetworkInterface = DetachNetworkInterface'
  { -- | Specifies whether to force a detachment.
    force :: Lude.Maybe Lude.Bool,
    -- | The ID of the attachment.
    attachmentId :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetachNetworkInterface' with the minimum fields required to make a request.
--
-- * 'force' - Specifies whether to force a detachment.
-- * 'attachmentId' - The ID of the attachment.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkDetachNetworkInterface ::
  -- | 'attachmentId'
  Lude.Text ->
  DetachNetworkInterface
mkDetachNetworkInterface pAttachmentId_ =
  DetachNetworkInterface'
    { force = Lude.Nothing,
      attachmentId = pAttachmentId_,
      dryRun = Lude.Nothing
    }

-- | Specifies whether to force a detachment.
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnigForce :: Lens.Lens' DetachNetworkInterface (Lude.Maybe Lude.Bool)
dnigForce = Lens.lens (force :: DetachNetworkInterface -> Lude.Maybe Lude.Bool) (\s a -> s {force = a} :: DetachNetworkInterface)
{-# DEPRECATED dnigForce "Use generic-lens or generic-optics with 'force' instead." #-}

-- | The ID of the attachment.
--
-- /Note:/ Consider using 'attachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnigAttachmentId :: Lens.Lens' DetachNetworkInterface Lude.Text
dnigAttachmentId = Lens.lens (attachmentId :: DetachNetworkInterface -> Lude.Text) (\s a -> s {attachmentId = a} :: DetachNetworkInterface)
{-# DEPRECATED dnigAttachmentId "Use generic-lens or generic-optics with 'attachmentId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnigDryRun :: Lens.Lens' DetachNetworkInterface (Lude.Maybe Lude.Bool)
dnigDryRun = Lens.lens (dryRun :: DetachNetworkInterface -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DetachNetworkInterface)
{-# DEPRECATED dnigDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest DetachNetworkInterface where
  type Rs DetachNetworkInterface = DetachNetworkInterfaceResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull DetachNetworkInterfaceResponse'

instance Lude.ToHeaders DetachNetworkInterface where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DetachNetworkInterface where
  toPath = Lude.const "/"

instance Lude.ToQuery DetachNetworkInterface where
  toQuery DetachNetworkInterface' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DetachNetworkInterface" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "Force" Lude.=: force,
        "AttachmentId" Lude.=: attachmentId,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkDetachNetworkInterfaceResponse' smart constructor.
data DetachNetworkInterfaceResponse = DetachNetworkInterfaceResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetachNetworkInterfaceResponse' with the minimum fields required to make a request.
mkDetachNetworkInterfaceResponse ::
  DetachNetworkInterfaceResponse
mkDetachNetworkInterfaceResponse = DetachNetworkInterfaceResponse'
