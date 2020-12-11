{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ResetNetworkInterfaceAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets a network interface attribute. You can specify only one attribute at a time.
module Network.AWS.EC2.ResetNetworkInterfaceAttribute
  ( -- * Creating a request
    ResetNetworkInterfaceAttribute (..),
    mkResetNetworkInterfaceAttribute,

    -- ** Request lenses
    rniaSourceDestCheck,
    rniaDryRun,
    rniaNetworkInterfaceId,

    -- * Destructuring the response
    ResetNetworkInterfaceAttributeResponse (..),
    mkResetNetworkInterfaceAttributeResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for ResetNetworkInterfaceAttribute.
--
-- /See:/ 'mkResetNetworkInterfaceAttribute' smart constructor.
data ResetNetworkInterfaceAttribute = ResetNetworkInterfaceAttribute'
  { sourceDestCheck ::
      Lude.Maybe Lude.Text,
    dryRun ::
      Lude.Maybe Lude.Bool,
    networkInterfaceId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResetNetworkInterfaceAttribute' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'networkInterfaceId' - The ID of the network interface.
-- * 'sourceDestCheck' - The source/destination checking attribute. Resets the value to @true@ .
mkResetNetworkInterfaceAttribute ::
  -- | 'networkInterfaceId'
  Lude.Text ->
  ResetNetworkInterfaceAttribute
mkResetNetworkInterfaceAttribute pNetworkInterfaceId_ =
  ResetNetworkInterfaceAttribute'
    { sourceDestCheck = Lude.Nothing,
      dryRun = Lude.Nothing,
      networkInterfaceId = pNetworkInterfaceId_
    }

-- | The source/destination checking attribute. Resets the value to @true@ .
--
-- /Note:/ Consider using 'sourceDestCheck' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rniaSourceDestCheck :: Lens.Lens' ResetNetworkInterfaceAttribute (Lude.Maybe Lude.Text)
rniaSourceDestCheck = Lens.lens (sourceDestCheck :: ResetNetworkInterfaceAttribute -> Lude.Maybe Lude.Text) (\s a -> s {sourceDestCheck = a} :: ResetNetworkInterfaceAttribute)
{-# DEPRECATED rniaSourceDestCheck "Use generic-lens or generic-optics with 'sourceDestCheck' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rniaDryRun :: Lens.Lens' ResetNetworkInterfaceAttribute (Lude.Maybe Lude.Bool)
rniaDryRun = Lens.lens (dryRun :: ResetNetworkInterfaceAttribute -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ResetNetworkInterfaceAttribute)
{-# DEPRECATED rniaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rniaNetworkInterfaceId :: Lens.Lens' ResetNetworkInterfaceAttribute Lude.Text
rniaNetworkInterfaceId = Lens.lens (networkInterfaceId :: ResetNetworkInterfaceAttribute -> Lude.Text) (\s a -> s {networkInterfaceId = a} :: ResetNetworkInterfaceAttribute)
{-# DEPRECATED rniaNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

instance Lude.AWSRequest ResetNetworkInterfaceAttribute where
  type
    Rs ResetNetworkInterfaceAttribute =
      ResetNetworkInterfaceAttributeResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull ResetNetworkInterfaceAttributeResponse'

instance Lude.ToHeaders ResetNetworkInterfaceAttribute where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ResetNetworkInterfaceAttribute where
  toPath = Lude.const "/"

instance Lude.ToQuery ResetNetworkInterfaceAttribute where
  toQuery ResetNetworkInterfaceAttribute' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("ResetNetworkInterfaceAttribute" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "SourceDestCheck" Lude.=: sourceDestCheck,
        "DryRun" Lude.=: dryRun,
        "NetworkInterfaceId" Lude.=: networkInterfaceId
      ]

-- | /See:/ 'mkResetNetworkInterfaceAttributeResponse' smart constructor.
data ResetNetworkInterfaceAttributeResponse = ResetNetworkInterfaceAttributeResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResetNetworkInterfaceAttributeResponse' with the minimum fields required to make a request.
mkResetNetworkInterfaceAttributeResponse ::
  ResetNetworkInterfaceAttributeResponse
mkResetNetworkInterfaceAttributeResponse =
  ResetNetworkInterfaceAttributeResponse'
