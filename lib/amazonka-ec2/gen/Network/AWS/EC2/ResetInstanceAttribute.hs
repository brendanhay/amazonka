{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ResetInstanceAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets an attribute of an instance to its default value. To reset the @kernel@ or @ramdisk@ , the instance must be in a stopped state. To reset the @sourceDestCheck@ , the instance can be either running or stopped.
--
-- The @sourceDestCheck@ attribute controls whether source/destination checking is enabled. The default value is @true@ , which means checking is enabled. This value must be @false@ for a NAT instance to perform NAT. For more information, see <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_NAT_Instance.html NAT Instances> in the /Amazon Virtual Private Cloud User Guide/ .
module Network.AWS.EC2.ResetInstanceAttribute
  ( -- * Creating a request
    ResetInstanceAttribute (..),
    mkResetInstanceAttribute,

    -- ** Request lenses
    riaDryRun,
    riaAttribute,
    riaInstanceId,

    -- * Destructuring the response
    ResetInstanceAttributeResponse (..),
    mkResetInstanceAttributeResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkResetInstanceAttribute' smart constructor.
data ResetInstanceAttribute = ResetInstanceAttribute'
  { dryRun ::
      Lude.Maybe Lude.Bool,
    attribute :: InstanceAttributeName,
    instanceId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResetInstanceAttribute' with the minimum fields required to make a request.
--
-- * 'attribute' - The attribute to reset.
--
-- /Important:/ You can only reset the following attributes: @kernel@ | @ramdisk@ | @sourceDestCheck@ . To change an instance attribute, use 'ModifyInstanceAttribute' .
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'instanceId' - The ID of the instance.
mkResetInstanceAttribute ::
  -- | 'attribute'
  InstanceAttributeName ->
  -- | 'instanceId'
  Lude.Text ->
  ResetInstanceAttribute
mkResetInstanceAttribute pAttribute_ pInstanceId_ =
  ResetInstanceAttribute'
    { dryRun = Lude.Nothing,
      attribute = pAttribute_,
      instanceId = pInstanceId_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riaDryRun :: Lens.Lens' ResetInstanceAttribute (Lude.Maybe Lude.Bool)
riaDryRun = Lens.lens (dryRun :: ResetInstanceAttribute -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ResetInstanceAttribute)
{-# DEPRECATED riaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The attribute to reset.
--
-- /Important:/ You can only reset the following attributes: @kernel@ | @ramdisk@ | @sourceDestCheck@ . To change an instance attribute, use 'ModifyInstanceAttribute' .
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riaAttribute :: Lens.Lens' ResetInstanceAttribute InstanceAttributeName
riaAttribute = Lens.lens (attribute :: ResetInstanceAttribute -> InstanceAttributeName) (\s a -> s {attribute = a} :: ResetInstanceAttribute)
{-# DEPRECATED riaAttribute "Use generic-lens or generic-optics with 'attribute' instead." #-}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riaInstanceId :: Lens.Lens' ResetInstanceAttribute Lude.Text
riaInstanceId = Lens.lens (instanceId :: ResetInstanceAttribute -> Lude.Text) (\s a -> s {instanceId = a} :: ResetInstanceAttribute)
{-# DEPRECATED riaInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Lude.AWSRequest ResetInstanceAttribute where
  type Rs ResetInstanceAttribute = ResetInstanceAttributeResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull ResetInstanceAttributeResponse'

instance Lude.ToHeaders ResetInstanceAttribute where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ResetInstanceAttribute where
  toPath = Lude.const "/"

instance Lude.ToQuery ResetInstanceAttribute where
  toQuery ResetInstanceAttribute' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ResetInstanceAttribute" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "Attribute" Lude.=: attribute,
        "InstanceId" Lude.=: instanceId
      ]

-- | /See:/ 'mkResetInstanceAttributeResponse' smart constructor.
data ResetInstanceAttributeResponse = ResetInstanceAttributeResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResetInstanceAttributeResponse' with the minimum fields required to make a request.
mkResetInstanceAttributeResponse ::
  ResetInstanceAttributeResponse
mkResetInstanceAttributeResponse = ResetInstanceAttributeResponse'
