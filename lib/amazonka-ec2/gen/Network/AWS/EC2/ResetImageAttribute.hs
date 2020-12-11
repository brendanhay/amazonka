{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ResetImageAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets an attribute of an AMI to its default value.
module Network.AWS.EC2.ResetImageAttribute
  ( -- * Creating a request
    ResetImageAttribute (..),
    mkResetImageAttribute,

    -- ** Request lenses
    resDryRun,
    resAttribute,
    resImageId,

    -- * Destructuring the response
    ResetImageAttributeResponse (..),
    mkResetImageAttributeResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for ResetImageAttribute.
--
-- /See:/ 'mkResetImageAttribute' smart constructor.
data ResetImageAttribute = ResetImageAttribute'
  { dryRun ::
      Lude.Maybe Lude.Bool,
    attribute :: ResetImageAttributeName,
    imageId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResetImageAttribute' with the minimum fields required to make a request.
--
-- * 'attribute' - The attribute to reset (currently you can only reset the launch permission attribute).
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'imageId' - The ID of the AMI.
mkResetImageAttribute ::
  -- | 'attribute'
  ResetImageAttributeName ->
  -- | 'imageId'
  Lude.Text ->
  ResetImageAttribute
mkResetImageAttribute pAttribute_ pImageId_ =
  ResetImageAttribute'
    { dryRun = Lude.Nothing,
      attribute = pAttribute_,
      imageId = pImageId_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
resDryRun :: Lens.Lens' ResetImageAttribute (Lude.Maybe Lude.Bool)
resDryRun = Lens.lens (dryRun :: ResetImageAttribute -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ResetImageAttribute)
{-# DEPRECATED resDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The attribute to reset (currently you can only reset the launch permission attribute).
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
resAttribute :: Lens.Lens' ResetImageAttribute ResetImageAttributeName
resAttribute = Lens.lens (attribute :: ResetImageAttribute -> ResetImageAttributeName) (\s a -> s {attribute = a} :: ResetImageAttribute)
{-# DEPRECATED resAttribute "Use generic-lens or generic-optics with 'attribute' instead." #-}

-- | The ID of the AMI.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
resImageId :: Lens.Lens' ResetImageAttribute Lude.Text
resImageId = Lens.lens (imageId :: ResetImageAttribute -> Lude.Text) (\s a -> s {imageId = a} :: ResetImageAttribute)
{-# DEPRECATED resImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

instance Lude.AWSRequest ResetImageAttribute where
  type Rs ResetImageAttribute = ResetImageAttributeResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull ResetImageAttributeResponse'

instance Lude.ToHeaders ResetImageAttribute where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ResetImageAttribute where
  toPath = Lude.const "/"

instance Lude.ToQuery ResetImageAttribute where
  toQuery ResetImageAttribute' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ResetImageAttribute" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "Attribute" Lude.=: attribute,
        "ImageId" Lude.=: imageId
      ]

-- | /See:/ 'mkResetImageAttributeResponse' smart constructor.
data ResetImageAttributeResponse = ResetImageAttributeResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResetImageAttributeResponse' with the minimum fields required to make a request.
mkResetImageAttributeResponse ::
  ResetImageAttributeResponse
mkResetImageAttributeResponse = ResetImageAttributeResponse'
