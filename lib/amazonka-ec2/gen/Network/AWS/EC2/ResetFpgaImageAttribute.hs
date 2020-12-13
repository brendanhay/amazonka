{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ResetFpgaImageAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets the specified attribute of the specified Amazon FPGA Image (AFI) to its default value. You can only reset the load permission attribute.
module Network.AWS.EC2.ResetFpgaImageAttribute
  ( -- * Creating a request
    ResetFpgaImageAttribute (..),
    mkResetFpgaImageAttribute,

    -- ** Request lenses
    rfiaAttribute,
    rfiaFpgaImageId,
    rfiaDryRun,

    -- * Destructuring the response
    ResetFpgaImageAttributeResponse (..),
    mkResetFpgaImageAttributeResponse,

    -- ** Response lenses
    rfiarsReturn,
    rfiarsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkResetFpgaImageAttribute' smart constructor.
data ResetFpgaImageAttribute = ResetFpgaImageAttribute'
  { -- | The attribute.
    attribute :: Lude.Maybe ResetFpgaImageAttributeName,
    -- | The ID of the AFI.
    fpgaImageId :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResetFpgaImageAttribute' with the minimum fields required to make a request.
--
-- * 'attribute' - The attribute.
-- * 'fpgaImageId' - The ID of the AFI.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkResetFpgaImageAttribute ::
  -- | 'fpgaImageId'
  Lude.Text ->
  ResetFpgaImageAttribute
mkResetFpgaImageAttribute pFpgaImageId_ =
  ResetFpgaImageAttribute'
    { attribute = Lude.Nothing,
      fpgaImageId = pFpgaImageId_,
      dryRun = Lude.Nothing
    }

-- | The attribute.
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfiaAttribute :: Lens.Lens' ResetFpgaImageAttribute (Lude.Maybe ResetFpgaImageAttributeName)
rfiaAttribute = Lens.lens (attribute :: ResetFpgaImageAttribute -> Lude.Maybe ResetFpgaImageAttributeName) (\s a -> s {attribute = a} :: ResetFpgaImageAttribute)
{-# DEPRECATED rfiaAttribute "Use generic-lens or generic-optics with 'attribute' instead." #-}

-- | The ID of the AFI.
--
-- /Note:/ Consider using 'fpgaImageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfiaFpgaImageId :: Lens.Lens' ResetFpgaImageAttribute Lude.Text
rfiaFpgaImageId = Lens.lens (fpgaImageId :: ResetFpgaImageAttribute -> Lude.Text) (\s a -> s {fpgaImageId = a} :: ResetFpgaImageAttribute)
{-# DEPRECATED rfiaFpgaImageId "Use generic-lens or generic-optics with 'fpgaImageId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfiaDryRun :: Lens.Lens' ResetFpgaImageAttribute (Lude.Maybe Lude.Bool)
rfiaDryRun = Lens.lens (dryRun :: ResetFpgaImageAttribute -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ResetFpgaImageAttribute)
{-# DEPRECATED rfiaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest ResetFpgaImageAttribute where
  type Rs ResetFpgaImageAttribute = ResetFpgaImageAttributeResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          ResetFpgaImageAttributeResponse'
            Lude.<$> (x Lude..@? "return") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ResetFpgaImageAttribute where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ResetFpgaImageAttribute where
  toPath = Lude.const "/"

instance Lude.ToQuery ResetFpgaImageAttribute where
  toQuery ResetFpgaImageAttribute' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ResetFpgaImageAttribute" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "Attribute" Lude.=: attribute,
        "FpgaImageId" Lude.=: fpgaImageId,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkResetFpgaImageAttributeResponse' smart constructor.
data ResetFpgaImageAttributeResponse = ResetFpgaImageAttributeResponse'
  { -- | Is @true@ if the request succeeds, and an error otherwise.
    return :: Lude.Maybe Lude.Bool,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResetFpgaImageAttributeResponse' with the minimum fields required to make a request.
--
-- * 'return' - Is @true@ if the request succeeds, and an error otherwise.
-- * 'responseStatus' - The response status code.
mkResetFpgaImageAttributeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ResetFpgaImageAttributeResponse
mkResetFpgaImageAttributeResponse pResponseStatus_ =
  ResetFpgaImageAttributeResponse'
    { return = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Is @true@ if the request succeeds, and an error otherwise.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfiarsReturn :: Lens.Lens' ResetFpgaImageAttributeResponse (Lude.Maybe Lude.Bool)
rfiarsReturn = Lens.lens (return :: ResetFpgaImageAttributeResponse -> Lude.Maybe Lude.Bool) (\s a -> s {return = a} :: ResetFpgaImageAttributeResponse)
{-# DEPRECATED rfiarsReturn "Use generic-lens or generic-optics with 'return' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfiarsResponseStatus :: Lens.Lens' ResetFpgaImageAttributeResponse Lude.Int
rfiarsResponseStatus = Lens.lens (responseStatus :: ResetFpgaImageAttributeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ResetFpgaImageAttributeResponse)
{-# DEPRECATED rfiarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
