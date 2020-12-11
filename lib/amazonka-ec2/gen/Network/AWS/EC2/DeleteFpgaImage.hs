{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteFpgaImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Amazon FPGA Image (AFI).
module Network.AWS.EC2.DeleteFpgaImage
  ( -- * Creating a request
    DeleteFpgaImage (..),
    mkDeleteFpgaImage,

    -- ** Request lenses
    dfiDryRun,
    dfiFpgaImageId,

    -- * Destructuring the response
    DeleteFpgaImageResponse (..),
    mkDeleteFpgaImageResponse,

    -- ** Response lenses
    dfifrsReturn,
    dfifrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteFpgaImage' smart constructor.
data DeleteFpgaImage = DeleteFpgaImage'
  { dryRun ::
      Lude.Maybe Lude.Bool,
    fpgaImageId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteFpgaImage' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'fpgaImageId' - The ID of the AFI.
mkDeleteFpgaImage ::
  -- | 'fpgaImageId'
  Lude.Text ->
  DeleteFpgaImage
mkDeleteFpgaImage pFpgaImageId_ =
  DeleteFpgaImage'
    { dryRun = Lude.Nothing,
      fpgaImageId = pFpgaImageId_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfiDryRun :: Lens.Lens' DeleteFpgaImage (Lude.Maybe Lude.Bool)
dfiDryRun = Lens.lens (dryRun :: DeleteFpgaImage -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeleteFpgaImage)
{-# DEPRECATED dfiDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the AFI.
--
-- /Note:/ Consider using 'fpgaImageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfiFpgaImageId :: Lens.Lens' DeleteFpgaImage Lude.Text
dfiFpgaImageId = Lens.lens (fpgaImageId :: DeleteFpgaImage -> Lude.Text) (\s a -> s {fpgaImageId = a} :: DeleteFpgaImage)
{-# DEPRECATED dfiFpgaImageId "Use generic-lens or generic-optics with 'fpgaImageId' instead." #-}

instance Lude.AWSRequest DeleteFpgaImage where
  type Rs DeleteFpgaImage = DeleteFpgaImageResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DeleteFpgaImageResponse'
            Lude.<$> (x Lude..@? "return") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteFpgaImage where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteFpgaImage where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteFpgaImage where
  toQuery DeleteFpgaImage' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteFpgaImage" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "FpgaImageId" Lude.=: fpgaImageId
      ]

-- | /See:/ 'mkDeleteFpgaImageResponse' smart constructor.
data DeleteFpgaImageResponse = DeleteFpgaImageResponse'
  { return ::
      Lude.Maybe Lude.Bool,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteFpgaImageResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'return' - Is @true@ if the request succeeds, and an error otherwise.
mkDeleteFpgaImageResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteFpgaImageResponse
mkDeleteFpgaImageResponse pResponseStatus_ =
  DeleteFpgaImageResponse'
    { return = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Is @true@ if the request succeeds, and an error otherwise.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfifrsReturn :: Lens.Lens' DeleteFpgaImageResponse (Lude.Maybe Lude.Bool)
dfifrsReturn = Lens.lens (return :: DeleteFpgaImageResponse -> Lude.Maybe Lude.Bool) (\s a -> s {return = a} :: DeleteFpgaImageResponse)
{-# DEPRECATED dfifrsReturn "Use generic-lens or generic-optics with 'return' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfifrsResponseStatus :: Lens.Lens' DeleteFpgaImageResponse Lude.Int
dfifrsResponseStatus = Lens.lens (responseStatus :: DeleteFpgaImageResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteFpgaImageResponse)
{-# DEPRECATED dfifrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
