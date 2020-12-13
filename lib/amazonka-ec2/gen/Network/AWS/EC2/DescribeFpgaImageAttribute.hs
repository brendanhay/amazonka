{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeFpgaImageAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified attribute of the specified Amazon FPGA Image (AFI).
module Network.AWS.EC2.DescribeFpgaImageAttribute
  ( -- * Creating a request
    DescribeFpgaImageAttribute (..),
    mkDescribeFpgaImageAttribute,

    -- ** Request lenses
    dfiaAttribute,
    dfiaFpgaImageId,
    dfiaDryRun,

    -- * Destructuring the response
    DescribeFpgaImageAttributeResponse (..),
    mkDescribeFpgaImageAttributeResponse,

    -- ** Response lenses
    dfiarsFpgaImageAttribute,
    dfiarsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeFpgaImageAttribute' smart constructor.
data DescribeFpgaImageAttribute = DescribeFpgaImageAttribute'
  { -- | The AFI attribute.
    attribute :: FpgaImageAttributeName,
    -- | The ID of the AFI.
    fpgaImageId :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeFpgaImageAttribute' with the minimum fields required to make a request.
--
-- * 'attribute' - The AFI attribute.
-- * 'fpgaImageId' - The ID of the AFI.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkDescribeFpgaImageAttribute ::
  -- | 'attribute'
  FpgaImageAttributeName ->
  -- | 'fpgaImageId'
  Lude.Text ->
  DescribeFpgaImageAttribute
mkDescribeFpgaImageAttribute pAttribute_ pFpgaImageId_ =
  DescribeFpgaImageAttribute'
    { attribute = pAttribute_,
      fpgaImageId = pFpgaImageId_,
      dryRun = Lude.Nothing
    }

-- | The AFI attribute.
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfiaAttribute :: Lens.Lens' DescribeFpgaImageAttribute FpgaImageAttributeName
dfiaAttribute = Lens.lens (attribute :: DescribeFpgaImageAttribute -> FpgaImageAttributeName) (\s a -> s {attribute = a} :: DescribeFpgaImageAttribute)
{-# DEPRECATED dfiaAttribute "Use generic-lens or generic-optics with 'attribute' instead." #-}

-- | The ID of the AFI.
--
-- /Note:/ Consider using 'fpgaImageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfiaFpgaImageId :: Lens.Lens' DescribeFpgaImageAttribute Lude.Text
dfiaFpgaImageId = Lens.lens (fpgaImageId :: DescribeFpgaImageAttribute -> Lude.Text) (\s a -> s {fpgaImageId = a} :: DescribeFpgaImageAttribute)
{-# DEPRECATED dfiaFpgaImageId "Use generic-lens or generic-optics with 'fpgaImageId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfiaDryRun :: Lens.Lens' DescribeFpgaImageAttribute (Lude.Maybe Lude.Bool)
dfiaDryRun = Lens.lens (dryRun :: DescribeFpgaImageAttribute -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeFpgaImageAttribute)
{-# DEPRECATED dfiaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest DescribeFpgaImageAttribute where
  type
    Rs DescribeFpgaImageAttribute =
      DescribeFpgaImageAttributeResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeFpgaImageAttributeResponse'
            Lude.<$> (x Lude..@? "fpgaImageAttribute")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeFpgaImageAttribute where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeFpgaImageAttribute where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeFpgaImageAttribute where
  toQuery DescribeFpgaImageAttribute' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeFpgaImageAttribute" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "Attribute" Lude.=: attribute,
        "FpgaImageId" Lude.=: fpgaImageId,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkDescribeFpgaImageAttributeResponse' smart constructor.
data DescribeFpgaImageAttributeResponse = DescribeFpgaImageAttributeResponse'
  { -- | Information about the attribute.
    fpgaImageAttribute :: Lude.Maybe FpgaImageAttribute,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeFpgaImageAttributeResponse' with the minimum fields required to make a request.
--
-- * 'fpgaImageAttribute' - Information about the attribute.
-- * 'responseStatus' - The response status code.
mkDescribeFpgaImageAttributeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeFpgaImageAttributeResponse
mkDescribeFpgaImageAttributeResponse pResponseStatus_ =
  DescribeFpgaImageAttributeResponse'
    { fpgaImageAttribute =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the attribute.
--
-- /Note:/ Consider using 'fpgaImageAttribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfiarsFpgaImageAttribute :: Lens.Lens' DescribeFpgaImageAttributeResponse (Lude.Maybe FpgaImageAttribute)
dfiarsFpgaImageAttribute = Lens.lens (fpgaImageAttribute :: DescribeFpgaImageAttributeResponse -> Lude.Maybe FpgaImageAttribute) (\s a -> s {fpgaImageAttribute = a} :: DescribeFpgaImageAttributeResponse)
{-# DEPRECATED dfiarsFpgaImageAttribute "Use generic-lens or generic-optics with 'fpgaImageAttribute' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfiarsResponseStatus :: Lens.Lens' DescribeFpgaImageAttributeResponse Lude.Int
dfiarsResponseStatus = Lens.lens (responseStatus :: DescribeFpgaImageAttributeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeFpgaImageAttributeResponse)
{-# DEPRECATED dfiarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
