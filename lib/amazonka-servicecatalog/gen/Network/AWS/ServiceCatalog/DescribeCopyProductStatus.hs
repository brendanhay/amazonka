{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DescribeCopyProductStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the status of the specified copy product operation.
module Network.AWS.ServiceCatalog.DescribeCopyProductStatus
  ( -- * Creating a request
    DescribeCopyProductStatus (..),
    mkDescribeCopyProductStatus,

    -- ** Request lenses
    dcpsCopyProductToken,
    dcpsAcceptLanguage,

    -- * Destructuring the response
    DescribeCopyProductStatusResponse (..),
    mkDescribeCopyProductStatusResponse,

    -- ** Response lenses
    dcpsrsTargetProductId,
    dcpsrsCopyProductStatus,
    dcpsrsStatusDetail,
    dcpsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkDescribeCopyProductStatus' smart constructor.
data DescribeCopyProductStatus = DescribeCopyProductStatus'
  { -- | The token for the copy product operation. This token is returned by 'CopyProduct' .
    copyProductToken :: Lude.Text,
    -- | The language code.
    --
    --
    --     * @en@ - English (default)
    --
    --
    --     * @jp@ - Japanese
    --
    --
    --     * @zh@ - Chinese
    acceptLanguage :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCopyProductStatus' with the minimum fields required to make a request.
--
-- * 'copyProductToken' - The token for the copy product operation. This token is returned by 'CopyProduct' .
-- * 'acceptLanguage' - The language code.
--
--
--     * @en@ - English (default)
--
--
--     * @jp@ - Japanese
--
--
--     * @zh@ - Chinese
mkDescribeCopyProductStatus ::
  -- | 'copyProductToken'
  Lude.Text ->
  DescribeCopyProductStatus
mkDescribeCopyProductStatus pCopyProductToken_ =
  DescribeCopyProductStatus'
    { copyProductToken = pCopyProductToken_,
      acceptLanguage = Lude.Nothing
    }

-- | The token for the copy product operation. This token is returned by 'CopyProduct' .
--
-- /Note:/ Consider using 'copyProductToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpsCopyProductToken :: Lens.Lens' DescribeCopyProductStatus Lude.Text
dcpsCopyProductToken = Lens.lens (copyProductToken :: DescribeCopyProductStatus -> Lude.Text) (\s a -> s {copyProductToken = a} :: DescribeCopyProductStatus)
{-# DEPRECATED dcpsCopyProductToken "Use generic-lens or generic-optics with 'copyProductToken' instead." #-}

-- | The language code.
--
--
--     * @en@ - English (default)
--
--
--     * @jp@ - Japanese
--
--
--     * @zh@ - Chinese
--
--
--
-- /Note:/ Consider using 'acceptLanguage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpsAcceptLanguage :: Lens.Lens' DescribeCopyProductStatus (Lude.Maybe Lude.Text)
dcpsAcceptLanguage = Lens.lens (acceptLanguage :: DescribeCopyProductStatus -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: DescribeCopyProductStatus)
{-# DEPRECATED dcpsAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

instance Lude.AWSRequest DescribeCopyProductStatus where
  type
    Rs DescribeCopyProductStatus =
      DescribeCopyProductStatusResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeCopyProductStatusResponse'
            Lude.<$> (x Lude..?> "TargetProductId")
            Lude.<*> (x Lude..?> "CopyProductStatus")
            Lude.<*> (x Lude..?> "StatusDetail")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeCopyProductStatus where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.DescribeCopyProductStatus" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeCopyProductStatus where
  toJSON DescribeCopyProductStatus' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("CopyProductToken" Lude..= copyProductToken),
            ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage
          ]
      )

instance Lude.ToPath DescribeCopyProductStatus where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeCopyProductStatus where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeCopyProductStatusResponse' smart constructor.
data DescribeCopyProductStatusResponse = DescribeCopyProductStatusResponse'
  { -- | The identifier of the copied product.
    targetProductId :: Lude.Maybe Lude.Text,
    -- | The status of the copy product operation.
    copyProductStatus :: Lude.Maybe CopyProductStatus,
    -- | The status message.
    statusDetail :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCopyProductStatusResponse' with the minimum fields required to make a request.
--
-- * 'targetProductId' - The identifier of the copied product.
-- * 'copyProductStatus' - The status of the copy product operation.
-- * 'statusDetail' - The status message.
-- * 'responseStatus' - The response status code.
mkDescribeCopyProductStatusResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeCopyProductStatusResponse
mkDescribeCopyProductStatusResponse pResponseStatus_ =
  DescribeCopyProductStatusResponse'
    { targetProductId =
        Lude.Nothing,
      copyProductStatus = Lude.Nothing,
      statusDetail = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identifier of the copied product.
--
-- /Note:/ Consider using 'targetProductId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpsrsTargetProductId :: Lens.Lens' DescribeCopyProductStatusResponse (Lude.Maybe Lude.Text)
dcpsrsTargetProductId = Lens.lens (targetProductId :: DescribeCopyProductStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {targetProductId = a} :: DescribeCopyProductStatusResponse)
{-# DEPRECATED dcpsrsTargetProductId "Use generic-lens or generic-optics with 'targetProductId' instead." #-}

-- | The status of the copy product operation.
--
-- /Note:/ Consider using 'copyProductStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpsrsCopyProductStatus :: Lens.Lens' DescribeCopyProductStatusResponse (Lude.Maybe CopyProductStatus)
dcpsrsCopyProductStatus = Lens.lens (copyProductStatus :: DescribeCopyProductStatusResponse -> Lude.Maybe CopyProductStatus) (\s a -> s {copyProductStatus = a} :: DescribeCopyProductStatusResponse)
{-# DEPRECATED dcpsrsCopyProductStatus "Use generic-lens or generic-optics with 'copyProductStatus' instead." #-}

-- | The status message.
--
-- /Note:/ Consider using 'statusDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpsrsStatusDetail :: Lens.Lens' DescribeCopyProductStatusResponse (Lude.Maybe Lude.Text)
dcpsrsStatusDetail = Lens.lens (statusDetail :: DescribeCopyProductStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {statusDetail = a} :: DescribeCopyProductStatusResponse)
{-# DEPRECATED dcpsrsStatusDetail "Use generic-lens or generic-optics with 'statusDetail' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpsrsResponseStatus :: Lens.Lens' DescribeCopyProductStatusResponse Lude.Int
dcpsrsResponseStatus = Lens.lens (responseStatus :: DescribeCopyProductStatusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeCopyProductStatusResponse)
{-# DEPRECATED dcpsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
