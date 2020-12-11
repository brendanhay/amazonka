{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.ListInputDeviceTransfers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List input devices that are currently being transferred. List input devices that you are transferring from your AWS account or input devices that another AWS account is transferring to you.
--
-- This operation returns paginated results.
module Network.AWS.MediaLive.ListInputDeviceTransfers
  ( -- * Creating a request
    ListInputDeviceTransfers (..),
    mkListInputDeviceTransfers,

    -- ** Request lenses
    lidtNextToken,
    lidtMaxResults,
    lidtTransferType,

    -- * Destructuring the response
    ListInputDeviceTransfersResponse (..),
    mkListInputDeviceTransfersResponse,

    -- ** Response lenses
    lidtrsNextToken,
    lidtrsInputDeviceTransfers,
    lidtrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Placeholder documentation for ListInputDeviceTransfersRequest
--
-- /See:/ 'mkListInputDeviceTransfers' smart constructor.
data ListInputDeviceTransfers = ListInputDeviceTransfers'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    transferType :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListInputDeviceTransfers' with the minimum fields required to make a request.
--
-- * 'maxResults' - Undocumented field.
-- * 'nextToken' - Undocumented field.
-- * 'transferType' - Undocumented field.
mkListInputDeviceTransfers ::
  -- | 'transferType'
  Lude.Text ->
  ListInputDeviceTransfers
mkListInputDeviceTransfers pTransferType_ =
  ListInputDeviceTransfers'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      transferType = pTransferType_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lidtNextToken :: Lens.Lens' ListInputDeviceTransfers (Lude.Maybe Lude.Text)
lidtNextToken = Lens.lens (nextToken :: ListInputDeviceTransfers -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListInputDeviceTransfers)
{-# DEPRECATED lidtNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lidtMaxResults :: Lens.Lens' ListInputDeviceTransfers (Lude.Maybe Lude.Natural)
lidtMaxResults = Lens.lens (maxResults :: ListInputDeviceTransfers -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListInputDeviceTransfers)
{-# DEPRECATED lidtMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'transferType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lidtTransferType :: Lens.Lens' ListInputDeviceTransfers Lude.Text
lidtTransferType = Lens.lens (transferType :: ListInputDeviceTransfers -> Lude.Text) (\s a -> s {transferType = a} :: ListInputDeviceTransfers)
{-# DEPRECATED lidtTransferType "Use generic-lens or generic-optics with 'transferType' instead." #-}

instance Page.AWSPager ListInputDeviceTransfers where
  page rq rs
    | Page.stop (rs Lens.^. lidtrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lidtrsInputDeviceTransfers) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lidtNextToken Lens..~ rs Lens.^. lidtrsNextToken

instance Lude.AWSRequest ListInputDeviceTransfers where
  type Rs ListInputDeviceTransfers = ListInputDeviceTransfersResponse
  request = Req.get mediaLiveService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListInputDeviceTransfersResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "inputDeviceTransfers" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListInputDeviceTransfers where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListInputDeviceTransfers where
  toPath = Lude.const "/prod/inputDeviceTransfers"

instance Lude.ToQuery ListInputDeviceTransfers where
  toQuery ListInputDeviceTransfers' {..} =
    Lude.mconcat
      [ "nextToken" Lude.=: nextToken,
        "maxResults" Lude.=: maxResults,
        "transferType" Lude.=: transferType
      ]

-- | Placeholder documentation for ListInputDeviceTransfersResponse
--
-- /See:/ 'mkListInputDeviceTransfersResponse' smart constructor.
data ListInputDeviceTransfersResponse = ListInputDeviceTransfersResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    inputDeviceTransfers ::
      Lude.Maybe
        [TransferringInputDeviceSummary],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListInputDeviceTransfersResponse' with the minimum fields required to make a request.
--
-- * 'inputDeviceTransfers' - The list of devices that you are transferring or are being transferred to you.
-- * 'nextToken' - A token to get additional list results.
-- * 'responseStatus' - The response status code.
mkListInputDeviceTransfersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListInputDeviceTransfersResponse
mkListInputDeviceTransfersResponse pResponseStatus_ =
  ListInputDeviceTransfersResponse'
    { nextToken = Lude.Nothing,
      inputDeviceTransfers = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A token to get additional list results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lidtrsNextToken :: Lens.Lens' ListInputDeviceTransfersResponse (Lude.Maybe Lude.Text)
lidtrsNextToken = Lens.lens (nextToken :: ListInputDeviceTransfersResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListInputDeviceTransfersResponse)
{-# DEPRECATED lidtrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The list of devices that you are transferring or are being transferred to you.
--
-- /Note:/ Consider using 'inputDeviceTransfers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lidtrsInputDeviceTransfers :: Lens.Lens' ListInputDeviceTransfersResponse (Lude.Maybe [TransferringInputDeviceSummary])
lidtrsInputDeviceTransfers = Lens.lens (inputDeviceTransfers :: ListInputDeviceTransfersResponse -> Lude.Maybe [TransferringInputDeviceSummary]) (\s a -> s {inputDeviceTransfers = a} :: ListInputDeviceTransfersResponse)
{-# DEPRECATED lidtrsInputDeviceTransfers "Use generic-lens or generic-optics with 'inputDeviceTransfers' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lidtrsResponseStatus :: Lens.Lens' ListInputDeviceTransfersResponse Lude.Int
lidtrsResponseStatus = Lens.lens (responseStatus :: ListInputDeviceTransfersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListInputDeviceTransfersResponse)
{-# DEPRECATED lidtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
