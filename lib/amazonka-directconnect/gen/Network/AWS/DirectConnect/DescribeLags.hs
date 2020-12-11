{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.DescribeLags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes all your link aggregation groups (LAG) or the specified LAG.
module Network.AWS.DirectConnect.DescribeLags
  ( -- * Creating a request
    DescribeLags (..),
    mkDescribeLags,

    -- ** Request lenses
    dlLagId,

    -- * Destructuring the response
    DescribeLagsResponse (..),
    mkDescribeLagsResponse,

    -- ** Response lenses
    desrsLags,
    desrsResponseStatus,
  )
where

import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeLags' smart constructor.
newtype DescribeLags = DescribeLags' {lagId :: Lude.Maybe Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLags' with the minimum fields required to make a request.
--
-- * 'lagId' - The ID of the LAG.
mkDescribeLags ::
  DescribeLags
mkDescribeLags = DescribeLags' {lagId = Lude.Nothing}

-- | The ID of the LAG.
--
-- /Note:/ Consider using 'lagId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlLagId :: Lens.Lens' DescribeLags (Lude.Maybe Lude.Text)
dlLagId = Lens.lens (lagId :: DescribeLags -> Lude.Maybe Lude.Text) (\s a -> s {lagId = a} :: DescribeLags)
{-# DEPRECATED dlLagId "Use generic-lens or generic-optics with 'lagId' instead." #-}

instance Lude.AWSRequest DescribeLags where
  type Rs DescribeLags = DescribeLagsResponse
  request = Req.postJSON directConnectService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeLagsResponse'
            Lude.<$> (x Lude..?> "lags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeLags where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OvertureService.DescribeLags" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeLags where
  toJSON DescribeLags' {..} =
    Lude.object (Lude.catMaybes [("lagId" Lude..=) Lude.<$> lagId])

instance Lude.ToPath DescribeLags where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeLags where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeLagsResponse' smart constructor.
data DescribeLagsResponse = DescribeLagsResponse'
  { lags ::
      Lude.Maybe [Lag],
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

-- | Creates a value of 'DescribeLagsResponse' with the minimum fields required to make a request.
--
-- * 'lags' - The LAGs.
-- * 'responseStatus' - The response status code.
mkDescribeLagsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeLagsResponse
mkDescribeLagsResponse pResponseStatus_ =
  DescribeLagsResponse'
    { lags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The LAGs.
--
-- /Note:/ Consider using 'lags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsLags :: Lens.Lens' DescribeLagsResponse (Lude.Maybe [Lag])
desrsLags = Lens.lens (lags :: DescribeLagsResponse -> Lude.Maybe [Lag]) (\s a -> s {lags = a} :: DescribeLagsResponse)
{-# DEPRECATED desrsLags "Use generic-lens or generic-optics with 'lags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsResponseStatus :: Lens.Lens' DescribeLagsResponse Lude.Int
desrsResponseStatus = Lens.lens (responseStatus :: DescribeLagsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeLagsResponse)
{-# DEPRECATED desrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
