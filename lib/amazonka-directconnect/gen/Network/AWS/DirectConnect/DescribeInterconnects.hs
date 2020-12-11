{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.DescribeInterconnects
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the interconnects owned by the AWS account or only the specified interconnect.
module Network.AWS.DirectConnect.DescribeInterconnects
  ( -- * Creating a request
    DescribeInterconnects (..),
    mkDescribeInterconnects,

    -- ** Request lenses
    diInterconnectId,

    -- * Destructuring the response
    DescribeInterconnectsResponse (..),
    mkDescribeInterconnectsResponse,

    -- ** Response lenses
    dirsInterconnects,
    dirsResponseStatus,
  )
where

import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeInterconnects' smart constructor.
newtype DescribeInterconnects = DescribeInterconnects'
  { interconnectId ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeInterconnects' with the minimum fields required to make a request.
--
-- * 'interconnectId' - The ID of the interconnect.
mkDescribeInterconnects ::
  DescribeInterconnects
mkDescribeInterconnects =
  DescribeInterconnects' {interconnectId = Lude.Nothing}

-- | The ID of the interconnect.
--
-- /Note:/ Consider using 'interconnectId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diInterconnectId :: Lens.Lens' DescribeInterconnects (Lude.Maybe Lude.Text)
diInterconnectId = Lens.lens (interconnectId :: DescribeInterconnects -> Lude.Maybe Lude.Text) (\s a -> s {interconnectId = a} :: DescribeInterconnects)
{-# DEPRECATED diInterconnectId "Use generic-lens or generic-optics with 'interconnectId' instead." #-}

instance Lude.AWSRequest DescribeInterconnects where
  type Rs DescribeInterconnects = DescribeInterconnectsResponse
  request = Req.postJSON directConnectService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeInterconnectsResponse'
            Lude.<$> (x Lude..?> "interconnects" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeInterconnects where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OvertureService.DescribeInterconnects" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeInterconnects where
  toJSON DescribeInterconnects' {..} =
    Lude.object
      ( Lude.catMaybes
          [("interconnectId" Lude..=) Lude.<$> interconnectId]
      )

instance Lude.ToPath DescribeInterconnects where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeInterconnects where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeInterconnectsResponse' smart constructor.
data DescribeInterconnectsResponse = DescribeInterconnectsResponse'
  { interconnects ::
      Lude.Maybe [Interconnect],
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

-- | Creates a value of 'DescribeInterconnectsResponse' with the minimum fields required to make a request.
--
-- * 'interconnects' - The interconnects.
-- * 'responseStatus' - The response status code.
mkDescribeInterconnectsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeInterconnectsResponse
mkDescribeInterconnectsResponse pResponseStatus_ =
  DescribeInterconnectsResponse'
    { interconnects = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The interconnects.
--
-- /Note:/ Consider using 'interconnects' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirsInterconnects :: Lens.Lens' DescribeInterconnectsResponse (Lude.Maybe [Interconnect])
dirsInterconnects = Lens.lens (interconnects :: DescribeInterconnectsResponse -> Lude.Maybe [Interconnect]) (\s a -> s {interconnects = a} :: DescribeInterconnectsResponse)
{-# DEPRECATED dirsInterconnects "Use generic-lens or generic-optics with 'interconnects' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirsResponseStatus :: Lens.Lens' DescribeInterconnectsResponse Lude.Int
dirsResponseStatus = Lens.lens (responseStatus :: DescribeInterconnectsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeInterconnectsResponse)
{-# DEPRECATED dirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
