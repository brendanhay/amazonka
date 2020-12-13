{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DescribeTagOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified TagOption.
module Network.AWS.ServiceCatalog.DescribeTagOption
  ( -- * Creating a request
    DescribeTagOption (..),
    mkDescribeTagOption,

    -- ** Request lenses
    dtoId,

    -- * Destructuring the response
    DescribeTagOptionResponse (..),
    mkDescribeTagOptionResponse,

    -- ** Response lenses
    dtofrsTagOptionDetail,
    dtofrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkDescribeTagOption' smart constructor.
newtype DescribeTagOption = DescribeTagOption'
  { -- | The TagOption identifier.
    id :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTagOption' with the minimum fields required to make a request.
--
-- * 'id' - The TagOption identifier.
mkDescribeTagOption ::
  -- | 'id'
  Lude.Text ->
  DescribeTagOption
mkDescribeTagOption pId_ = DescribeTagOption' {id = pId_}

-- | The TagOption identifier.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtoId :: Lens.Lens' DescribeTagOption Lude.Text
dtoId = Lens.lens (id :: DescribeTagOption -> Lude.Text) (\s a -> s {id = a} :: DescribeTagOption)
{-# DEPRECATED dtoId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest DescribeTagOption where
  type Rs DescribeTagOption = DescribeTagOptionResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeTagOptionResponse'
            Lude.<$> (x Lude..?> "TagOptionDetail")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeTagOption where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.DescribeTagOption" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeTagOption where
  toJSON DescribeTagOption' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Id" Lude..= id)])

instance Lude.ToPath DescribeTagOption where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeTagOption where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeTagOptionResponse' smart constructor.
data DescribeTagOptionResponse = DescribeTagOptionResponse'
  { -- | Information about the TagOption.
    tagOptionDetail :: Lude.Maybe TagOptionDetail,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTagOptionResponse' with the minimum fields required to make a request.
--
-- * 'tagOptionDetail' - Information about the TagOption.
-- * 'responseStatus' - The response status code.
mkDescribeTagOptionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeTagOptionResponse
mkDescribeTagOptionResponse pResponseStatus_ =
  DescribeTagOptionResponse'
    { tagOptionDetail = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the TagOption.
--
-- /Note:/ Consider using 'tagOptionDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtofrsTagOptionDetail :: Lens.Lens' DescribeTagOptionResponse (Lude.Maybe TagOptionDetail)
dtofrsTagOptionDetail = Lens.lens (tagOptionDetail :: DescribeTagOptionResponse -> Lude.Maybe TagOptionDetail) (\s a -> s {tagOptionDetail = a} :: DescribeTagOptionResponse)
{-# DEPRECATED dtofrsTagOptionDetail "Use generic-lens or generic-optics with 'tagOptionDetail' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtofrsResponseStatus :: Lens.Lens' DescribeTagOptionResponse Lude.Int
dtofrsResponseStatus = Lens.lens (responseStatus :: DescribeTagOptionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeTagOptionResponse)
{-# DEPRECATED dtofrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
