{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.ListHSMs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__ . For more information, see <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs> , the <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference> .
--
-- __For information about the current version of AWS CloudHSM__ , see <http://aws.amazon.com/cloudhsm/ AWS CloudHSM> , the <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference> .
-- Retrieves the identifiers of all of the HSMs provisioned for the current customer.
-- This operation supports pagination with the use of the @NextToken@ member. If more results are available, the @NextToken@ member of the response contains a token that you pass in the next call to @ListHsms@ to retrieve the next set of items.
--
-- This operation returns paginated results.
module Network.AWS.CloudHSM.ListHSMs
  ( -- * Creating a request
    ListHSMs (..),
    mkListHSMs,

    -- ** Request lenses
    lhNextToken,

    -- * Destructuring the response
    ListHSMsResponse (..),
    mkListHSMsResponse,

    -- ** Response lenses
    lhrsNextToken,
    lhrsHSMList,
    lhrsResponseStatus,
  )
where

import Network.AWS.CloudHSM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListHSMs' smart constructor.
newtype ListHSMs = ListHSMs'
  { -- | The @NextToken@ value from a previous call to @ListHsms@ . Pass null if this is the first call.
    nextToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListHSMs' with the minimum fields required to make a request.
--
-- * 'nextToken' - The @NextToken@ value from a previous call to @ListHsms@ . Pass null if this is the first call.
mkListHSMs ::
  ListHSMs
mkListHSMs = ListHSMs' {nextToken = Lude.Nothing}

-- | The @NextToken@ value from a previous call to @ListHsms@ . Pass null if this is the first call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhNextToken :: Lens.Lens' ListHSMs (Lude.Maybe Lude.Text)
lhNextToken = Lens.lens (nextToken :: ListHSMs -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListHSMs)
{-# DEPRECATED lhNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Page.AWSPager ListHSMs where
  page rq rs
    | Page.stop (rs Lens.^. lhrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lhrsHSMList) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lhNextToken Lens..~ rs Lens.^. lhrsNextToken

instance Lude.AWSRequest ListHSMs where
  type Rs ListHSMs = ListHSMsResponse
  request = Req.postJSON cloudHSMService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListHSMsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "HsmList" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListHSMs where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CloudHsmFrontendService.ListHsms" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListHSMs where
  toJSON ListHSMs' {..} =
    Lude.object
      (Lude.catMaybes [("NextToken" Lude..=) Lude.<$> nextToken])

instance Lude.ToPath ListHSMs where
  toPath = Lude.const "/"

instance Lude.ToQuery ListHSMs where
  toQuery = Lude.const Lude.mempty

-- | Contains the output of the @ListHsms@ operation.
--
-- /See:/ 'mkListHSMsResponse' smart constructor.
data ListHSMsResponse = ListHSMsResponse'
  { -- | If not null, more results are available. Pass this value to @ListHsms@ to retrieve the next set of items.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The list of ARNs that identify the HSMs.
    hsmList :: Lude.Maybe [Lude.Text],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListHSMsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If not null, more results are available. Pass this value to @ListHsms@ to retrieve the next set of items.
-- * 'hsmList' - The list of ARNs that identify the HSMs.
-- * 'responseStatus' - The response status code.
mkListHSMsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListHSMsResponse
mkListHSMsResponse pResponseStatus_ =
  ListHSMsResponse'
    { nextToken = Lude.Nothing,
      hsmList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If not null, more results are available. Pass this value to @ListHsms@ to retrieve the next set of items.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhrsNextToken :: Lens.Lens' ListHSMsResponse (Lude.Maybe Lude.Text)
lhrsNextToken = Lens.lens (nextToken :: ListHSMsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListHSMsResponse)
{-# DEPRECATED lhrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The list of ARNs that identify the HSMs.
--
-- /Note:/ Consider using 'hsmList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhrsHSMList :: Lens.Lens' ListHSMsResponse (Lude.Maybe [Lude.Text])
lhrsHSMList = Lens.lens (hsmList :: ListHSMsResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {hsmList = a} :: ListHSMsResponse)
{-# DEPRECATED lhrsHSMList "Use generic-lens or generic-optics with 'hsmList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhrsResponseStatus :: Lens.Lens' ListHSMsResponse Lude.Int
lhrsResponseStatus = Lens.lens (responseStatus :: ListHSMsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListHSMsResponse)
{-# DEPRECATED lhrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
