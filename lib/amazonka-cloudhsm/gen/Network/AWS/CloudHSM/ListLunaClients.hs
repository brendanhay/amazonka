{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.ListLunaClients
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__ . For more information, see <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs> , the <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference> .
--
-- __For information about the current version of AWS CloudHSM__ , see <http://aws.amazon.com/cloudhsm/ AWS CloudHSM> , the <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference> .
-- Lists all of the clients.
-- This operation supports pagination with the use of the @NextToken@ member. If more results are available, the @NextToken@ member of the response contains a token that you pass in the next call to @ListLunaClients@ to retrieve the next set of items.
--
-- This operation returns paginated results.
module Network.AWS.CloudHSM.ListLunaClients
  ( -- * Creating a request
    ListLunaClients (..),
    mkListLunaClients,

    -- ** Request lenses
    llcNextToken,

    -- * Destructuring the response
    ListLunaClientsResponse (..),
    mkListLunaClientsResponse,

    -- ** Response lenses
    llcrsNextToken,
    llcrsClientList,
    llcrsResponseStatus,
  )
where

import Network.AWS.CloudHSM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListLunaClients' smart constructor.
newtype ListLunaClients = ListLunaClients'
  { -- | The @NextToken@ value from a previous call to @ListLunaClients@ . Pass null if this is the first call.
    nextToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListLunaClients' with the minimum fields required to make a request.
--
-- * 'nextToken' - The @NextToken@ value from a previous call to @ListLunaClients@ . Pass null if this is the first call.
mkListLunaClients ::
  ListLunaClients
mkListLunaClients = ListLunaClients' {nextToken = Lude.Nothing}

-- | The @NextToken@ value from a previous call to @ListLunaClients@ . Pass null if this is the first call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llcNextToken :: Lens.Lens' ListLunaClients (Lude.Maybe Lude.Text)
llcNextToken = Lens.lens (nextToken :: ListLunaClients -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListLunaClients)
{-# DEPRECATED llcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Page.AWSPager ListLunaClients where
  page rq rs
    | Page.stop (rs Lens.^. llcrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. llcrsClientList) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& llcNextToken Lens..~ rs Lens.^. llcrsNextToken

instance Lude.AWSRequest ListLunaClients where
  type Rs ListLunaClients = ListLunaClientsResponse
  request = Req.postJSON cloudHSMService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListLunaClientsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "ClientList" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListLunaClients where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CloudHsmFrontendService.ListLunaClients" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListLunaClients where
  toJSON ListLunaClients' {..} =
    Lude.object
      (Lude.catMaybes [("NextToken" Lude..=) Lude.<$> nextToken])

instance Lude.ToPath ListLunaClients where
  toPath = Lude.const "/"

instance Lude.ToQuery ListLunaClients where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListLunaClientsResponse' smart constructor.
data ListLunaClientsResponse = ListLunaClientsResponse'
  { -- | If not null, more results are available. Pass this to @ListLunaClients@ to retrieve the next set of items.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The list of clients.
    clientList :: [Lude.Text],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListLunaClientsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If not null, more results are available. Pass this to @ListLunaClients@ to retrieve the next set of items.
-- * 'clientList' - The list of clients.
-- * 'responseStatus' - The response status code.
mkListLunaClientsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListLunaClientsResponse
mkListLunaClientsResponse pResponseStatus_ =
  ListLunaClientsResponse'
    { nextToken = Lude.Nothing,
      clientList = Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | If not null, more results are available. Pass this to @ListLunaClients@ to retrieve the next set of items.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llcrsNextToken :: Lens.Lens' ListLunaClientsResponse (Lude.Maybe Lude.Text)
llcrsNextToken = Lens.lens (nextToken :: ListLunaClientsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListLunaClientsResponse)
{-# DEPRECATED llcrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The list of clients.
--
-- /Note:/ Consider using 'clientList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llcrsClientList :: Lens.Lens' ListLunaClientsResponse [Lude.Text]
llcrsClientList = Lens.lens (clientList :: ListLunaClientsResponse -> [Lude.Text]) (\s a -> s {clientList = a} :: ListLunaClientsResponse)
{-# DEPRECATED llcrsClientList "Use generic-lens or generic-optics with 'clientList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llcrsResponseStatus :: Lens.Lens' ListLunaClientsResponse Lude.Int
llcrsResponseStatus = Lens.lens (responseStatus :: ListLunaClientsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListLunaClientsResponse)
{-# DEPRECATED llcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
