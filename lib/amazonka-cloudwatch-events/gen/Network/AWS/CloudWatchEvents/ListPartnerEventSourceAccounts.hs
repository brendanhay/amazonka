{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.ListPartnerEventSourceAccounts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- An SaaS partner can use this operation to display the AWS account ID that a particular partner event source name is associated with. This operation is not used by AWS customers.
module Network.AWS.CloudWatchEvents.ListPartnerEventSourceAccounts
  ( -- * Creating a request
    ListPartnerEventSourceAccounts (..),
    mkListPartnerEventSourceAccounts,

    -- ** Request lenses
    lpesaNextToken,
    lpesaLimit,
    lpesaEventSourceName,

    -- * Destructuring the response
    ListPartnerEventSourceAccountsResponse (..),
    mkListPartnerEventSourceAccountsResponse,

    -- ** Response lenses
    lpesarsPartnerEventSourceAccounts,
    lpesarsNextToken,
    lpesarsResponseStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListPartnerEventSourceAccounts' smart constructor.
data ListPartnerEventSourceAccounts = ListPartnerEventSourceAccounts'
  { nextToken ::
      Lude.Maybe Lude.Text,
    limit ::
      Lude.Maybe Lude.Natural,
    eventSourceName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPartnerEventSourceAccounts' with the minimum fields required to make a request.
--
-- * 'eventSourceName' - The name of the partner event source to display account information about.
-- * 'limit' - Specifying this limits the number of results returned by this operation. The operation also returns a NextToken which you can use in a subsequent operation to retrieve the next set of results.
-- * 'nextToken' - The token returned by a previous call to this operation. Specifying this retrieves the next set of results.
mkListPartnerEventSourceAccounts ::
  -- | 'eventSourceName'
  Lude.Text ->
  ListPartnerEventSourceAccounts
mkListPartnerEventSourceAccounts pEventSourceName_ =
  ListPartnerEventSourceAccounts'
    { nextToken = Lude.Nothing,
      limit = Lude.Nothing,
      eventSourceName = pEventSourceName_
    }

-- | The token returned by a previous call to this operation. Specifying this retrieves the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpesaNextToken :: Lens.Lens' ListPartnerEventSourceAccounts (Lude.Maybe Lude.Text)
lpesaNextToken = Lens.lens (nextToken :: ListPartnerEventSourceAccounts -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListPartnerEventSourceAccounts)
{-# DEPRECATED lpesaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Specifying this limits the number of results returned by this operation. The operation also returns a NextToken which you can use in a subsequent operation to retrieve the next set of results.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpesaLimit :: Lens.Lens' ListPartnerEventSourceAccounts (Lude.Maybe Lude.Natural)
lpesaLimit = Lens.lens (limit :: ListPartnerEventSourceAccounts -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListPartnerEventSourceAccounts)
{-# DEPRECATED lpesaLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The name of the partner event source to display account information about.
--
-- /Note:/ Consider using 'eventSourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpesaEventSourceName :: Lens.Lens' ListPartnerEventSourceAccounts Lude.Text
lpesaEventSourceName = Lens.lens (eventSourceName :: ListPartnerEventSourceAccounts -> Lude.Text) (\s a -> s {eventSourceName = a} :: ListPartnerEventSourceAccounts)
{-# DEPRECATED lpesaEventSourceName "Use generic-lens or generic-optics with 'eventSourceName' instead." #-}

instance Lude.AWSRequest ListPartnerEventSourceAccounts where
  type
    Rs ListPartnerEventSourceAccounts =
      ListPartnerEventSourceAccountsResponse
  request = Req.postJSON cloudWatchEventsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListPartnerEventSourceAccountsResponse'
            Lude.<$> (x Lude..?> "PartnerEventSourceAccounts" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListPartnerEventSourceAccounts where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSEvents.ListPartnerEventSourceAccounts" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListPartnerEventSourceAccounts where
  toJSON ListPartnerEventSourceAccounts' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Limit" Lude..=) Lude.<$> limit,
            Lude.Just ("EventSourceName" Lude..= eventSourceName)
          ]
      )

instance Lude.ToPath ListPartnerEventSourceAccounts where
  toPath = Lude.const "/"

instance Lude.ToQuery ListPartnerEventSourceAccounts where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListPartnerEventSourceAccountsResponse' smart constructor.
data ListPartnerEventSourceAccountsResponse = ListPartnerEventSourceAccountsResponse'
  { partnerEventSourceAccounts ::
      Lude.Maybe
        [PartnerEventSourceAccount],
    nextToken ::
      Lude.Maybe
        Lude.Text,
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

-- | Creates a value of 'ListPartnerEventSourceAccountsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - A token you can use in a subsequent operation to retrieve the next set of results.
-- * 'partnerEventSourceAccounts' - The list of partner event sources returned by the operation.
-- * 'responseStatus' - The response status code.
mkListPartnerEventSourceAccountsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListPartnerEventSourceAccountsResponse
mkListPartnerEventSourceAccountsResponse pResponseStatus_ =
  ListPartnerEventSourceAccountsResponse'
    { partnerEventSourceAccounts =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of partner event sources returned by the operation.
--
-- /Note:/ Consider using 'partnerEventSourceAccounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpesarsPartnerEventSourceAccounts :: Lens.Lens' ListPartnerEventSourceAccountsResponse (Lude.Maybe [PartnerEventSourceAccount])
lpesarsPartnerEventSourceAccounts = Lens.lens (partnerEventSourceAccounts :: ListPartnerEventSourceAccountsResponse -> Lude.Maybe [PartnerEventSourceAccount]) (\s a -> s {partnerEventSourceAccounts = a} :: ListPartnerEventSourceAccountsResponse)
{-# DEPRECATED lpesarsPartnerEventSourceAccounts "Use generic-lens or generic-optics with 'partnerEventSourceAccounts' instead." #-}

-- | A token you can use in a subsequent operation to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpesarsNextToken :: Lens.Lens' ListPartnerEventSourceAccountsResponse (Lude.Maybe Lude.Text)
lpesarsNextToken = Lens.lens (nextToken :: ListPartnerEventSourceAccountsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListPartnerEventSourceAccountsResponse)
{-# DEPRECATED lpesarsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpesarsResponseStatus :: Lens.Lens' ListPartnerEventSourceAccountsResponse Lude.Int
lpesarsResponseStatus = Lens.lens (responseStatus :: ListPartnerEventSourceAccountsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListPartnerEventSourceAccountsResponse)
{-# DEPRECATED lpesarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
