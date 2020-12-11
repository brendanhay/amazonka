{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.ListPartnerEventSources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- An SaaS partner can use this operation to list all the partner event source names that they have created. This operation is not used by AWS customers.
module Network.AWS.CloudWatchEvents.ListPartnerEventSources
  ( -- * Creating a request
    ListPartnerEventSources (..),
    mkListPartnerEventSources,

    -- ** Request lenses
    lpesNextToken,
    lpesLimit,
    lpesNamePrefix,

    -- * Destructuring the response
    ListPartnerEventSourcesResponse (..),
    mkListPartnerEventSourcesResponse,

    -- ** Response lenses
    lpesrsPartnerEventSources,
    lpesrsNextToken,
    lpesrsResponseStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListPartnerEventSources' smart constructor.
data ListPartnerEventSources = ListPartnerEventSources'
  { nextToken ::
      Lude.Maybe Lude.Text,
    limit :: Lude.Maybe Lude.Natural,
    namePrefix :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPartnerEventSources' with the minimum fields required to make a request.
--
-- * 'limit' - pecifying this limits the number of results returned by this operation. The operation also returns a NextToken which you can use in a subsequent operation to retrieve the next set of results.
-- * 'namePrefix' - If you specify this, the results are limited to only those partner event sources that start with the string you specify.
-- * 'nextToken' - The token returned by a previous call to this operation. Specifying this retrieves the next set of results.
mkListPartnerEventSources ::
  -- | 'namePrefix'
  Lude.Text ->
  ListPartnerEventSources
mkListPartnerEventSources pNamePrefix_ =
  ListPartnerEventSources'
    { nextToken = Lude.Nothing,
      limit = Lude.Nothing,
      namePrefix = pNamePrefix_
    }

-- | The token returned by a previous call to this operation. Specifying this retrieves the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpesNextToken :: Lens.Lens' ListPartnerEventSources (Lude.Maybe Lude.Text)
lpesNextToken = Lens.lens (nextToken :: ListPartnerEventSources -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListPartnerEventSources)
{-# DEPRECATED lpesNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | pecifying this limits the number of results returned by this operation. The operation also returns a NextToken which you can use in a subsequent operation to retrieve the next set of results.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpesLimit :: Lens.Lens' ListPartnerEventSources (Lude.Maybe Lude.Natural)
lpesLimit = Lens.lens (limit :: ListPartnerEventSources -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListPartnerEventSources)
{-# DEPRECATED lpesLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | If you specify this, the results are limited to only those partner event sources that start with the string you specify.
--
-- /Note:/ Consider using 'namePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpesNamePrefix :: Lens.Lens' ListPartnerEventSources Lude.Text
lpesNamePrefix = Lens.lens (namePrefix :: ListPartnerEventSources -> Lude.Text) (\s a -> s {namePrefix = a} :: ListPartnerEventSources)
{-# DEPRECATED lpesNamePrefix "Use generic-lens or generic-optics with 'namePrefix' instead." #-}

instance Lude.AWSRequest ListPartnerEventSources where
  type Rs ListPartnerEventSources = ListPartnerEventSourcesResponse
  request = Req.postJSON cloudWatchEventsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListPartnerEventSourcesResponse'
            Lude.<$> (x Lude..?> "PartnerEventSources" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListPartnerEventSources where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSEvents.ListPartnerEventSources" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListPartnerEventSources where
  toJSON ListPartnerEventSources' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Limit" Lude..=) Lude.<$> limit,
            Lude.Just ("NamePrefix" Lude..= namePrefix)
          ]
      )

instance Lude.ToPath ListPartnerEventSources where
  toPath = Lude.const "/"

instance Lude.ToQuery ListPartnerEventSources where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListPartnerEventSourcesResponse' smart constructor.
data ListPartnerEventSourcesResponse = ListPartnerEventSourcesResponse'
  { partnerEventSources ::
      Lude.Maybe
        [PartnerEventSource],
    nextToken ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListPartnerEventSourcesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - A token you can use in a subsequent operation to retrieve the next set of results.
-- * 'partnerEventSources' - The list of partner event sources returned by the operation.
-- * 'responseStatus' - The response status code.
mkListPartnerEventSourcesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListPartnerEventSourcesResponse
mkListPartnerEventSourcesResponse pResponseStatus_ =
  ListPartnerEventSourcesResponse'
    { partnerEventSources =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of partner event sources returned by the operation.
--
-- /Note:/ Consider using 'partnerEventSources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpesrsPartnerEventSources :: Lens.Lens' ListPartnerEventSourcesResponse (Lude.Maybe [PartnerEventSource])
lpesrsPartnerEventSources = Lens.lens (partnerEventSources :: ListPartnerEventSourcesResponse -> Lude.Maybe [PartnerEventSource]) (\s a -> s {partnerEventSources = a} :: ListPartnerEventSourcesResponse)
{-# DEPRECATED lpesrsPartnerEventSources "Use generic-lens or generic-optics with 'partnerEventSources' instead." #-}

-- | A token you can use in a subsequent operation to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpesrsNextToken :: Lens.Lens' ListPartnerEventSourcesResponse (Lude.Maybe Lude.Text)
lpesrsNextToken = Lens.lens (nextToken :: ListPartnerEventSourcesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListPartnerEventSourcesResponse)
{-# DEPRECATED lpesrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpesrsResponseStatus :: Lens.Lens' ListPartnerEventSourcesResponse Lude.Int
lpesrsResponseStatus = Lens.lens (responseStatus :: ListPartnerEventSourcesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListPartnerEventSourcesResponse)
{-# DEPRECATED lpesrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
