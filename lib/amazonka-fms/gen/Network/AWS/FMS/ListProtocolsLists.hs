{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.ListProtocolsLists
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of @ProtocolsListDataSummary@ objects.
module Network.AWS.FMS.ListProtocolsLists
  ( -- * Creating a request
    ListProtocolsLists (..),
    mkListProtocolsLists,

    -- ** Request lenses
    lplDefaultLists,
    lplNextToken,
    lplMaxResults,

    -- * Destructuring the response
    ListProtocolsListsResponse (..),
    mkListProtocolsListsResponse,

    -- ** Response lenses
    lplrsProtocolsLists,
    lplrsNextToken,
    lplrsResponseStatus,
  )
where

import Network.AWS.FMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListProtocolsLists' smart constructor.
data ListProtocolsLists = ListProtocolsLists'
  { -- | Specifies whether the lists to retrieve are default lists owned by AWS Firewall Manager.
    defaultLists :: Lude.Maybe Lude.Bool,
    -- | If you specify a value for @MaxResults@ in your list request, and you have more objects than the maximum, AWS Firewall Manager returns this token in the response. For all but the first request, you provide the token returned by the prior request in the request parameters, to retrieve the next batch of objects.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of objects that you want AWS Firewall Manager to return for this request. If more objects are available, in the response, AWS Firewall Manager provides a @NextToken@ value that you can use in a subsequent call to get the next batch of objects.
    --
    -- If you don't specify this, AWS Firewall Manager returns all available objects.
    maxResults :: Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListProtocolsLists' with the minimum fields required to make a request.
--
-- * 'defaultLists' - Specifies whether the lists to retrieve are default lists owned by AWS Firewall Manager.
-- * 'nextToken' - If you specify a value for @MaxResults@ in your list request, and you have more objects than the maximum, AWS Firewall Manager returns this token in the response. For all but the first request, you provide the token returned by the prior request in the request parameters, to retrieve the next batch of objects.
-- * 'maxResults' - The maximum number of objects that you want AWS Firewall Manager to return for this request. If more objects are available, in the response, AWS Firewall Manager provides a @NextToken@ value that you can use in a subsequent call to get the next batch of objects.
--
-- If you don't specify this, AWS Firewall Manager returns all available objects.
mkListProtocolsLists ::
  -- | 'maxResults'
  Lude.Natural ->
  ListProtocolsLists
mkListProtocolsLists pMaxResults_ =
  ListProtocolsLists'
    { defaultLists = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = pMaxResults_
    }

-- | Specifies whether the lists to retrieve are default lists owned by AWS Firewall Manager.
--
-- /Note:/ Consider using 'defaultLists' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lplDefaultLists :: Lens.Lens' ListProtocolsLists (Lude.Maybe Lude.Bool)
lplDefaultLists = Lens.lens (defaultLists :: ListProtocolsLists -> Lude.Maybe Lude.Bool) (\s a -> s {defaultLists = a} :: ListProtocolsLists)
{-# DEPRECATED lplDefaultLists "Use generic-lens or generic-optics with 'defaultLists' instead." #-}

-- | If you specify a value for @MaxResults@ in your list request, and you have more objects than the maximum, AWS Firewall Manager returns this token in the response. For all but the first request, you provide the token returned by the prior request in the request parameters, to retrieve the next batch of objects.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lplNextToken :: Lens.Lens' ListProtocolsLists (Lude.Maybe Lude.Text)
lplNextToken = Lens.lens (nextToken :: ListProtocolsLists -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListProtocolsLists)
{-# DEPRECATED lplNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of objects that you want AWS Firewall Manager to return for this request. If more objects are available, in the response, AWS Firewall Manager provides a @NextToken@ value that you can use in a subsequent call to get the next batch of objects.
--
-- If you don't specify this, AWS Firewall Manager returns all available objects.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lplMaxResults :: Lens.Lens' ListProtocolsLists Lude.Natural
lplMaxResults = Lens.lens (maxResults :: ListProtocolsLists -> Lude.Natural) (\s a -> s {maxResults = a} :: ListProtocolsLists)
{-# DEPRECATED lplMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest ListProtocolsLists where
  type Rs ListProtocolsLists = ListProtocolsListsResponse
  request = Req.postJSON fmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListProtocolsListsResponse'
            Lude.<$> (x Lude..?> "ProtocolsLists" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListProtocolsLists where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSFMS_20180101.ListProtocolsLists" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListProtocolsLists where
  toJSON ListProtocolsLists' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DefaultLists" Lude..=) Lude.<$> defaultLists,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("MaxResults" Lude..= maxResults)
          ]
      )

instance Lude.ToPath ListProtocolsLists where
  toPath = Lude.const "/"

instance Lude.ToQuery ListProtocolsLists where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListProtocolsListsResponse' smart constructor.
data ListProtocolsListsResponse = ListProtocolsListsResponse'
  { -- | An array of @ProtocolsListDataSummary@ objects.
    protocolsLists :: Lude.Maybe [ProtocolsListDataSummary],
    -- | If you specify a value for @MaxResults@ in your list request, and you have more objects than the maximum, AWS Firewall Manager returns this token in the response. You can use this token in subsequent requests to retrieve the next batch of objects.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListProtocolsListsResponse' with the minimum fields required to make a request.
--
-- * 'protocolsLists' - An array of @ProtocolsListDataSummary@ objects.
-- * 'nextToken' - If you specify a value for @MaxResults@ in your list request, and you have more objects than the maximum, AWS Firewall Manager returns this token in the response. You can use this token in subsequent requests to retrieve the next batch of objects.
-- * 'responseStatus' - The response status code.
mkListProtocolsListsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListProtocolsListsResponse
mkListProtocolsListsResponse pResponseStatus_ =
  ListProtocolsListsResponse'
    { protocolsLists = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of @ProtocolsListDataSummary@ objects.
--
-- /Note:/ Consider using 'protocolsLists' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lplrsProtocolsLists :: Lens.Lens' ListProtocolsListsResponse (Lude.Maybe [ProtocolsListDataSummary])
lplrsProtocolsLists = Lens.lens (protocolsLists :: ListProtocolsListsResponse -> Lude.Maybe [ProtocolsListDataSummary]) (\s a -> s {protocolsLists = a} :: ListProtocolsListsResponse)
{-# DEPRECATED lplrsProtocolsLists "Use generic-lens or generic-optics with 'protocolsLists' instead." #-}

-- | If you specify a value for @MaxResults@ in your list request, and you have more objects than the maximum, AWS Firewall Manager returns this token in the response. You can use this token in subsequent requests to retrieve the next batch of objects.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lplrsNextToken :: Lens.Lens' ListProtocolsListsResponse (Lude.Maybe Lude.Text)
lplrsNextToken = Lens.lens (nextToken :: ListProtocolsListsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListProtocolsListsResponse)
{-# DEPRECATED lplrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lplrsResponseStatus :: Lens.Lens' ListProtocolsListsResponse Lude.Int
lplrsResponseStatus = Lens.lens (responseStatus :: ListProtocolsListsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListProtocolsListsResponse)
{-# DEPRECATED lplrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
