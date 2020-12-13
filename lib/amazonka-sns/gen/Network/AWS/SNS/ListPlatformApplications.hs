{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.ListPlatformApplications
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the platform application objects for the supported push notification services, such as APNS and GCM (Firebase Cloud Messaging). The results for @ListPlatformApplications@ are paginated and return a limited list of applications, up to 100. If additional records are available after the first page results, then a NextToken string will be returned. To receive the next page, you call @ListPlatformApplications@ using the NextToken string received from the previous call. When there are no more records to return, @NextToken@ will be null. For more information, see <https://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html Using Amazon SNS Mobile Push Notifications> .
--
-- This action is throttled at 15 transactions per second (TPS).
--
-- This operation returns paginated results.
module Network.AWS.SNS.ListPlatformApplications
  ( -- * Creating a request
    ListPlatformApplications (..),
    mkListPlatformApplications,

    -- ** Request lenses
    lpaNextToken,

    -- * Destructuring the response
    ListPlatformApplicationsResponse (..),
    mkListPlatformApplicationsResponse,

    -- ** Response lenses
    lparsPlatformApplications,
    lparsNextToken,
    lparsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SNS.Types

-- | Input for ListPlatformApplications action.
--
-- /See:/ 'mkListPlatformApplications' smart constructor.
newtype ListPlatformApplications = ListPlatformApplications'
  { -- | NextToken string is used when calling ListPlatformApplications action to retrieve additional records that are available after the first page results.
    nextToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPlatformApplications' with the minimum fields required to make a request.
--
-- * 'nextToken' - NextToken string is used when calling ListPlatformApplications action to retrieve additional records that are available after the first page results.
mkListPlatformApplications ::
  ListPlatformApplications
mkListPlatformApplications =
  ListPlatformApplications' {nextToken = Lude.Nothing}

-- | NextToken string is used when calling ListPlatformApplications action to retrieve additional records that are available after the first page results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpaNextToken :: Lens.Lens' ListPlatformApplications (Lude.Maybe Lude.Text)
lpaNextToken = Lens.lens (nextToken :: ListPlatformApplications -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListPlatformApplications)
{-# DEPRECATED lpaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Page.AWSPager ListPlatformApplications where
  page rq rs
    | Page.stop (rs Lens.^. lparsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lparsPlatformApplications) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lpaNextToken Lens..~ rs Lens.^. lparsNextToken

instance Lude.AWSRequest ListPlatformApplications where
  type Rs ListPlatformApplications = ListPlatformApplicationsResponse
  request = Req.postQuery snsService
  response =
    Res.receiveXMLWrapper
      "ListPlatformApplicationsResult"
      ( \s h x ->
          ListPlatformApplicationsResponse'
            Lude.<$> ( x Lude..@? "PlatformApplications" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListPlatformApplications where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListPlatformApplications where
  toPath = Lude.const "/"

instance Lude.ToQuery ListPlatformApplications where
  toQuery ListPlatformApplications' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ListPlatformApplications" :: Lude.ByteString),
        "Version" Lude.=: ("2010-03-31" :: Lude.ByteString),
        "NextToken" Lude.=: nextToken
      ]

-- | Response for ListPlatformApplications action.
--
-- /See:/ 'mkListPlatformApplicationsResponse' smart constructor.
data ListPlatformApplicationsResponse = ListPlatformApplicationsResponse'
  { -- | Platform applications returned when calling ListPlatformApplications action.
    platformApplications :: Lude.Maybe [PlatformApplication],
    -- | NextToken string is returned when calling ListPlatformApplications action if additional records are available after the first page results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPlatformApplicationsResponse' with the minimum fields required to make a request.
--
-- * 'platformApplications' - Platform applications returned when calling ListPlatformApplications action.
-- * 'nextToken' - NextToken string is returned when calling ListPlatformApplications action if additional records are available after the first page results.
-- * 'responseStatus' - The response status code.
mkListPlatformApplicationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListPlatformApplicationsResponse
mkListPlatformApplicationsResponse pResponseStatus_ =
  ListPlatformApplicationsResponse'
    { platformApplications =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Platform applications returned when calling ListPlatformApplications action.
--
-- /Note:/ Consider using 'platformApplications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lparsPlatformApplications :: Lens.Lens' ListPlatformApplicationsResponse (Lude.Maybe [PlatformApplication])
lparsPlatformApplications = Lens.lens (platformApplications :: ListPlatformApplicationsResponse -> Lude.Maybe [PlatformApplication]) (\s a -> s {platformApplications = a} :: ListPlatformApplicationsResponse)
{-# DEPRECATED lparsPlatformApplications "Use generic-lens or generic-optics with 'platformApplications' instead." #-}

-- | NextToken string is returned when calling ListPlatformApplications action if additional records are available after the first page results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lparsNextToken :: Lens.Lens' ListPlatformApplicationsResponse (Lude.Maybe Lude.Text)
lparsNextToken = Lens.lens (nextToken :: ListPlatformApplicationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListPlatformApplicationsResponse)
{-# DEPRECATED lparsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lparsResponseStatus :: Lens.Lens' ListPlatformApplicationsResponse Lude.Int
lparsResponseStatus = Lens.lens (responseStatus :: ListPlatformApplicationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListPlatformApplicationsResponse)
{-# DEPRECATED lparsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
