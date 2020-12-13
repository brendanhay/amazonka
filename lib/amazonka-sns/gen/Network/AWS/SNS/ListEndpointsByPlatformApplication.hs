{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.ListEndpointsByPlatformApplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the endpoints and endpoint attributes for devices in a supported push notification service, such as GCM (Firebase Cloud Messaging) and APNS. The results for @ListEndpointsByPlatformApplication@ are paginated and return a limited list of endpoints, up to 100. If additional records are available after the first page results, then a NextToken string will be returned. To receive the next page, you call @ListEndpointsByPlatformApplication@ again using the NextToken string received from the previous call. When there are no more records to return, NextToken will be null. For more information, see <https://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html Using Amazon SNS Mobile Push Notifications> .
--
-- This action is throttled at 30 transactions per second (TPS).
--
-- This operation returns paginated results.
module Network.AWS.SNS.ListEndpointsByPlatformApplication
  ( -- * Creating a request
    ListEndpointsByPlatformApplication (..),
    mkListEndpointsByPlatformApplication,

    -- ** Request lenses
    lebpaPlatformApplicationARN,
    lebpaNextToken,

    -- * Destructuring the response
    ListEndpointsByPlatformApplicationResponse (..),
    mkListEndpointsByPlatformApplicationResponse,

    -- ** Response lenses
    lebparsNextToken,
    lebparsEndpoints,
    lebparsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SNS.Types

-- | Input for ListEndpointsByPlatformApplication action.
--
-- /See:/ 'mkListEndpointsByPlatformApplication' smart constructor.
data ListEndpointsByPlatformApplication = ListEndpointsByPlatformApplication'
  { -- | PlatformApplicationArn for ListEndpointsByPlatformApplicationInput action.
    platformApplicationARN :: Lude.Text,
    -- | NextToken string is used when calling ListEndpointsByPlatformApplication action to retrieve additional records that are available after the first page results.
    nextToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListEndpointsByPlatformApplication' with the minimum fields required to make a request.
--
-- * 'platformApplicationARN' - PlatformApplicationArn for ListEndpointsByPlatformApplicationInput action.
-- * 'nextToken' - NextToken string is used when calling ListEndpointsByPlatformApplication action to retrieve additional records that are available after the first page results.
mkListEndpointsByPlatformApplication ::
  -- | 'platformApplicationARN'
  Lude.Text ->
  ListEndpointsByPlatformApplication
mkListEndpointsByPlatformApplication pPlatformApplicationARN_ =
  ListEndpointsByPlatformApplication'
    { platformApplicationARN =
        pPlatformApplicationARN_,
      nextToken = Lude.Nothing
    }

-- | PlatformApplicationArn for ListEndpointsByPlatformApplicationInput action.
--
-- /Note:/ Consider using 'platformApplicationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lebpaPlatformApplicationARN :: Lens.Lens' ListEndpointsByPlatformApplication Lude.Text
lebpaPlatformApplicationARN = Lens.lens (platformApplicationARN :: ListEndpointsByPlatformApplication -> Lude.Text) (\s a -> s {platformApplicationARN = a} :: ListEndpointsByPlatformApplication)
{-# DEPRECATED lebpaPlatformApplicationARN "Use generic-lens or generic-optics with 'platformApplicationARN' instead." #-}

-- | NextToken string is used when calling ListEndpointsByPlatformApplication action to retrieve additional records that are available after the first page results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lebpaNextToken :: Lens.Lens' ListEndpointsByPlatformApplication (Lude.Maybe Lude.Text)
lebpaNextToken = Lens.lens (nextToken :: ListEndpointsByPlatformApplication -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListEndpointsByPlatformApplication)
{-# DEPRECATED lebpaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Page.AWSPager ListEndpointsByPlatformApplication where
  page rq rs
    | Page.stop (rs Lens.^. lebparsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lebparsEndpoints) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lebpaNextToken Lens..~ rs Lens.^. lebparsNextToken

instance Lude.AWSRequest ListEndpointsByPlatformApplication where
  type
    Rs ListEndpointsByPlatformApplication =
      ListEndpointsByPlatformApplicationResponse
  request = Req.postQuery snsService
  response =
    Res.receiveXMLWrapper
      "ListEndpointsByPlatformApplicationResult"
      ( \s h x ->
          ListEndpointsByPlatformApplicationResponse'
            Lude.<$> (x Lude..@? "NextToken")
            Lude.<*> ( x Lude..@? "Endpoints" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListEndpointsByPlatformApplication where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListEndpointsByPlatformApplication where
  toPath = Lude.const "/"

instance Lude.ToQuery ListEndpointsByPlatformApplication where
  toQuery ListEndpointsByPlatformApplication' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("ListEndpointsByPlatformApplication" :: Lude.ByteString),
        "Version" Lude.=: ("2010-03-31" :: Lude.ByteString),
        "PlatformApplicationArn" Lude.=: platformApplicationARN,
        "NextToken" Lude.=: nextToken
      ]

-- | Response for ListEndpointsByPlatformApplication action.
--
-- /See:/ 'mkListEndpointsByPlatformApplicationResponse' smart constructor.
data ListEndpointsByPlatformApplicationResponse = ListEndpointsByPlatformApplicationResponse'
  { -- | NextToken string is returned when calling ListEndpointsByPlatformApplication action if additional records are available after the first page results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Endpoints returned for ListEndpointsByPlatformApplication action.
    endpoints :: Lude.Maybe [Endpoint],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListEndpointsByPlatformApplicationResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - NextToken string is returned when calling ListEndpointsByPlatformApplication action if additional records are available after the first page results.
-- * 'endpoints' - Endpoints returned for ListEndpointsByPlatformApplication action.
-- * 'responseStatus' - The response status code.
mkListEndpointsByPlatformApplicationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListEndpointsByPlatformApplicationResponse
mkListEndpointsByPlatformApplicationResponse pResponseStatus_ =
  ListEndpointsByPlatformApplicationResponse'
    { nextToken =
        Lude.Nothing,
      endpoints = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | NextToken string is returned when calling ListEndpointsByPlatformApplication action if additional records are available after the first page results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lebparsNextToken :: Lens.Lens' ListEndpointsByPlatformApplicationResponse (Lude.Maybe Lude.Text)
lebparsNextToken = Lens.lens (nextToken :: ListEndpointsByPlatformApplicationResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListEndpointsByPlatformApplicationResponse)
{-# DEPRECATED lebparsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Endpoints returned for ListEndpointsByPlatformApplication action.
--
-- /Note:/ Consider using 'endpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lebparsEndpoints :: Lens.Lens' ListEndpointsByPlatformApplicationResponse (Lude.Maybe [Endpoint])
lebparsEndpoints = Lens.lens (endpoints :: ListEndpointsByPlatformApplicationResponse -> Lude.Maybe [Endpoint]) (\s a -> s {endpoints = a} :: ListEndpointsByPlatformApplicationResponse)
{-# DEPRECATED lebparsEndpoints "Use generic-lens or generic-optics with 'endpoints' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lebparsResponseStatus :: Lens.Lens' ListEndpointsByPlatformApplicationResponse Lude.Int
lebparsResponseStatus = Lens.lens (responseStatus :: ListEndpointsByPlatformApplicationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListEndpointsByPlatformApplicationResponse)
{-# DEPRECATED lebparsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
