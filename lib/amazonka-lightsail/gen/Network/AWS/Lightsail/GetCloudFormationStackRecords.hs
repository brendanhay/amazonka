{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetCloudFormationStackRecords
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the CloudFormation stack record created as a result of the @create cloud formation stack@ operation.
--
-- An AWS CloudFormation stack is used to create a new Amazon EC2 instance from an exported Lightsail snapshot.
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetCloudFormationStackRecords
  ( -- * Creating a request
    GetCloudFormationStackRecords (..),
    mkGetCloudFormationStackRecords,

    -- ** Request lenses
    gcfsrPageToken,

    -- * Destructuring the response
    GetCloudFormationStackRecordsResponse (..),
    mkGetCloudFormationStackRecordsResponse,

    -- ** Response lenses
    gcfsrrsNextPageToken,
    gcfsrrsCloudFormationStackRecords,
    gcfsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetCloudFormationStackRecords' smart constructor.
newtype GetCloudFormationStackRecords = GetCloudFormationStackRecords'
  { pageToken ::
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

-- | Creates a value of 'GetCloudFormationStackRecords' with the minimum fields required to make a request.
--
-- * 'pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetClouFormationStackRecords@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
mkGetCloudFormationStackRecords ::
  GetCloudFormationStackRecords
mkGetCloudFormationStackRecords =
  GetCloudFormationStackRecords' {pageToken = Lude.Nothing}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetClouFormationStackRecords@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfsrPageToken :: Lens.Lens' GetCloudFormationStackRecords (Lude.Maybe Lude.Text)
gcfsrPageToken = Lens.lens (pageToken :: GetCloudFormationStackRecords -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: GetCloudFormationStackRecords)
{-# DEPRECATED gcfsrPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

instance Page.AWSPager GetCloudFormationStackRecords where
  page rq rs
    | Page.stop (rs Lens.^. gcfsrrsNextPageToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gcfsrrsCloudFormationStackRecords) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gcfsrPageToken Lens..~ rs Lens.^. gcfsrrsNextPageToken

instance Lude.AWSRequest GetCloudFormationStackRecords where
  type
    Rs GetCloudFormationStackRecords =
      GetCloudFormationStackRecordsResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetCloudFormationStackRecordsResponse'
            Lude.<$> (x Lude..?> "nextPageToken")
            Lude.<*> (x Lude..?> "cloudFormationStackRecords" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetCloudFormationStackRecords where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Lightsail_20161128.GetCloudFormationStackRecords" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetCloudFormationStackRecords where
  toJSON GetCloudFormationStackRecords' {..} =
    Lude.object
      (Lude.catMaybes [("pageToken" Lude..=) Lude.<$> pageToken])

instance Lude.ToPath GetCloudFormationStackRecords where
  toPath = Lude.const "/"

instance Lude.ToQuery GetCloudFormationStackRecords where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetCloudFormationStackRecordsResponse' smart constructor.
data GetCloudFormationStackRecordsResponse = GetCloudFormationStackRecordsResponse'
  { nextPageToken ::
      Lude.Maybe
        Lude.Text,
    cloudFormationStackRecords ::
      Lude.Maybe
        [CloudFormationStackRecord],
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

-- | Creates a value of 'GetCloudFormationStackRecordsResponse' with the minimum fields required to make a request.
--
-- * 'cloudFormationStackRecords' - A list of objects describing the CloudFormation stack records.
-- * 'nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetCloudFormationStackRecords@ request and specify the next page token using the @pageToken@ parameter.
-- * 'responseStatus' - The response status code.
mkGetCloudFormationStackRecordsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetCloudFormationStackRecordsResponse
mkGetCloudFormationStackRecordsResponse pResponseStatus_ =
  GetCloudFormationStackRecordsResponse'
    { nextPageToken =
        Lude.Nothing,
      cloudFormationStackRecords = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetCloudFormationStackRecords@ request and specify the next page token using the @pageToken@ parameter.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfsrrsNextPageToken :: Lens.Lens' GetCloudFormationStackRecordsResponse (Lude.Maybe Lude.Text)
gcfsrrsNextPageToken = Lens.lens (nextPageToken :: GetCloudFormationStackRecordsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: GetCloudFormationStackRecordsResponse)
{-# DEPRECATED gcfsrrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | A list of objects describing the CloudFormation stack records.
--
-- /Note:/ Consider using 'cloudFormationStackRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfsrrsCloudFormationStackRecords :: Lens.Lens' GetCloudFormationStackRecordsResponse (Lude.Maybe [CloudFormationStackRecord])
gcfsrrsCloudFormationStackRecords = Lens.lens (cloudFormationStackRecords :: GetCloudFormationStackRecordsResponse -> Lude.Maybe [CloudFormationStackRecord]) (\s a -> s {cloudFormationStackRecords = a} :: GetCloudFormationStackRecordsResponse)
{-# DEPRECATED gcfsrrsCloudFormationStackRecords "Use generic-lens or generic-optics with 'cloudFormationStackRecords' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfsrrsResponseStatus :: Lens.Lens' GetCloudFormationStackRecordsResponse Lude.Int
gcfsrrsResponseStatus = Lens.lens (responseStatus :: GetCloudFormationStackRecordsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetCloudFormationStackRecordsResponse)
{-# DEPRECATED gcfsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
