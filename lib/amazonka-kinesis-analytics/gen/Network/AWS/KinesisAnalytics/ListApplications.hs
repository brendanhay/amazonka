{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.ListApplications
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of Amazon Kinesis Analytics applications in your account. For each application, the response includes the application name, Amazon Resource Name (ARN), and status. If the response returns the @HasMoreApplications@ value as true, you can send another request by adding the @ExclusiveStartApplicationName@ in the request body, and set the value of this to the last application name from the previous response.
--
-- If you want detailed information about a specific application, use <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> .
-- This operation requires permissions to perform the @kinesisanalytics:ListApplications@ action.
module Network.AWS.KinesisAnalytics.ListApplications
  ( -- * Creating a request
    ListApplications (..),
    mkListApplications,

    -- ** Request lenses
    laLimit,
    laExclusiveStartApplicationName,

    -- * Destructuring the response
    ListApplicationsResponse (..),
    mkListApplicationsResponse,

    -- ** Response lenses
    larsApplicationSummaries,
    larsHasMoreApplications,
    larsResponseStatus,
  )
where

import Network.AWS.KinesisAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkListApplications' smart constructor.
data ListApplications = ListApplications'
  { -- | Maximum number of applications to list.
    limit :: Lude.Maybe Lude.Natural,
    -- | Name of the application to start the list with. When using pagination to retrieve the list, you don't need to specify this parameter in the first request. However, in subsequent requests, you add the last application name from the previous response to get the next page of applications.
    exclusiveStartApplicationName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListApplications' with the minimum fields required to make a request.
--
-- * 'limit' - Maximum number of applications to list.
-- * 'exclusiveStartApplicationName' - Name of the application to start the list with. When using pagination to retrieve the list, you don't need to specify this parameter in the first request. However, in subsequent requests, you add the last application name from the previous response to get the next page of applications.
mkListApplications ::
  ListApplications
mkListApplications =
  ListApplications'
    { limit = Lude.Nothing,
      exclusiveStartApplicationName = Lude.Nothing
    }

-- | Maximum number of applications to list.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laLimit :: Lens.Lens' ListApplications (Lude.Maybe Lude.Natural)
laLimit = Lens.lens (limit :: ListApplications -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListApplications)
{-# DEPRECATED laLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | Name of the application to start the list with. When using pagination to retrieve the list, you don't need to specify this parameter in the first request. However, in subsequent requests, you add the last application name from the previous response to get the next page of applications.
--
-- /Note:/ Consider using 'exclusiveStartApplicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laExclusiveStartApplicationName :: Lens.Lens' ListApplications (Lude.Maybe Lude.Text)
laExclusiveStartApplicationName = Lens.lens (exclusiveStartApplicationName :: ListApplications -> Lude.Maybe Lude.Text) (\s a -> s {exclusiveStartApplicationName = a} :: ListApplications)
{-# DEPRECATED laExclusiveStartApplicationName "Use generic-lens or generic-optics with 'exclusiveStartApplicationName' instead." #-}

instance Lude.AWSRequest ListApplications where
  type Rs ListApplications = ListApplicationsResponse
  request = Req.postJSON kinesisAnalyticsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListApplicationsResponse'
            Lude.<$> (x Lude..?> "ApplicationSummaries" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..:> "HasMoreApplications")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListApplications where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("KinesisAnalytics_20150814.ListApplications" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListApplications where
  toJSON ListApplications' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Limit" Lude..=) Lude.<$> limit,
            ("ExclusiveStartApplicationName" Lude..=)
              Lude.<$> exclusiveStartApplicationName
          ]
      )

instance Lude.ToPath ListApplications where
  toPath = Lude.const "/"

instance Lude.ToQuery ListApplications where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkListApplicationsResponse' smart constructor.
data ListApplicationsResponse = ListApplicationsResponse'
  { -- | List of @ApplicationSummary@ objects.
    applicationSummaries :: [ApplicationSummary],
    -- | Returns true if there are more applications to retrieve.
    hasMoreApplications :: Lude.Bool,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListApplicationsResponse' with the minimum fields required to make a request.
--
-- * 'applicationSummaries' - List of @ApplicationSummary@ objects.
-- * 'hasMoreApplications' - Returns true if there are more applications to retrieve.
-- * 'responseStatus' - The response status code.
mkListApplicationsResponse ::
  -- | 'hasMoreApplications'
  Lude.Bool ->
  -- | 'responseStatus'
  Lude.Int ->
  ListApplicationsResponse
mkListApplicationsResponse pHasMoreApplications_ pResponseStatus_ =
  ListApplicationsResponse'
    { applicationSummaries = Lude.mempty,
      hasMoreApplications = pHasMoreApplications_,
      responseStatus = pResponseStatus_
    }

-- | List of @ApplicationSummary@ objects.
--
-- /Note:/ Consider using 'applicationSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larsApplicationSummaries :: Lens.Lens' ListApplicationsResponse [ApplicationSummary]
larsApplicationSummaries = Lens.lens (applicationSummaries :: ListApplicationsResponse -> [ApplicationSummary]) (\s a -> s {applicationSummaries = a} :: ListApplicationsResponse)
{-# DEPRECATED larsApplicationSummaries "Use generic-lens or generic-optics with 'applicationSummaries' instead." #-}

-- | Returns true if there are more applications to retrieve.
--
-- /Note:/ Consider using 'hasMoreApplications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larsHasMoreApplications :: Lens.Lens' ListApplicationsResponse Lude.Bool
larsHasMoreApplications = Lens.lens (hasMoreApplications :: ListApplicationsResponse -> Lude.Bool) (\s a -> s {hasMoreApplications = a} :: ListApplicationsResponse)
{-# DEPRECATED larsHasMoreApplications "Use generic-lens or generic-optics with 'hasMoreApplications' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larsResponseStatus :: Lens.Lens' ListApplicationsResponse Lude.Int
larsResponseStatus = Lens.lens (responseStatus :: ListApplicationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListApplicationsResponse)
{-# DEPRECATED larsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
