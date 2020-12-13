{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.GetResourceConfigHistory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of configuration items for the specified resource. The list contains details about each state of the resource during the specified time interval. If you specified a retention period to retain your @ConfigurationItems@ between a minimum of 30 days and a maximum of 7 years (2557 days), AWS Config returns the @ConfigurationItems@ for the specified retention period.
--
-- The response is paginated. By default, AWS Config returns a limit of 10 configuration items per page. You can customize this number with the @limit@ parameter. The response includes a @nextToken@ string. To get the next page of results, run the request again and specify the string for the @nextToken@ parameter.
--
-- This operation returns paginated results.
module Network.AWS.Config.GetResourceConfigHistory
  ( -- * Creating a request
    GetResourceConfigHistory (..),
    mkGetResourceConfigHistory,

    -- ** Request lenses
    grchResourceId,
    grchResourceType,
    grchChronologicalOrder,
    grchNextToken,
    grchLimit,
    grchLaterTime,
    grchEarlierTime,

    -- * Destructuring the response
    GetResourceConfigHistoryResponse (..),
    mkGetResourceConfigHistoryResponse,

    -- ** Response lenses
    grchrsNextToken,
    grchrsConfigurationItems,
    grchrsResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the 'GetResourceConfigHistory' action.
--
-- /See:/ 'mkGetResourceConfigHistory' smart constructor.
data GetResourceConfigHistory = GetResourceConfigHistory'
  { -- | The ID of the resource (for example., @sg-xxxxxx@ ).
    resourceId :: Lude.Text,
    -- | The resource type.
    resourceType :: ResourceType,
    -- | The chronological order for configuration items listed. By default, the results are listed in reverse chronological order.
    chronologicalOrder :: Lude.Maybe ChronologicalOrder,
    -- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of configuration items returned on each page. The default is 10. You cannot specify a number greater than 100. If you specify 0, AWS Config uses the default.
    limit :: Lude.Maybe Lude.Natural,
    -- | The time stamp that indicates a later time. If not specified, current time is taken.
    laterTime :: Lude.Maybe Lude.Timestamp,
    -- | The time stamp that indicates an earlier time. If not specified, the action returns paginated results that contain configuration items that start when the first configuration item was recorded.
    earlierTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetResourceConfigHistory' with the minimum fields required to make a request.
--
-- * 'resourceId' - The ID of the resource (for example., @sg-xxxxxx@ ).
-- * 'resourceType' - The resource type.
-- * 'chronologicalOrder' - The chronological order for configuration items listed. By default, the results are listed in reverse chronological order.
-- * 'nextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
-- * 'limit' - The maximum number of configuration items returned on each page. The default is 10. You cannot specify a number greater than 100. If you specify 0, AWS Config uses the default.
-- * 'laterTime' - The time stamp that indicates a later time. If not specified, current time is taken.
-- * 'earlierTime' - The time stamp that indicates an earlier time. If not specified, the action returns paginated results that contain configuration items that start when the first configuration item was recorded.
mkGetResourceConfigHistory ::
  -- | 'resourceId'
  Lude.Text ->
  -- | 'resourceType'
  ResourceType ->
  GetResourceConfigHistory
mkGetResourceConfigHistory pResourceId_ pResourceType_ =
  GetResourceConfigHistory'
    { resourceId = pResourceId_,
      resourceType = pResourceType_,
      chronologicalOrder = Lude.Nothing,
      nextToken = Lude.Nothing,
      limit = Lude.Nothing,
      laterTime = Lude.Nothing,
      earlierTime = Lude.Nothing
    }

-- | The ID of the resource (for example., @sg-xxxxxx@ ).
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grchResourceId :: Lens.Lens' GetResourceConfigHistory Lude.Text
grchResourceId = Lens.lens (resourceId :: GetResourceConfigHistory -> Lude.Text) (\s a -> s {resourceId = a} :: GetResourceConfigHistory)
{-# DEPRECATED grchResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The resource type.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grchResourceType :: Lens.Lens' GetResourceConfigHistory ResourceType
grchResourceType = Lens.lens (resourceType :: GetResourceConfigHistory -> ResourceType) (\s a -> s {resourceType = a} :: GetResourceConfigHistory)
{-# DEPRECATED grchResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The chronological order for configuration items listed. By default, the results are listed in reverse chronological order.
--
-- /Note:/ Consider using 'chronologicalOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grchChronologicalOrder :: Lens.Lens' GetResourceConfigHistory (Lude.Maybe ChronologicalOrder)
grchChronologicalOrder = Lens.lens (chronologicalOrder :: GetResourceConfigHistory -> Lude.Maybe ChronologicalOrder) (\s a -> s {chronologicalOrder = a} :: GetResourceConfigHistory)
{-# DEPRECATED grchChronologicalOrder "Use generic-lens or generic-optics with 'chronologicalOrder' instead." #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grchNextToken :: Lens.Lens' GetResourceConfigHistory (Lude.Maybe Lude.Text)
grchNextToken = Lens.lens (nextToken :: GetResourceConfigHistory -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetResourceConfigHistory)
{-# DEPRECATED grchNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of configuration items returned on each page. The default is 10. You cannot specify a number greater than 100. If you specify 0, AWS Config uses the default.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grchLimit :: Lens.Lens' GetResourceConfigHistory (Lude.Maybe Lude.Natural)
grchLimit = Lens.lens (limit :: GetResourceConfigHistory -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: GetResourceConfigHistory)
{-# DEPRECATED grchLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The time stamp that indicates a later time. If not specified, current time is taken.
--
-- /Note:/ Consider using 'laterTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grchLaterTime :: Lens.Lens' GetResourceConfigHistory (Lude.Maybe Lude.Timestamp)
grchLaterTime = Lens.lens (laterTime :: GetResourceConfigHistory -> Lude.Maybe Lude.Timestamp) (\s a -> s {laterTime = a} :: GetResourceConfigHistory)
{-# DEPRECATED grchLaterTime "Use generic-lens or generic-optics with 'laterTime' instead." #-}

-- | The time stamp that indicates an earlier time. If not specified, the action returns paginated results that contain configuration items that start when the first configuration item was recorded.
--
-- /Note:/ Consider using 'earlierTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grchEarlierTime :: Lens.Lens' GetResourceConfigHistory (Lude.Maybe Lude.Timestamp)
grchEarlierTime = Lens.lens (earlierTime :: GetResourceConfigHistory -> Lude.Maybe Lude.Timestamp) (\s a -> s {earlierTime = a} :: GetResourceConfigHistory)
{-# DEPRECATED grchEarlierTime "Use generic-lens or generic-optics with 'earlierTime' instead." #-}

instance Page.AWSPager GetResourceConfigHistory where
  page rq rs
    | Page.stop (rs Lens.^. grchrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. grchrsConfigurationItems) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& grchNextToken Lens..~ rs Lens.^. grchrsNextToken

instance Lude.AWSRequest GetResourceConfigHistory where
  type Rs GetResourceConfigHistory = GetResourceConfigHistoryResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetResourceConfigHistoryResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "configurationItems" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetResourceConfigHistory where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.GetResourceConfigHistory" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetResourceConfigHistory where
  toJSON GetResourceConfigHistory' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("resourceId" Lude..= resourceId),
            Lude.Just ("resourceType" Lude..= resourceType),
            ("chronologicalOrder" Lude..=) Lude.<$> chronologicalOrder,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("limit" Lude..=) Lude.<$> limit,
            ("laterTime" Lude..=) Lude.<$> laterTime,
            ("earlierTime" Lude..=) Lude.<$> earlierTime
          ]
      )

instance Lude.ToPath GetResourceConfigHistory where
  toPath = Lude.const "/"

instance Lude.ToQuery GetResourceConfigHistory where
  toQuery = Lude.const Lude.mempty

-- | The output for the 'GetResourceConfigHistory' action.
--
-- /See:/ 'mkGetResourceConfigHistoryResponse' smart constructor.
data GetResourceConfigHistoryResponse = GetResourceConfigHistoryResponse'
  { -- | The string that you use in a subsequent request to get the next page of results in a paginated response.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list that contains the configuration history of one or more resources.
    configurationItems :: Lude.Maybe [ConfigurationItem],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetResourceConfigHistoryResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The string that you use in a subsequent request to get the next page of results in a paginated response.
-- * 'configurationItems' - A list that contains the configuration history of one or more resources.
-- * 'responseStatus' - The response status code.
mkGetResourceConfigHistoryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetResourceConfigHistoryResponse
mkGetResourceConfigHistoryResponse pResponseStatus_ =
  GetResourceConfigHistoryResponse'
    { nextToken = Lude.Nothing,
      configurationItems = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The string that you use in a subsequent request to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grchrsNextToken :: Lens.Lens' GetResourceConfigHistoryResponse (Lude.Maybe Lude.Text)
grchrsNextToken = Lens.lens (nextToken :: GetResourceConfigHistoryResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetResourceConfigHistoryResponse)
{-# DEPRECATED grchrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list that contains the configuration history of one or more resources.
--
-- /Note:/ Consider using 'configurationItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grchrsConfigurationItems :: Lens.Lens' GetResourceConfigHistoryResponse (Lude.Maybe [ConfigurationItem])
grchrsConfigurationItems = Lens.lens (configurationItems :: GetResourceConfigHistoryResponse -> Lude.Maybe [ConfigurationItem]) (\s a -> s {configurationItems = a} :: GetResourceConfigHistoryResponse)
{-# DEPRECATED grchrsConfigurationItems "Use generic-lens or generic-optics with 'configurationItems' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grchrsResponseStatus :: Lens.Lens' GetResourceConfigHistoryResponse Lude.Int
grchrsResponseStatus = Lens.lens (responseStatus :: GetResourceConfigHistoryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetResourceConfigHistoryResponse)
{-# DEPRECATED grchrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
