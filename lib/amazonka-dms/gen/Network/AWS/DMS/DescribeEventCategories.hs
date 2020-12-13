{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DescribeEventCategories
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists categories for all event source types, or, if specified, for a specified source type. You can see a list of the event categories and source types in <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Events.html Working with Events and Notifications> in the /AWS Database Migration Service User Guide./
module Network.AWS.DMS.DescribeEventCategories
  ( -- * Creating a request
    DescribeEventCategories (..),
    mkDescribeEventCategories,

    -- ** Request lenses
    decSourceType,
    decFilters,

    -- * Destructuring the response
    DescribeEventCategoriesResponse (..),
    mkDescribeEventCategoriesResponse,

    -- ** Response lenses
    decrsEventCategoryGroupList,
    decrsResponseStatus,
  )
where

import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDescribeEventCategories' smart constructor.
data DescribeEventCategories = DescribeEventCategories'
  { -- | The type of AWS DMS resource that generates events.
    --
    -- Valid values: replication-instance | replication-task
    sourceType :: Lude.Maybe Lude.Text,
    -- | Filters applied to the event categories.
    filters :: Lude.Maybe [Filter]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEventCategories' with the minimum fields required to make a request.
--
-- * 'sourceType' - The type of AWS DMS resource that generates events.
--
-- Valid values: replication-instance | replication-task
-- * 'filters' - Filters applied to the event categories.
mkDescribeEventCategories ::
  DescribeEventCategories
mkDescribeEventCategories =
  DescribeEventCategories'
    { sourceType = Lude.Nothing,
      filters = Lude.Nothing
    }

-- | The type of AWS DMS resource that generates events.
--
-- Valid values: replication-instance | replication-task
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decSourceType :: Lens.Lens' DescribeEventCategories (Lude.Maybe Lude.Text)
decSourceType = Lens.lens (sourceType :: DescribeEventCategories -> Lude.Maybe Lude.Text) (\s a -> s {sourceType = a} :: DescribeEventCategories)
{-# DEPRECATED decSourceType "Use generic-lens or generic-optics with 'sourceType' instead." #-}

-- | Filters applied to the event categories.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decFilters :: Lens.Lens' DescribeEventCategories (Lude.Maybe [Filter])
decFilters = Lens.lens (filters :: DescribeEventCategories -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeEventCategories)
{-# DEPRECATED decFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

instance Lude.AWSRequest DescribeEventCategories where
  type Rs DescribeEventCategories = DescribeEventCategoriesResponse
  request = Req.postJSON dmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeEventCategoriesResponse'
            Lude.<$> (x Lude..?> "EventCategoryGroupList" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeEventCategories where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonDMSv20160101.DescribeEventCategories" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeEventCategories where
  toJSON DescribeEventCategories' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SourceType" Lude..=) Lude.<$> sourceType,
            ("Filters" Lude..=) Lude.<$> filters
          ]
      )

instance Lude.ToPath DescribeEventCategories where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeEventCategories where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkDescribeEventCategoriesResponse' smart constructor.
data DescribeEventCategoriesResponse = DescribeEventCategoriesResponse'
  { -- | A list of event categories.
    eventCategoryGroupList :: Lude.Maybe [EventCategoryGroup],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEventCategoriesResponse' with the minimum fields required to make a request.
--
-- * 'eventCategoryGroupList' - A list of event categories.
-- * 'responseStatus' - The response status code.
mkDescribeEventCategoriesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeEventCategoriesResponse
mkDescribeEventCategoriesResponse pResponseStatus_ =
  DescribeEventCategoriesResponse'
    { eventCategoryGroupList =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of event categories.
--
-- /Note:/ Consider using 'eventCategoryGroupList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrsEventCategoryGroupList :: Lens.Lens' DescribeEventCategoriesResponse (Lude.Maybe [EventCategoryGroup])
decrsEventCategoryGroupList = Lens.lens (eventCategoryGroupList :: DescribeEventCategoriesResponse -> Lude.Maybe [EventCategoryGroup]) (\s a -> s {eventCategoryGroupList = a} :: DescribeEventCategoriesResponse)
{-# DEPRECATED decrsEventCategoryGroupList "Use generic-lens or generic-optics with 'eventCategoryGroupList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrsResponseStatus :: Lens.Lens' DescribeEventCategoriesResponse Lude.Int
decrsResponseStatus = Lens.lens (responseStatus :: DescribeEventCategoriesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeEventCategoriesResponse)
{-# DEPRECATED decrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
