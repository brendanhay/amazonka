{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeEventCategories
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays a list of categories for all event source types, or, if specified, for a specified source type. You can see a list of the event categories and source types in <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Events.html Events> in the /Amazon RDS User Guide./
module Network.AWS.RDS.DescribeEventCategories
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
    decrsEventCategoriesMapList,
    decrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDescribeEventCategories' smart constructor.
data DescribeEventCategories = DescribeEventCategories'
  { sourceType ::
      Lude.Maybe Lude.Text,
    filters :: Lude.Maybe [Filter]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEventCategories' with the minimum fields required to make a request.
--
-- * 'filters' - This parameter isn't currently supported.
-- * 'sourceType' - The type of source that is generating the events.
--
-- Valid values: @db-instance@ | @db-cluster@ | @db-parameter-group@ | @db-security-group@ | @db-snapshot@ | @db-cluster-snapshot@
mkDescribeEventCategories ::
  DescribeEventCategories
mkDescribeEventCategories =
  DescribeEventCategories'
    { sourceType = Lude.Nothing,
      filters = Lude.Nothing
    }

-- | The type of source that is generating the events.
--
-- Valid values: @db-instance@ | @db-cluster@ | @db-parameter-group@ | @db-security-group@ | @db-snapshot@ | @db-cluster-snapshot@
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decSourceType :: Lens.Lens' DescribeEventCategories (Lude.Maybe Lude.Text)
decSourceType = Lens.lens (sourceType :: DescribeEventCategories -> Lude.Maybe Lude.Text) (\s a -> s {sourceType = a} :: DescribeEventCategories)
{-# DEPRECATED decSourceType "Use generic-lens or generic-optics with 'sourceType' instead." #-}

-- | This parameter isn't currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decFilters :: Lens.Lens' DescribeEventCategories (Lude.Maybe [Filter])
decFilters = Lens.lens (filters :: DescribeEventCategories -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeEventCategories)
{-# DEPRECATED decFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

instance Lude.AWSRequest DescribeEventCategories where
  type Rs DescribeEventCategories = DescribeEventCategoriesResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "DescribeEventCategoriesResult"
      ( \s h x ->
          DescribeEventCategoriesResponse'
            Lude.<$> ( x Lude..@? "EventCategoriesMapList" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "EventCategoriesMap")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeEventCategories where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeEventCategories where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeEventCategories where
  toQuery DescribeEventCategories' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeEventCategories" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "SourceType" Lude.=: sourceType,
        "Filters"
          Lude.=: Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters)
      ]

-- | Data returned from the @DescribeEventCategories@ operation.
--
-- /See:/ 'mkDescribeEventCategoriesResponse' smart constructor.
data DescribeEventCategoriesResponse = DescribeEventCategoriesResponse'
  { eventCategoriesMapList ::
      Lude.Maybe
        [EventCategoriesMap],
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

-- | Creates a value of 'DescribeEventCategoriesResponse' with the minimum fields required to make a request.
--
-- * 'eventCategoriesMapList' - A list of EventCategoriesMap data types.
-- * 'responseStatus' - The response status code.
mkDescribeEventCategoriesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeEventCategoriesResponse
mkDescribeEventCategoriesResponse pResponseStatus_ =
  DescribeEventCategoriesResponse'
    { eventCategoriesMapList =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of EventCategoriesMap data types.
--
-- /Note:/ Consider using 'eventCategoriesMapList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrsEventCategoriesMapList :: Lens.Lens' DescribeEventCategoriesResponse (Lude.Maybe [EventCategoriesMap])
decrsEventCategoriesMapList = Lens.lens (eventCategoriesMapList :: DescribeEventCategoriesResponse -> Lude.Maybe [EventCategoriesMap]) (\s a -> s {eventCategoriesMapList = a} :: DescribeEventCategoriesResponse)
{-# DEPRECATED decrsEventCategoriesMapList "Use generic-lens or generic-optics with 'eventCategoriesMapList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrsResponseStatus :: Lens.Lens' DescribeEventCategoriesResponse Lude.Int
decrsResponseStatus = Lens.lens (responseStatus :: DescribeEventCategoriesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeEventCategoriesResponse)
{-# DEPRECATED decrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
