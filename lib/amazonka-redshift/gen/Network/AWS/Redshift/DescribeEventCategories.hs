{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeEventCategories
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays a list of event categories for all event source types, or for a specified source type. For a list of the event categories and source types, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-event-notifications.html Amazon Redshift Event Notifications> .
module Network.AWS.Redshift.DescribeEventCategories
  ( -- * Creating a request
    DescribeEventCategories (..),
    mkDescribeEventCategories,

    -- ** Request lenses
    decSourceType,

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
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDescribeEventCategories' smart constructor.
newtype DescribeEventCategories = DescribeEventCategories'
  { sourceType ::
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

-- | Creates a value of 'DescribeEventCategories' with the minimum fields required to make a request.
--
-- * 'sourceType' - The source type, such as cluster or parameter group, to which the described event categories apply.
--
-- Valid values: cluster, cluster-snapshot, cluster-parameter-group, cluster-security-group, and scheduled-action.
mkDescribeEventCategories ::
  DescribeEventCategories
mkDescribeEventCategories =
  DescribeEventCategories' {sourceType = Lude.Nothing}

-- | The source type, such as cluster or parameter group, to which the described event categories apply.
--
-- Valid values: cluster, cluster-snapshot, cluster-parameter-group, cluster-security-group, and scheduled-action.
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decSourceType :: Lens.Lens' DescribeEventCategories (Lude.Maybe Lude.Text)
decSourceType = Lens.lens (sourceType :: DescribeEventCategories -> Lude.Maybe Lude.Text) (\s a -> s {sourceType = a} :: DescribeEventCategories)
{-# DEPRECATED decSourceType "Use generic-lens or generic-optics with 'sourceType' instead." #-}

instance Lude.AWSRequest DescribeEventCategories where
  type Rs DescribeEventCategories = DescribeEventCategoriesResponse
  request = Req.postQuery redshiftService
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
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "SourceType" Lude.=: sourceType
      ]

-- |
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
-- * 'eventCategoriesMapList' - A list of event categories descriptions.
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

-- | A list of event categories descriptions.
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
