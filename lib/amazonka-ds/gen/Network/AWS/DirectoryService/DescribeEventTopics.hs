{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.DescribeEventTopics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Obtains information about which SNS topics receive status messages from the specified directory.
--
-- If no input parameters are provided, such as DirectoryId or TopicName, this request describes all of the associations in the account.
module Network.AWS.DirectoryService.DescribeEventTopics
  ( -- * Creating a request
    DescribeEventTopics (..),
    mkDescribeEventTopics,

    -- ** Request lenses
    dDirectoryId,
    dTopicNames,

    -- * Destructuring the response
    DescribeEventTopicsResponse (..),
    mkDescribeEventTopicsResponse,

    -- ** Response lenses
    detrsEventTopics,
    detrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Describes event topics.
--
-- /See:/ 'mkDescribeEventTopics' smart constructor.
data DescribeEventTopics = DescribeEventTopics'
  { directoryId ::
      Lude.Maybe Lude.Text,
    topicNames :: Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEventTopics' with the minimum fields required to make a request.
--
-- * 'directoryId' - The Directory ID for which to get the list of associated SNS topics. If this member is null, associations for all Directory IDs are returned.
-- * 'topicNames' - A list of SNS topic names for which to obtain the information. If this member is null, all associations for the specified Directory ID are returned.
--
-- An empty list results in an @InvalidParameterException@ being thrown.
mkDescribeEventTopics ::
  DescribeEventTopics
mkDescribeEventTopics =
  DescribeEventTopics'
    { directoryId = Lude.Nothing,
      topicNames = Lude.Nothing
    }

-- | The Directory ID for which to get the list of associated SNS topics. If this member is null, associations for all Directory IDs are returned.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDirectoryId :: Lens.Lens' DescribeEventTopics (Lude.Maybe Lude.Text)
dDirectoryId = Lens.lens (directoryId :: DescribeEventTopics -> Lude.Maybe Lude.Text) (\s a -> s {directoryId = a} :: DescribeEventTopics)
{-# DEPRECATED dDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | A list of SNS topic names for which to obtain the information. If this member is null, all associations for the specified Directory ID are returned.
--
-- An empty list results in an @InvalidParameterException@ being thrown.
--
-- /Note:/ Consider using 'topicNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dTopicNames :: Lens.Lens' DescribeEventTopics (Lude.Maybe [Lude.Text])
dTopicNames = Lens.lens (topicNames :: DescribeEventTopics -> Lude.Maybe [Lude.Text]) (\s a -> s {topicNames = a} :: DescribeEventTopics)
{-# DEPRECATED dTopicNames "Use generic-lens or generic-optics with 'topicNames' instead." #-}

instance Lude.AWSRequest DescribeEventTopics where
  type Rs DescribeEventTopics = DescribeEventTopicsResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeEventTopicsResponse'
            Lude.<$> (x Lude..?> "EventTopics" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeEventTopics where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "DirectoryService_20150416.DescribeEventTopics" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeEventTopics where
  toJSON DescribeEventTopics' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DirectoryId" Lude..=) Lude.<$> directoryId,
            ("TopicNames" Lude..=) Lude.<$> topicNames
          ]
      )

instance Lude.ToPath DescribeEventTopics where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeEventTopics where
  toQuery = Lude.const Lude.mempty

-- | The result of a DescribeEventTopic request.
--
-- /See:/ 'mkDescribeEventTopicsResponse' smart constructor.
data DescribeEventTopicsResponse = DescribeEventTopicsResponse'
  { eventTopics ::
      Lude.Maybe [EventTopic],
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

-- | Creates a value of 'DescribeEventTopicsResponse' with the minimum fields required to make a request.
--
-- * 'eventTopics' - A list of SNS topic names that receive status messages from the specified Directory ID.
-- * 'responseStatus' - The response status code.
mkDescribeEventTopicsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeEventTopicsResponse
mkDescribeEventTopicsResponse pResponseStatus_ =
  DescribeEventTopicsResponse'
    { eventTopics = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of SNS topic names that receive status messages from the specified Directory ID.
--
-- /Note:/ Consider using 'eventTopics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detrsEventTopics :: Lens.Lens' DescribeEventTopicsResponse (Lude.Maybe [EventTopic])
detrsEventTopics = Lens.lens (eventTopics :: DescribeEventTopicsResponse -> Lude.Maybe [EventTopic]) (\s a -> s {eventTopics = a} :: DescribeEventTopicsResponse)
{-# DEPRECATED detrsEventTopics "Use generic-lens or generic-optics with 'eventTopics' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detrsResponseStatus :: Lens.Lens' DescribeEventTopicsResponse Lude.Int
detrsResponseStatus = Lens.lens (responseStatus :: DescribeEventTopicsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeEventTopicsResponse)
{-# DEPRECATED detrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
