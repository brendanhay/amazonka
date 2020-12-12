{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.EventTopic
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.EventTopic
  ( EventTopic (..),

    -- * Smart constructor
    mkEventTopic,

    -- * Lenses
    etStatus,
    etDirectoryId,
    etTopicName,
    etTopicARN,
    etCreatedDateTime,
  )
where

import Network.AWS.DirectoryService.Types.TopicStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about SNS topic and AWS Directory Service directory associations.
--
-- /See:/ 'mkEventTopic' smart constructor.
data EventTopic = EventTopic'
  { status :: Lude.Maybe TopicStatus,
    directoryId :: Lude.Maybe Lude.Text,
    topicName :: Lude.Maybe Lude.Text,
    topicARN :: Lude.Maybe Lude.Text,
    createdDateTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EventTopic' with the minimum fields required to make a request.
--
-- * 'createdDateTime' - The date and time of when you associated your directory with the SNS topic.
-- * 'directoryId' - The Directory ID of an AWS Directory Service directory that will publish status messages to an SNS topic.
-- * 'status' - The topic registration status.
-- * 'topicARN' - The SNS topic ARN (Amazon Resource Name).
-- * 'topicName' - The name of an AWS SNS topic the receives status messages from the directory.
mkEventTopic ::
  EventTopic
mkEventTopic =
  EventTopic'
    { status = Lude.Nothing,
      directoryId = Lude.Nothing,
      topicName = Lude.Nothing,
      topicARN = Lude.Nothing,
      createdDateTime = Lude.Nothing
    }

-- | The topic registration status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etStatus :: Lens.Lens' EventTopic (Lude.Maybe TopicStatus)
etStatus = Lens.lens (status :: EventTopic -> Lude.Maybe TopicStatus) (\s a -> s {status = a} :: EventTopic)
{-# DEPRECATED etStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The Directory ID of an AWS Directory Service directory that will publish status messages to an SNS topic.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etDirectoryId :: Lens.Lens' EventTopic (Lude.Maybe Lude.Text)
etDirectoryId = Lens.lens (directoryId :: EventTopic -> Lude.Maybe Lude.Text) (\s a -> s {directoryId = a} :: EventTopic)
{-# DEPRECATED etDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The name of an AWS SNS topic the receives status messages from the directory.
--
-- /Note:/ Consider using 'topicName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etTopicName :: Lens.Lens' EventTopic (Lude.Maybe Lude.Text)
etTopicName = Lens.lens (topicName :: EventTopic -> Lude.Maybe Lude.Text) (\s a -> s {topicName = a} :: EventTopic)
{-# DEPRECATED etTopicName "Use generic-lens or generic-optics with 'topicName' instead." #-}

-- | The SNS topic ARN (Amazon Resource Name).
--
-- /Note:/ Consider using 'topicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etTopicARN :: Lens.Lens' EventTopic (Lude.Maybe Lude.Text)
etTopicARN = Lens.lens (topicARN :: EventTopic -> Lude.Maybe Lude.Text) (\s a -> s {topicARN = a} :: EventTopic)
{-# DEPRECATED etTopicARN "Use generic-lens or generic-optics with 'topicARN' instead." #-}

-- | The date and time of when you associated your directory with the SNS topic.
--
-- /Note:/ Consider using 'createdDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etCreatedDateTime :: Lens.Lens' EventTopic (Lude.Maybe Lude.Timestamp)
etCreatedDateTime = Lens.lens (createdDateTime :: EventTopic -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdDateTime = a} :: EventTopic)
{-# DEPRECATED etCreatedDateTime "Use generic-lens or generic-optics with 'createdDateTime' instead." #-}

instance Lude.FromJSON EventTopic where
  parseJSON =
    Lude.withObject
      "EventTopic"
      ( \x ->
          EventTopic'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "DirectoryId")
            Lude.<*> (x Lude..:? "TopicName")
            Lude.<*> (x Lude..:? "TopicArn")
            Lude.<*> (x Lude..:? "CreatedDateTime")
      )
