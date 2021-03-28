{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.EventTopic
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectoryService.Types.EventTopic
  ( EventTopic (..)
  -- * Smart constructor
  , mkEventTopic
  -- * Lenses
  , etCreatedDateTime
  , etDirectoryId
  , etStatus
  , etTopicArn
  , etTopicName
  ) where

import qualified Network.AWS.DirectoryService.Types.DirectoryId as Types
import qualified Network.AWS.DirectoryService.Types.TopicArn as Types
import qualified Network.AWS.DirectoryService.Types.TopicName as Types
import qualified Network.AWS.DirectoryService.Types.TopicStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about SNS topic and AWS Directory Service directory associations.
--
-- /See:/ 'mkEventTopic' smart constructor.
data EventTopic = EventTopic'
  { createdDateTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time of when you associated your directory with the SNS topic.
  , directoryId :: Core.Maybe Types.DirectoryId
    -- ^ The Directory ID of an AWS Directory Service directory that will publish status messages to an SNS topic.
  , status :: Core.Maybe Types.TopicStatus
    -- ^ The topic registration status.
  , topicArn :: Core.Maybe Types.TopicArn
    -- ^ The SNS topic ARN (Amazon Resource Name).
  , topicName :: Core.Maybe Types.TopicName
    -- ^ The name of an AWS SNS topic the receives status messages from the directory.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'EventTopic' value with any optional fields omitted.
mkEventTopic
    :: EventTopic
mkEventTopic
  = EventTopic'{createdDateTime = Core.Nothing,
                directoryId = Core.Nothing, status = Core.Nothing,
                topicArn = Core.Nothing, topicName = Core.Nothing}

-- | The date and time of when you associated your directory with the SNS topic.
--
-- /Note:/ Consider using 'createdDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etCreatedDateTime :: Lens.Lens' EventTopic (Core.Maybe Core.NominalDiffTime)
etCreatedDateTime = Lens.field @"createdDateTime"
{-# INLINEABLE etCreatedDateTime #-}
{-# DEPRECATED createdDateTime "Use generic-lens or generic-optics with 'createdDateTime' instead"  #-}

-- | The Directory ID of an AWS Directory Service directory that will publish status messages to an SNS topic.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etDirectoryId :: Lens.Lens' EventTopic (Core.Maybe Types.DirectoryId)
etDirectoryId = Lens.field @"directoryId"
{-# INLINEABLE etDirectoryId #-}
{-# DEPRECATED directoryId "Use generic-lens or generic-optics with 'directoryId' instead"  #-}

-- | The topic registration status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etStatus :: Lens.Lens' EventTopic (Core.Maybe Types.TopicStatus)
etStatus = Lens.field @"status"
{-# INLINEABLE etStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The SNS topic ARN (Amazon Resource Name).
--
-- /Note:/ Consider using 'topicArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etTopicArn :: Lens.Lens' EventTopic (Core.Maybe Types.TopicArn)
etTopicArn = Lens.field @"topicArn"
{-# INLINEABLE etTopicArn #-}
{-# DEPRECATED topicArn "Use generic-lens or generic-optics with 'topicArn' instead"  #-}

-- | The name of an AWS SNS topic the receives status messages from the directory.
--
-- /Note:/ Consider using 'topicName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etTopicName :: Lens.Lens' EventTopic (Core.Maybe Types.TopicName)
etTopicName = Lens.field @"topicName"
{-# INLINEABLE etTopicName #-}
{-# DEPRECATED topicName "Use generic-lens or generic-optics with 'topicName' instead"  #-}

instance Core.FromJSON EventTopic where
        parseJSON
          = Core.withObject "EventTopic" Core.$
              \ x ->
                EventTopic' Core.<$>
                  (x Core..:? "CreatedDateTime") Core.<*> x Core..:? "DirectoryId"
                    Core.<*> x Core..:? "Status"
                    Core.<*> x Core..:? "TopicArn"
                    Core.<*> x Core..:? "TopicName"
