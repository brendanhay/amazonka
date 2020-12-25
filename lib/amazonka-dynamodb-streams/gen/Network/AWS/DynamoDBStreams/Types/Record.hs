{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDBStreams.Types.Record
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDBStreams.Types.Record
  ( Record (..),

    -- * Smart constructor
    mkRecord,

    -- * Lenses
    rAwsRegion,
    rDynamodb,
    rEventID,
    rEventName,
    rEventSource,
    rEventVersion,
    rUserIdentity,
  )
where

import qualified Network.AWS.DynamoDBStreams.Types.Identity as Types
import qualified Network.AWS.DynamoDBStreams.Types.OperationType as Types
import qualified Network.AWS.DynamoDBStreams.Types.StreamRecord as Types
import qualified Network.AWS.DynamoDBStreams.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A description of a unique event within a stream.
--
-- /See:/ 'mkRecord' smart constructor.
data Record = Record'
  { -- | The region in which the @GetRecords@ request was received.
    awsRegion :: Core.Maybe Types.String,
    -- | The main body of the stream record, containing all of the DynamoDB-specific fields.
    dynamodb :: Core.Maybe Types.StreamRecord,
    -- | A globally unique identifier for the event that was recorded in this stream record.
    eventID :: Core.Maybe Types.String,
    -- | The type of data modification that was performed on the DynamoDB table:
    --
    --
    --     * @INSERT@ - a new item was added to the table.
    --
    --
    --     * @MODIFY@ - one or more of an existing item's attributes were modified.
    --
    --
    --     * @REMOVE@ - the item was deleted from the table
    eventName :: Core.Maybe Types.OperationType,
    -- | The AWS service from which the stream record originated. For DynamoDB Streams, this is @aws:dynamodb@ .
    eventSource :: Core.Maybe Types.String,
    -- | The version number of the stream record format. This number is updated whenever the structure of @Record@ is modified.
    --
    -- Client applications must not assume that @eventVersion@ will remain at a particular value, as this number is subject to change at any time. In general, @eventVersion@ will only increase as the low-level DynamoDB Streams API evolves.
    eventVersion :: Core.Maybe Types.String,
    -- | Items that are deleted by the Time to Live process after expiration have the following fields:
    --
    --
    --     * Records[].userIdentity.type
    -- "Service"
    --
    --
    --     * Records[].userIdentity.principalId
    -- "dynamodb.amazonaws.com"
    userIdentity :: Core.Maybe Types.Identity
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Record' value with any optional fields omitted.
mkRecord ::
  Record
mkRecord =
  Record'
    { awsRegion = Core.Nothing,
      dynamodb = Core.Nothing,
      eventID = Core.Nothing,
      eventName = Core.Nothing,
      eventSource = Core.Nothing,
      eventVersion = Core.Nothing,
      userIdentity = Core.Nothing
    }

-- | The region in which the @GetRecords@ request was received.
--
-- /Note:/ Consider using 'awsRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rAwsRegion :: Lens.Lens' Record (Core.Maybe Types.String)
rAwsRegion = Lens.field @"awsRegion"
{-# DEPRECATED rAwsRegion "Use generic-lens or generic-optics with 'awsRegion' instead." #-}

-- | The main body of the stream record, containing all of the DynamoDB-specific fields.
--
-- /Note:/ Consider using 'dynamodb' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDynamodb :: Lens.Lens' Record (Core.Maybe Types.StreamRecord)
rDynamodb = Lens.field @"dynamodb"
{-# DEPRECATED rDynamodb "Use generic-lens or generic-optics with 'dynamodb' instead." #-}

-- | A globally unique identifier for the event that was recorded in this stream record.
--
-- /Note:/ Consider using 'eventID' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rEventID :: Lens.Lens' Record (Core.Maybe Types.String)
rEventID = Lens.field @"eventID"
{-# DEPRECATED rEventID "Use generic-lens or generic-optics with 'eventID' instead." #-}

-- | The type of data modification that was performed on the DynamoDB table:
--
--
--     * @INSERT@ - a new item was added to the table.
--
--
--     * @MODIFY@ - one or more of an existing item's attributes were modified.
--
--
--     * @REMOVE@ - the item was deleted from the table
--
--
--
-- /Note:/ Consider using 'eventName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rEventName :: Lens.Lens' Record (Core.Maybe Types.OperationType)
rEventName = Lens.field @"eventName"
{-# DEPRECATED rEventName "Use generic-lens or generic-optics with 'eventName' instead." #-}

-- | The AWS service from which the stream record originated. For DynamoDB Streams, this is @aws:dynamodb@ .
--
-- /Note:/ Consider using 'eventSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rEventSource :: Lens.Lens' Record (Core.Maybe Types.String)
rEventSource = Lens.field @"eventSource"
{-# DEPRECATED rEventSource "Use generic-lens or generic-optics with 'eventSource' instead." #-}

-- | The version number of the stream record format. This number is updated whenever the structure of @Record@ is modified.
--
-- Client applications must not assume that @eventVersion@ will remain at a particular value, as this number is subject to change at any time. In general, @eventVersion@ will only increase as the low-level DynamoDB Streams API evolves.
--
-- /Note:/ Consider using 'eventVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rEventVersion :: Lens.Lens' Record (Core.Maybe Types.String)
rEventVersion = Lens.field @"eventVersion"
{-# DEPRECATED rEventVersion "Use generic-lens or generic-optics with 'eventVersion' instead." #-}

-- | Items that are deleted by the Time to Live process after expiration have the following fields:
--
--
--     * Records[].userIdentity.type
-- "Service"
--
--
--     * Records[].userIdentity.principalId
-- "dynamodb.amazonaws.com"
--
--
--
-- /Note:/ Consider using 'userIdentity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rUserIdentity :: Lens.Lens' Record (Core.Maybe Types.Identity)
rUserIdentity = Lens.field @"userIdentity"
{-# DEPRECATED rUserIdentity "Use generic-lens or generic-optics with 'userIdentity' instead." #-}

instance Core.FromJSON Record where
  parseJSON =
    Core.withObject "Record" Core.$
      \x ->
        Record'
          Core.<$> (x Core..:? "awsRegion")
          Core.<*> (x Core..:? "dynamodb")
          Core.<*> (x Core..:? "eventID")
          Core.<*> (x Core..:? "eventName")
          Core.<*> (x Core..:? "eventSource")
          Core.<*> (x Core..:? "eventVersion")
          Core.<*> (x Core..:? "userIdentity")
