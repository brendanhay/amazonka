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
    rUserIdentity,
    rEventVersion,
    rDynamodb,
    rAwsRegion,
    rEventName,
    rEventSource,
    rEventId,
  )
where

import Network.AWS.DynamoDBStreams.Types.Identity
import Network.AWS.DynamoDBStreams.Types.OperationType
import Network.AWS.DynamoDBStreams.Types.StreamRecord
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A description of a unique event within a stream.
--
-- /See:/ 'mkRecord' smart constructor.
data Record = Record'
  { userIdentity :: Lude.Maybe Identity,
    eventVersion :: Lude.Maybe Lude.Text,
    dynamodb :: Lude.Maybe StreamRecord,
    awsRegion :: Lude.Maybe Lude.Text,
    eventName :: Lude.Maybe OperationType,
    eventSource :: Lude.Maybe Lude.Text,
    eventId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Record' with the minimum fields required to make a request.
--
-- * 'awsRegion' - The region in which the @GetRecords@ request was received.
-- * 'dynamodb' - The main body of the stream record, containing all of the DynamoDB-specific fields.
-- * 'eventId' - A globally unique identifier for the event that was recorded in this stream record.
-- * 'eventName' - The type of data modification that was performed on the DynamoDB table:
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
-- * 'eventSource' - The AWS service from which the stream record originated. For DynamoDB Streams, this is @aws:dynamodb@ .
-- * 'eventVersion' - The version number of the stream record format. This number is updated whenever the structure of @Record@ is modified.
--
-- Client applications must not assume that @eventVersion@ will remain at a particular value, as this number is subject to change at any time. In general, @eventVersion@ will only increase as the low-level DynamoDB Streams API evolves.
-- * 'userIdentity' - Items that are deleted by the Time to Live process after expiration have the following fields:
--
--
--     * Records[].userIdentity.type
-- "Service"
--
--
--     * Records[].userIdentity.principalId
-- "dynamodb.amazonaws.com"
mkRecord ::
  Record
mkRecord =
  Record'
    { userIdentity = Lude.Nothing,
      eventVersion = Lude.Nothing,
      dynamodb = Lude.Nothing,
      awsRegion = Lude.Nothing,
      eventName = Lude.Nothing,
      eventSource = Lude.Nothing,
      eventId = Lude.Nothing
    }

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
rUserIdentity :: Lens.Lens' Record (Lude.Maybe Identity)
rUserIdentity = Lens.lens (userIdentity :: Record -> Lude.Maybe Identity) (\s a -> s {userIdentity = a} :: Record)
{-# DEPRECATED rUserIdentity "Use generic-lens or generic-optics with 'userIdentity' instead." #-}

-- | The version number of the stream record format. This number is updated whenever the structure of @Record@ is modified.
--
-- Client applications must not assume that @eventVersion@ will remain at a particular value, as this number is subject to change at any time. In general, @eventVersion@ will only increase as the low-level DynamoDB Streams API evolves.
--
-- /Note:/ Consider using 'eventVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rEventVersion :: Lens.Lens' Record (Lude.Maybe Lude.Text)
rEventVersion = Lens.lens (eventVersion :: Record -> Lude.Maybe Lude.Text) (\s a -> s {eventVersion = a} :: Record)
{-# DEPRECATED rEventVersion "Use generic-lens or generic-optics with 'eventVersion' instead." #-}

-- | The main body of the stream record, containing all of the DynamoDB-specific fields.
--
-- /Note:/ Consider using 'dynamodb' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDynamodb :: Lens.Lens' Record (Lude.Maybe StreamRecord)
rDynamodb = Lens.lens (dynamodb :: Record -> Lude.Maybe StreamRecord) (\s a -> s {dynamodb = a} :: Record)
{-# DEPRECATED rDynamodb "Use generic-lens or generic-optics with 'dynamodb' instead." #-}

-- | The region in which the @GetRecords@ request was received.
--
-- /Note:/ Consider using 'awsRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rAwsRegion :: Lens.Lens' Record (Lude.Maybe Lude.Text)
rAwsRegion = Lens.lens (awsRegion :: Record -> Lude.Maybe Lude.Text) (\s a -> s {awsRegion = a} :: Record)
{-# DEPRECATED rAwsRegion "Use generic-lens or generic-optics with 'awsRegion' instead." #-}

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
rEventName :: Lens.Lens' Record (Lude.Maybe OperationType)
rEventName = Lens.lens (eventName :: Record -> Lude.Maybe OperationType) (\s a -> s {eventName = a} :: Record)
{-# DEPRECATED rEventName "Use generic-lens or generic-optics with 'eventName' instead." #-}

-- | The AWS service from which the stream record originated. For DynamoDB Streams, this is @aws:dynamodb@ .
--
-- /Note:/ Consider using 'eventSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rEventSource :: Lens.Lens' Record (Lude.Maybe Lude.Text)
rEventSource = Lens.lens (eventSource :: Record -> Lude.Maybe Lude.Text) (\s a -> s {eventSource = a} :: Record)
{-# DEPRECATED rEventSource "Use generic-lens or generic-optics with 'eventSource' instead." #-}

-- | A globally unique identifier for the event that was recorded in this stream record.
--
-- /Note:/ Consider using 'eventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rEventId :: Lens.Lens' Record (Lude.Maybe Lude.Text)
rEventId = Lens.lens (eventId :: Record -> Lude.Maybe Lude.Text) (\s a -> s {eventId = a} :: Record)
{-# DEPRECATED rEventId "Use generic-lens or generic-optics with 'eventId' instead." #-}

instance Lude.FromJSON Record where
  parseJSON =
    Lude.withObject
      "Record"
      ( \x ->
          Record'
            Lude.<$> (x Lude..:? "userIdentity")
            Lude.<*> (x Lude..:? "eventVersion")
            Lude.<*> (x Lude..:? "dynamodb")
            Lude.<*> (x Lude..:? "awsRegion")
            Lude.<*> (x Lude..:? "eventName")
            Lude.<*> (x Lude..:? "eventSource")
            Lude.<*> (x Lude..:? "eventID")
      )
