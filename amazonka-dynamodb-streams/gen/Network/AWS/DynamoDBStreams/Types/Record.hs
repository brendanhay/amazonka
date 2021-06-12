{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDBStreams.Types.Record
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDBStreams.Types.Record where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDBStreams.Types.Identity
import Network.AWS.DynamoDBStreams.Types.OperationType
import Network.AWS.DynamoDBStreams.Types.StreamRecord
import qualified Network.AWS.Lens as Lens

-- | A description of a unique event within a stream.
--
-- /See:/ 'newRecord' smart constructor.
data Record = Record'
  { -- | Items that are deleted by the Time to Live process after expiration have
    -- the following fields:
    --
    -- -   Records[].userIdentity.type
    --
    --     \"Service\"
    --
    -- -   Records[].userIdentity.principalId
    --
    --     \"dynamodb.amazonaws.com\"
    userIdentity :: Core.Maybe Identity,
    -- | A globally unique identifier for the event that was recorded in this
    -- stream record.
    eventID :: Core.Maybe Core.Text,
    -- | The AWS service from which the stream record originated. For DynamoDB
    -- Streams, this is @aws:dynamodb@.
    eventSource :: Core.Maybe Core.Text,
    -- | The type of data modification that was performed on the DynamoDB table:
    --
    -- -   @INSERT@ - a new item was added to the table.
    --
    -- -   @MODIFY@ - one or more of an existing item\'s attributes were
    --     modified.
    --
    -- -   @REMOVE@ - the item was deleted from the table
    eventName :: Core.Maybe OperationType,
    -- | The version number of the stream record format. This number is updated
    -- whenever the structure of @Record@ is modified.
    --
    -- Client applications must not assume that @eventVersion@ will remain at a
    -- particular value, as this number is subject to change at any time. In
    -- general, @eventVersion@ will only increase as the low-level DynamoDB
    -- Streams API evolves.
    eventVersion :: Core.Maybe Core.Text,
    -- | The main body of the stream record, containing all of the
    -- DynamoDB-specific fields.
    dynamodb :: Core.Maybe StreamRecord,
    -- | The region in which the @GetRecords@ request was received.
    awsRegion :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Record' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userIdentity', 'record_userIdentity' - Items that are deleted by the Time to Live process after expiration have
-- the following fields:
--
-- -   Records[].userIdentity.type
--
--     \"Service\"
--
-- -   Records[].userIdentity.principalId
--
--     \"dynamodb.amazonaws.com\"
--
-- 'eventID', 'record_eventID' - A globally unique identifier for the event that was recorded in this
-- stream record.
--
-- 'eventSource', 'record_eventSource' - The AWS service from which the stream record originated. For DynamoDB
-- Streams, this is @aws:dynamodb@.
--
-- 'eventName', 'record_eventName' - The type of data modification that was performed on the DynamoDB table:
--
-- -   @INSERT@ - a new item was added to the table.
--
-- -   @MODIFY@ - one or more of an existing item\'s attributes were
--     modified.
--
-- -   @REMOVE@ - the item was deleted from the table
--
-- 'eventVersion', 'record_eventVersion' - The version number of the stream record format. This number is updated
-- whenever the structure of @Record@ is modified.
--
-- Client applications must not assume that @eventVersion@ will remain at a
-- particular value, as this number is subject to change at any time. In
-- general, @eventVersion@ will only increase as the low-level DynamoDB
-- Streams API evolves.
--
-- 'dynamodb', 'record_dynamodb' - The main body of the stream record, containing all of the
-- DynamoDB-specific fields.
--
-- 'awsRegion', 'record_awsRegion' - The region in which the @GetRecords@ request was received.
newRecord ::
  Record
newRecord =
  Record'
    { userIdentity = Core.Nothing,
      eventID = Core.Nothing,
      eventSource = Core.Nothing,
      eventName = Core.Nothing,
      eventVersion = Core.Nothing,
      dynamodb = Core.Nothing,
      awsRegion = Core.Nothing
    }

-- | Items that are deleted by the Time to Live process after expiration have
-- the following fields:
--
-- -   Records[].userIdentity.type
--
--     \"Service\"
--
-- -   Records[].userIdentity.principalId
--
--     \"dynamodb.amazonaws.com\"
record_userIdentity :: Lens.Lens' Record (Core.Maybe Identity)
record_userIdentity = Lens.lens (\Record' {userIdentity} -> userIdentity) (\s@Record' {} a -> s {userIdentity = a} :: Record)

-- | A globally unique identifier for the event that was recorded in this
-- stream record.
record_eventID :: Lens.Lens' Record (Core.Maybe Core.Text)
record_eventID = Lens.lens (\Record' {eventID} -> eventID) (\s@Record' {} a -> s {eventID = a} :: Record)

-- | The AWS service from which the stream record originated. For DynamoDB
-- Streams, this is @aws:dynamodb@.
record_eventSource :: Lens.Lens' Record (Core.Maybe Core.Text)
record_eventSource = Lens.lens (\Record' {eventSource} -> eventSource) (\s@Record' {} a -> s {eventSource = a} :: Record)

-- | The type of data modification that was performed on the DynamoDB table:
--
-- -   @INSERT@ - a new item was added to the table.
--
-- -   @MODIFY@ - one or more of an existing item\'s attributes were
--     modified.
--
-- -   @REMOVE@ - the item was deleted from the table
record_eventName :: Lens.Lens' Record (Core.Maybe OperationType)
record_eventName = Lens.lens (\Record' {eventName} -> eventName) (\s@Record' {} a -> s {eventName = a} :: Record)

-- | The version number of the stream record format. This number is updated
-- whenever the structure of @Record@ is modified.
--
-- Client applications must not assume that @eventVersion@ will remain at a
-- particular value, as this number is subject to change at any time. In
-- general, @eventVersion@ will only increase as the low-level DynamoDB
-- Streams API evolves.
record_eventVersion :: Lens.Lens' Record (Core.Maybe Core.Text)
record_eventVersion = Lens.lens (\Record' {eventVersion} -> eventVersion) (\s@Record' {} a -> s {eventVersion = a} :: Record)

-- | The main body of the stream record, containing all of the
-- DynamoDB-specific fields.
record_dynamodb :: Lens.Lens' Record (Core.Maybe StreamRecord)
record_dynamodb = Lens.lens (\Record' {dynamodb} -> dynamodb) (\s@Record' {} a -> s {dynamodb = a} :: Record)

-- | The region in which the @GetRecords@ request was received.
record_awsRegion :: Lens.Lens' Record (Core.Maybe Core.Text)
record_awsRegion = Lens.lens (\Record' {awsRegion} -> awsRegion) (\s@Record' {} a -> s {awsRegion = a} :: Record)

instance Core.FromJSON Record where
  parseJSON =
    Core.withObject
      "Record"
      ( \x ->
          Record'
            Core.<$> (x Core..:? "userIdentity")
            Core.<*> (x Core..:? "eventID")
            Core.<*> (x Core..:? "eventSource")
            Core.<*> (x Core..:? "eventName")
            Core.<*> (x Core..:? "eventVersion")
            Core.<*> (x Core..:? "dynamodb")
            Core.<*> (x Core..:? "awsRegion")
      )

instance Core.Hashable Record

instance Core.NFData Record
