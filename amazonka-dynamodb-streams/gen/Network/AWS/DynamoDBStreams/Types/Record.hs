{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.DynamoDBStreams.Types.Identity
import Network.AWS.DynamoDBStreams.Types.OperationType
import Network.AWS.DynamoDBStreams.Types.StreamRecord
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
    userIdentity :: Prelude.Maybe Identity,
    -- | A globally unique identifier for the event that was recorded in this
    -- stream record.
    eventID :: Prelude.Maybe Prelude.Text,
    -- | The AWS service from which the stream record originated. For DynamoDB
    -- Streams, this is @aws:dynamodb@.
    eventSource :: Prelude.Maybe Prelude.Text,
    -- | The type of data modification that was performed on the DynamoDB table:
    --
    -- -   @INSERT@ - a new item was added to the table.
    --
    -- -   @MODIFY@ - one or more of an existing item\'s attributes were
    --     modified.
    --
    -- -   @REMOVE@ - the item was deleted from the table
    eventName :: Prelude.Maybe OperationType,
    -- | The version number of the stream record format. This number is updated
    -- whenever the structure of @Record@ is modified.
    --
    -- Client applications must not assume that @eventVersion@ will remain at a
    -- particular value, as this number is subject to change at any time. In
    -- general, @eventVersion@ will only increase as the low-level DynamoDB
    -- Streams API evolves.
    eventVersion :: Prelude.Maybe Prelude.Text,
    -- | The main body of the stream record, containing all of the
    -- DynamoDB-specific fields.
    dynamodb :: Prelude.Maybe StreamRecord,
    -- | The region in which the @GetRecords@ request was received.
    awsRegion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { userIdentity = Prelude.Nothing,
      eventID = Prelude.Nothing,
      eventSource = Prelude.Nothing,
      eventName = Prelude.Nothing,
      eventVersion = Prelude.Nothing,
      dynamodb = Prelude.Nothing,
      awsRegion = Prelude.Nothing
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
record_userIdentity :: Lens.Lens' Record (Prelude.Maybe Identity)
record_userIdentity = Lens.lens (\Record' {userIdentity} -> userIdentity) (\s@Record' {} a -> s {userIdentity = a} :: Record)

-- | A globally unique identifier for the event that was recorded in this
-- stream record.
record_eventID :: Lens.Lens' Record (Prelude.Maybe Prelude.Text)
record_eventID = Lens.lens (\Record' {eventID} -> eventID) (\s@Record' {} a -> s {eventID = a} :: Record)

-- | The AWS service from which the stream record originated. For DynamoDB
-- Streams, this is @aws:dynamodb@.
record_eventSource :: Lens.Lens' Record (Prelude.Maybe Prelude.Text)
record_eventSource = Lens.lens (\Record' {eventSource} -> eventSource) (\s@Record' {} a -> s {eventSource = a} :: Record)

-- | The type of data modification that was performed on the DynamoDB table:
--
-- -   @INSERT@ - a new item was added to the table.
--
-- -   @MODIFY@ - one or more of an existing item\'s attributes were
--     modified.
--
-- -   @REMOVE@ - the item was deleted from the table
record_eventName :: Lens.Lens' Record (Prelude.Maybe OperationType)
record_eventName = Lens.lens (\Record' {eventName} -> eventName) (\s@Record' {} a -> s {eventName = a} :: Record)

-- | The version number of the stream record format. This number is updated
-- whenever the structure of @Record@ is modified.
--
-- Client applications must not assume that @eventVersion@ will remain at a
-- particular value, as this number is subject to change at any time. In
-- general, @eventVersion@ will only increase as the low-level DynamoDB
-- Streams API evolves.
record_eventVersion :: Lens.Lens' Record (Prelude.Maybe Prelude.Text)
record_eventVersion = Lens.lens (\Record' {eventVersion} -> eventVersion) (\s@Record' {} a -> s {eventVersion = a} :: Record)

-- | The main body of the stream record, containing all of the
-- DynamoDB-specific fields.
record_dynamodb :: Lens.Lens' Record (Prelude.Maybe StreamRecord)
record_dynamodb = Lens.lens (\Record' {dynamodb} -> dynamodb) (\s@Record' {} a -> s {dynamodb = a} :: Record)

-- | The region in which the @GetRecords@ request was received.
record_awsRegion :: Lens.Lens' Record (Prelude.Maybe Prelude.Text)
record_awsRegion = Lens.lens (\Record' {awsRegion} -> awsRegion) (\s@Record' {} a -> s {awsRegion = a} :: Record)

instance Prelude.FromJSON Record where
  parseJSON =
    Prelude.withObject
      "Record"
      ( \x ->
          Record'
            Prelude.<$> (x Prelude..:? "userIdentity")
            Prelude.<*> (x Prelude..:? "eventID")
            Prelude.<*> (x Prelude..:? "eventSource")
            Prelude.<*> (x Prelude..:? "eventName")
            Prelude.<*> (x Prelude..:? "eventVersion")
            Prelude.<*> (x Prelude..:? "dynamodb")
            Prelude.<*> (x Prelude..:? "awsRegion")
      )

instance Prelude.Hashable Record

instance Prelude.NFData Record
