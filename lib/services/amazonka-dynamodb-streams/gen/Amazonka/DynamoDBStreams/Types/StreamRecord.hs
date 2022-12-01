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
-- Module      : Amazonka.DynamoDBStreams.Types.StreamRecord
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDBStreams.Types.StreamRecord where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DynamoDBStreams.Internal
import Amazonka.DynamoDBStreams.Types.StreamViewType
import qualified Amazonka.Prelude as Prelude

-- | A description of a single data modification that was performed on an
-- item in a DynamoDB table.
--
-- /See:/ 'newStreamRecord' smart constructor.
data StreamRecord = StreamRecord'
  { -- | The size of the stream record, in bytes.
    sizeBytes :: Prelude.Maybe Prelude.Natural,
    -- | The type of data from the modified DynamoDB item that was captured in
    -- this stream record:
    --
    -- -   @KEYS_ONLY@ - only the key attributes of the modified item.
    --
    -- -   @NEW_IMAGE@ - the entire item, as it appeared after it was modified.
    --
    -- -   @OLD_IMAGE@ - the entire item, as it appeared before it was
    --     modified.
    --
    -- -   @NEW_AND_OLD_IMAGES@ - both the new and the old item images of the
    --     item.
    streamViewType :: Prelude.Maybe StreamViewType,
    -- | The item in the DynamoDB table as it appeared before it was modified.
    oldImage :: Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeValue),
    -- | The approximate date and time when the stream record was created, in
    -- <http://www.epochconverter.com/ UNIX epoch time> format.
    approximateCreationDateTime :: Prelude.Maybe Core.POSIX,
    -- | The sequence number of the stream record.
    sequenceNumber :: Prelude.Maybe Prelude.Text,
    -- | The primary key attribute(s) for the DynamoDB item that was modified.
    keys :: Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeValue),
    -- | The item in the DynamoDB table as it appeared after it was modified.
    newImage' :: Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeValue)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StreamRecord' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sizeBytes', 'streamRecord_sizeBytes' - The size of the stream record, in bytes.
--
-- 'streamViewType', 'streamRecord_streamViewType' - The type of data from the modified DynamoDB item that was captured in
-- this stream record:
--
-- -   @KEYS_ONLY@ - only the key attributes of the modified item.
--
-- -   @NEW_IMAGE@ - the entire item, as it appeared after it was modified.
--
-- -   @OLD_IMAGE@ - the entire item, as it appeared before it was
--     modified.
--
-- -   @NEW_AND_OLD_IMAGES@ - both the new and the old item images of the
--     item.
--
-- 'oldImage', 'streamRecord_oldImage' - The item in the DynamoDB table as it appeared before it was modified.
--
-- 'approximateCreationDateTime', 'streamRecord_approximateCreationDateTime' - The approximate date and time when the stream record was created, in
-- <http://www.epochconverter.com/ UNIX epoch time> format.
--
-- 'sequenceNumber', 'streamRecord_sequenceNumber' - The sequence number of the stream record.
--
-- 'keys', 'streamRecord_keys' - The primary key attribute(s) for the DynamoDB item that was modified.
--
-- 'newImage'', 'streamRecord_newImage' - The item in the DynamoDB table as it appeared after it was modified.
newStreamRecord ::
  StreamRecord
newStreamRecord =
  StreamRecord'
    { sizeBytes = Prelude.Nothing,
      streamViewType = Prelude.Nothing,
      oldImage = Prelude.Nothing,
      approximateCreationDateTime = Prelude.Nothing,
      sequenceNumber = Prelude.Nothing,
      keys = Prelude.Nothing,
      newImage' = Prelude.Nothing
    }

-- | The size of the stream record, in bytes.
streamRecord_sizeBytes :: Lens.Lens' StreamRecord (Prelude.Maybe Prelude.Natural)
streamRecord_sizeBytes = Lens.lens (\StreamRecord' {sizeBytes} -> sizeBytes) (\s@StreamRecord' {} a -> s {sizeBytes = a} :: StreamRecord)

-- | The type of data from the modified DynamoDB item that was captured in
-- this stream record:
--
-- -   @KEYS_ONLY@ - only the key attributes of the modified item.
--
-- -   @NEW_IMAGE@ - the entire item, as it appeared after it was modified.
--
-- -   @OLD_IMAGE@ - the entire item, as it appeared before it was
--     modified.
--
-- -   @NEW_AND_OLD_IMAGES@ - both the new and the old item images of the
--     item.
streamRecord_streamViewType :: Lens.Lens' StreamRecord (Prelude.Maybe StreamViewType)
streamRecord_streamViewType = Lens.lens (\StreamRecord' {streamViewType} -> streamViewType) (\s@StreamRecord' {} a -> s {streamViewType = a} :: StreamRecord)

-- | The item in the DynamoDB table as it appeared before it was modified.
streamRecord_oldImage :: Lens.Lens' StreamRecord (Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeValue))
streamRecord_oldImage = Lens.lens (\StreamRecord' {oldImage} -> oldImage) (\s@StreamRecord' {} a -> s {oldImage = a} :: StreamRecord) Prelude.. Lens.mapping Lens.coerced

-- | The approximate date and time when the stream record was created, in
-- <http://www.epochconverter.com/ UNIX epoch time> format.
streamRecord_approximateCreationDateTime :: Lens.Lens' StreamRecord (Prelude.Maybe Prelude.UTCTime)
streamRecord_approximateCreationDateTime = Lens.lens (\StreamRecord' {approximateCreationDateTime} -> approximateCreationDateTime) (\s@StreamRecord' {} a -> s {approximateCreationDateTime = a} :: StreamRecord) Prelude.. Lens.mapping Core._Time

-- | The sequence number of the stream record.
streamRecord_sequenceNumber :: Lens.Lens' StreamRecord (Prelude.Maybe Prelude.Text)
streamRecord_sequenceNumber = Lens.lens (\StreamRecord' {sequenceNumber} -> sequenceNumber) (\s@StreamRecord' {} a -> s {sequenceNumber = a} :: StreamRecord)

-- | The primary key attribute(s) for the DynamoDB item that was modified.
streamRecord_keys :: Lens.Lens' StreamRecord (Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeValue))
streamRecord_keys = Lens.lens (\StreamRecord' {keys} -> keys) (\s@StreamRecord' {} a -> s {keys = a} :: StreamRecord) Prelude.. Lens.mapping Lens.coerced

-- | The item in the DynamoDB table as it appeared after it was modified.
streamRecord_newImage :: Lens.Lens' StreamRecord (Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeValue))
streamRecord_newImage = Lens.lens (\StreamRecord' {newImage'} -> newImage') (\s@StreamRecord' {} a -> s {newImage' = a} :: StreamRecord) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON StreamRecord where
  parseJSON =
    Core.withObject
      "StreamRecord"
      ( \x ->
          StreamRecord'
            Prelude.<$> (x Core..:? "SizeBytes")
            Prelude.<*> (x Core..:? "StreamViewType")
            Prelude.<*> (x Core..:? "OldImage" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ApproximateCreationDateTime")
            Prelude.<*> (x Core..:? "SequenceNumber")
            Prelude.<*> (x Core..:? "Keys" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "NewImage" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable StreamRecord where
  hashWithSalt _salt StreamRecord' {..} =
    _salt `Prelude.hashWithSalt` sizeBytes
      `Prelude.hashWithSalt` streamViewType
      `Prelude.hashWithSalt` oldImage
      `Prelude.hashWithSalt` approximateCreationDateTime
      `Prelude.hashWithSalt` sequenceNumber
      `Prelude.hashWithSalt` keys
      `Prelude.hashWithSalt` newImage'

instance Prelude.NFData StreamRecord where
  rnf StreamRecord' {..} =
    Prelude.rnf sizeBytes
      `Prelude.seq` Prelude.rnf streamViewType
      `Prelude.seq` Prelude.rnf oldImage
      `Prelude.seq` Prelude.rnf approximateCreationDateTime
      `Prelude.seq` Prelude.rnf sequenceNumber
      `Prelude.seq` Prelude.rnf keys
      `Prelude.seq` Prelude.rnf newImage'
