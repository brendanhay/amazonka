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
-- Module      : Network.AWS.KinesisVideoMedia.Types.StartSelector
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideoMedia.Types.StartSelector where

import Network.AWS.KinesisVideoMedia.Types.StartSelectorType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Identifies the chunk on the Kinesis video stream where you want the
-- @GetMedia@ API to start returning media data. You have the following
-- options to identify the starting chunk:
--
-- -   Choose the latest (or oldest) chunk.
--
-- -   Identify a specific chunk. You can identify a specific chunk either
--     by providing a fragment number or timestamp (server or producer).
--
-- -   Each chunk\'s metadata includes a continuation token as a Matroska
--     (MKV) tag (@AWS_KINESISVIDEO_CONTINUATION_TOKEN@). If your previous
--     @GetMedia@ request terminated, you can use this tag value in your
--     next @GetMedia@ request. The API then starts returning chunks
--     starting where the last API ended.
--
-- /See:/ 'newStartSelector' smart constructor.
data StartSelector = StartSelector'
  { -- | Specifies the fragment number from where you want the @GetMedia@ API to
    -- start returning the fragments.
    afterFragmentNumber :: Prelude.Maybe Prelude.Text,
    -- | A timestamp value. This value is required if you choose the
    -- PRODUCER_TIMESTAMP or the SERVER_TIMESTAMP as the @startSelectorType@.
    -- The @GetMedia@ API then starts with the chunk containing the fragment
    -- that has the specified timestamp.
    startTimestamp :: Prelude.Maybe Prelude.POSIX,
    -- | Continuation token that Kinesis Video Streams returned in the previous
    -- @GetMedia@ response. The @GetMedia@ API then starts with the chunk
    -- identified by the continuation token.
    continuationToken :: Prelude.Maybe Prelude.Text,
    -- | Identifies the fragment on the Kinesis video stream where you want to
    -- start getting the data from.
    --
    -- -   NOW - Start with the latest chunk on the stream.
    --
    -- -   EARLIEST - Start with earliest available chunk on the stream.
    --
    -- -   FRAGMENT_NUMBER - Start with the chunk after a specific fragment.
    --     You must also specify the @AfterFragmentNumber@ parameter.
    --
    -- -   PRODUCER_TIMESTAMP or SERVER_TIMESTAMP - Start with the chunk
    --     containing a fragment with the specified producer or server
    --     timestamp. You specify the timestamp by adding @StartTimestamp@.
    --
    -- -   CONTINUATION_TOKEN - Read using the specified continuation token.
    --
    -- If you choose the NOW, EARLIEST, or CONTINUATION_TOKEN as the
    -- @startSelectorType@, you don\'t provide any additional information in
    -- the @startSelector@.
    startSelectorType :: StartSelectorType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StartSelector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'afterFragmentNumber', 'startSelector_afterFragmentNumber' - Specifies the fragment number from where you want the @GetMedia@ API to
-- start returning the fragments.
--
-- 'startTimestamp', 'startSelector_startTimestamp' - A timestamp value. This value is required if you choose the
-- PRODUCER_TIMESTAMP or the SERVER_TIMESTAMP as the @startSelectorType@.
-- The @GetMedia@ API then starts with the chunk containing the fragment
-- that has the specified timestamp.
--
-- 'continuationToken', 'startSelector_continuationToken' - Continuation token that Kinesis Video Streams returned in the previous
-- @GetMedia@ response. The @GetMedia@ API then starts with the chunk
-- identified by the continuation token.
--
-- 'startSelectorType', 'startSelector_startSelectorType' - Identifies the fragment on the Kinesis video stream where you want to
-- start getting the data from.
--
-- -   NOW - Start with the latest chunk on the stream.
--
-- -   EARLIEST - Start with earliest available chunk on the stream.
--
-- -   FRAGMENT_NUMBER - Start with the chunk after a specific fragment.
--     You must also specify the @AfterFragmentNumber@ parameter.
--
-- -   PRODUCER_TIMESTAMP or SERVER_TIMESTAMP - Start with the chunk
--     containing a fragment with the specified producer or server
--     timestamp. You specify the timestamp by adding @StartTimestamp@.
--
-- -   CONTINUATION_TOKEN - Read using the specified continuation token.
--
-- If you choose the NOW, EARLIEST, or CONTINUATION_TOKEN as the
-- @startSelectorType@, you don\'t provide any additional information in
-- the @startSelector@.
newStartSelector ::
  -- | 'startSelectorType'
  StartSelectorType ->
  StartSelector
newStartSelector pStartSelectorType_ =
  StartSelector'
    { afterFragmentNumber =
        Prelude.Nothing,
      startTimestamp = Prelude.Nothing,
      continuationToken = Prelude.Nothing,
      startSelectorType = pStartSelectorType_
    }

-- | Specifies the fragment number from where you want the @GetMedia@ API to
-- start returning the fragments.
startSelector_afterFragmentNumber :: Lens.Lens' StartSelector (Prelude.Maybe Prelude.Text)
startSelector_afterFragmentNumber = Lens.lens (\StartSelector' {afterFragmentNumber} -> afterFragmentNumber) (\s@StartSelector' {} a -> s {afterFragmentNumber = a} :: StartSelector)

-- | A timestamp value. This value is required if you choose the
-- PRODUCER_TIMESTAMP or the SERVER_TIMESTAMP as the @startSelectorType@.
-- The @GetMedia@ API then starts with the chunk containing the fragment
-- that has the specified timestamp.
startSelector_startTimestamp :: Lens.Lens' StartSelector (Prelude.Maybe Prelude.UTCTime)
startSelector_startTimestamp = Lens.lens (\StartSelector' {startTimestamp} -> startTimestamp) (\s@StartSelector' {} a -> s {startTimestamp = a} :: StartSelector) Prelude.. Lens.mapping Prelude._Time

-- | Continuation token that Kinesis Video Streams returned in the previous
-- @GetMedia@ response. The @GetMedia@ API then starts with the chunk
-- identified by the continuation token.
startSelector_continuationToken :: Lens.Lens' StartSelector (Prelude.Maybe Prelude.Text)
startSelector_continuationToken = Lens.lens (\StartSelector' {continuationToken} -> continuationToken) (\s@StartSelector' {} a -> s {continuationToken = a} :: StartSelector)

-- | Identifies the fragment on the Kinesis video stream where you want to
-- start getting the data from.
--
-- -   NOW - Start with the latest chunk on the stream.
--
-- -   EARLIEST - Start with earliest available chunk on the stream.
--
-- -   FRAGMENT_NUMBER - Start with the chunk after a specific fragment.
--     You must also specify the @AfterFragmentNumber@ parameter.
--
-- -   PRODUCER_TIMESTAMP or SERVER_TIMESTAMP - Start with the chunk
--     containing a fragment with the specified producer or server
--     timestamp. You specify the timestamp by adding @StartTimestamp@.
--
-- -   CONTINUATION_TOKEN - Read using the specified continuation token.
--
-- If you choose the NOW, EARLIEST, or CONTINUATION_TOKEN as the
-- @startSelectorType@, you don\'t provide any additional information in
-- the @startSelector@.
startSelector_startSelectorType :: Lens.Lens' StartSelector StartSelectorType
startSelector_startSelectorType = Lens.lens (\StartSelector' {startSelectorType} -> startSelectorType) (\s@StartSelector' {} a -> s {startSelectorType = a} :: StartSelector)

instance Prelude.Hashable StartSelector

instance Prelude.NFData StartSelector

instance Prelude.ToJSON StartSelector where
  toJSON StartSelector' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("AfterFragmentNumber" Prelude..=)
              Prelude.<$> afterFragmentNumber,
            ("StartTimestamp" Prelude..=)
              Prelude.<$> startTimestamp,
            ("ContinuationToken" Prelude..=)
              Prelude.<$> continuationToken,
            Prelude.Just
              ("StartSelectorType" Prelude..= startSelectorType)
          ]
      )
