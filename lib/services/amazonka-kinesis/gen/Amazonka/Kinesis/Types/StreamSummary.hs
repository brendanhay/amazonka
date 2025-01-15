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
-- Module      : Amazonka.Kinesis.Types.StreamSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kinesis.Types.StreamSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kinesis.Types.StreamModeDetails
import Amazonka.Kinesis.Types.StreamStatus
import qualified Amazonka.Prelude as Prelude

-- | The summary of a stream.
--
-- /See:/ 'newStreamSummary' smart constructor.
data StreamSummary = StreamSummary'
  { -- | The timestamp at which the stream was created.
    streamCreationTimestamp :: Prelude.Maybe Data.POSIX,
    streamModeDetails :: Prelude.Maybe StreamModeDetails,
    -- | The name of a stream.
    streamName :: Prelude.Text,
    -- | The ARN of the stream.
    streamARN :: Prelude.Text,
    -- | The status of the stream.
    streamStatus :: StreamStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StreamSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamCreationTimestamp', 'streamSummary_streamCreationTimestamp' - The timestamp at which the stream was created.
--
-- 'streamModeDetails', 'streamSummary_streamModeDetails' - Undocumented member.
--
-- 'streamName', 'streamSummary_streamName' - The name of a stream.
--
-- 'streamARN', 'streamSummary_streamARN' - The ARN of the stream.
--
-- 'streamStatus', 'streamSummary_streamStatus' - The status of the stream.
newStreamSummary ::
  -- | 'streamName'
  Prelude.Text ->
  -- | 'streamARN'
  Prelude.Text ->
  -- | 'streamStatus'
  StreamStatus ->
  StreamSummary
newStreamSummary
  pStreamName_
  pStreamARN_
  pStreamStatus_ =
    StreamSummary'
      { streamCreationTimestamp =
          Prelude.Nothing,
        streamModeDetails = Prelude.Nothing,
        streamName = pStreamName_,
        streamARN = pStreamARN_,
        streamStatus = pStreamStatus_
      }

-- | The timestamp at which the stream was created.
streamSummary_streamCreationTimestamp :: Lens.Lens' StreamSummary (Prelude.Maybe Prelude.UTCTime)
streamSummary_streamCreationTimestamp = Lens.lens (\StreamSummary' {streamCreationTimestamp} -> streamCreationTimestamp) (\s@StreamSummary' {} a -> s {streamCreationTimestamp = a} :: StreamSummary) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
streamSummary_streamModeDetails :: Lens.Lens' StreamSummary (Prelude.Maybe StreamModeDetails)
streamSummary_streamModeDetails = Lens.lens (\StreamSummary' {streamModeDetails} -> streamModeDetails) (\s@StreamSummary' {} a -> s {streamModeDetails = a} :: StreamSummary)

-- | The name of a stream.
streamSummary_streamName :: Lens.Lens' StreamSummary Prelude.Text
streamSummary_streamName = Lens.lens (\StreamSummary' {streamName} -> streamName) (\s@StreamSummary' {} a -> s {streamName = a} :: StreamSummary)

-- | The ARN of the stream.
streamSummary_streamARN :: Lens.Lens' StreamSummary Prelude.Text
streamSummary_streamARN = Lens.lens (\StreamSummary' {streamARN} -> streamARN) (\s@StreamSummary' {} a -> s {streamARN = a} :: StreamSummary)

-- | The status of the stream.
streamSummary_streamStatus :: Lens.Lens' StreamSummary StreamStatus
streamSummary_streamStatus = Lens.lens (\StreamSummary' {streamStatus} -> streamStatus) (\s@StreamSummary' {} a -> s {streamStatus = a} :: StreamSummary)

instance Data.FromJSON StreamSummary where
  parseJSON =
    Data.withObject
      "StreamSummary"
      ( \x ->
          StreamSummary'
            Prelude.<$> (x Data..:? "StreamCreationTimestamp")
            Prelude.<*> (x Data..:? "StreamModeDetails")
            Prelude.<*> (x Data..: "StreamName")
            Prelude.<*> (x Data..: "StreamARN")
            Prelude.<*> (x Data..: "StreamStatus")
      )

instance Prelude.Hashable StreamSummary where
  hashWithSalt _salt StreamSummary' {..} =
    _salt
      `Prelude.hashWithSalt` streamCreationTimestamp
      `Prelude.hashWithSalt` streamModeDetails
      `Prelude.hashWithSalt` streamName
      `Prelude.hashWithSalt` streamARN
      `Prelude.hashWithSalt` streamStatus

instance Prelude.NFData StreamSummary where
  rnf StreamSummary' {..} =
    Prelude.rnf streamCreationTimestamp `Prelude.seq`
      Prelude.rnf streamModeDetails `Prelude.seq`
        Prelude.rnf streamName `Prelude.seq`
          Prelude.rnf streamARN `Prelude.seq`
            Prelude.rnf streamStatus
