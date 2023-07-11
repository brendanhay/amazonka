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
-- Module      : Amazonka.KinesisVideoArchivedMedia.Types.Fragment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisVideoArchivedMedia.Types.Fragment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a segment of video or other time-delimited data.
--
-- /See:/ 'newFragment' smart constructor.
data Fragment = Fragment'
  { -- | The playback duration or other time value associated with the fragment.
    fragmentLengthInMilliseconds :: Prelude.Maybe Prelude.Integer,
    -- | The unique identifier of the fragment. This value monotonically
    -- increases based on the ingestion order.
    fragmentNumber :: Prelude.Maybe Prelude.Text,
    -- | The total fragment size, including information about the fragment and
    -- contained media data.
    fragmentSizeInBytes :: Prelude.Maybe Prelude.Integer,
    -- | The timestamp from the producer corresponding to the fragment.
    producerTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The timestamp from the AWS server corresponding to the fragment.
    serverTimestamp :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Fragment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fragmentLengthInMilliseconds', 'fragment_fragmentLengthInMilliseconds' - The playback duration or other time value associated with the fragment.
--
-- 'fragmentNumber', 'fragment_fragmentNumber' - The unique identifier of the fragment. This value monotonically
-- increases based on the ingestion order.
--
-- 'fragmentSizeInBytes', 'fragment_fragmentSizeInBytes' - The total fragment size, including information about the fragment and
-- contained media data.
--
-- 'producerTimestamp', 'fragment_producerTimestamp' - The timestamp from the producer corresponding to the fragment.
--
-- 'serverTimestamp', 'fragment_serverTimestamp' - The timestamp from the AWS server corresponding to the fragment.
newFragment ::
  Fragment
newFragment =
  Fragment'
    { fragmentLengthInMilliseconds =
        Prelude.Nothing,
      fragmentNumber = Prelude.Nothing,
      fragmentSizeInBytes = Prelude.Nothing,
      producerTimestamp = Prelude.Nothing,
      serverTimestamp = Prelude.Nothing
    }

-- | The playback duration or other time value associated with the fragment.
fragment_fragmentLengthInMilliseconds :: Lens.Lens' Fragment (Prelude.Maybe Prelude.Integer)
fragment_fragmentLengthInMilliseconds = Lens.lens (\Fragment' {fragmentLengthInMilliseconds} -> fragmentLengthInMilliseconds) (\s@Fragment' {} a -> s {fragmentLengthInMilliseconds = a} :: Fragment)

-- | The unique identifier of the fragment. This value monotonically
-- increases based on the ingestion order.
fragment_fragmentNumber :: Lens.Lens' Fragment (Prelude.Maybe Prelude.Text)
fragment_fragmentNumber = Lens.lens (\Fragment' {fragmentNumber} -> fragmentNumber) (\s@Fragment' {} a -> s {fragmentNumber = a} :: Fragment)

-- | The total fragment size, including information about the fragment and
-- contained media data.
fragment_fragmentSizeInBytes :: Lens.Lens' Fragment (Prelude.Maybe Prelude.Integer)
fragment_fragmentSizeInBytes = Lens.lens (\Fragment' {fragmentSizeInBytes} -> fragmentSizeInBytes) (\s@Fragment' {} a -> s {fragmentSizeInBytes = a} :: Fragment)

-- | The timestamp from the producer corresponding to the fragment.
fragment_producerTimestamp :: Lens.Lens' Fragment (Prelude.Maybe Prelude.UTCTime)
fragment_producerTimestamp = Lens.lens (\Fragment' {producerTimestamp} -> producerTimestamp) (\s@Fragment' {} a -> s {producerTimestamp = a} :: Fragment) Prelude.. Lens.mapping Data._Time

-- | The timestamp from the AWS server corresponding to the fragment.
fragment_serverTimestamp :: Lens.Lens' Fragment (Prelude.Maybe Prelude.UTCTime)
fragment_serverTimestamp = Lens.lens (\Fragment' {serverTimestamp} -> serverTimestamp) (\s@Fragment' {} a -> s {serverTimestamp = a} :: Fragment) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON Fragment where
  parseJSON =
    Data.withObject
      "Fragment"
      ( \x ->
          Fragment'
            Prelude.<$> (x Data..:? "FragmentLengthInMilliseconds")
            Prelude.<*> (x Data..:? "FragmentNumber")
            Prelude.<*> (x Data..:? "FragmentSizeInBytes")
            Prelude.<*> (x Data..:? "ProducerTimestamp")
            Prelude.<*> (x Data..:? "ServerTimestamp")
      )

instance Prelude.Hashable Fragment where
  hashWithSalt _salt Fragment' {..} =
    _salt
      `Prelude.hashWithSalt` fragmentLengthInMilliseconds
      `Prelude.hashWithSalt` fragmentNumber
      `Prelude.hashWithSalt` fragmentSizeInBytes
      `Prelude.hashWithSalt` producerTimestamp
      `Prelude.hashWithSalt` serverTimestamp

instance Prelude.NFData Fragment where
  rnf Fragment' {..} =
    Prelude.rnf fragmentLengthInMilliseconds
      `Prelude.seq` Prelude.rnf fragmentNumber
      `Prelude.seq` Prelude.rnf fragmentSizeInBytes
      `Prelude.seq` Prelude.rnf producerTimestamp
      `Prelude.seq` Prelude.rnf serverTimestamp
