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
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.Fragment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideoArchivedMedia.Types.Fragment where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents a segment of video or other time-delimited data.
--
-- /See:/ 'newFragment' smart constructor.
data Fragment = Fragment'
  { -- | The timestamp from the producer corresponding to the fragment.
    producerTimestamp :: Prelude.Maybe Prelude.POSIX,
    -- | The unique identifier of the fragment. This value monotonically
    -- increases based on the ingestion order.
    fragmentNumber :: Prelude.Maybe Prelude.Text,
    -- | The total fragment size, including information about the fragment and
    -- contained media data.
    fragmentSizeInBytes :: Prelude.Maybe Prelude.Integer,
    -- | The timestamp from the AWS server corresponding to the fragment.
    serverTimestamp :: Prelude.Maybe Prelude.POSIX,
    -- | The playback duration or other time value associated with the fragment.
    fragmentLengthInMilliseconds :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Fragment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'producerTimestamp', 'fragment_producerTimestamp' - The timestamp from the producer corresponding to the fragment.
--
-- 'fragmentNumber', 'fragment_fragmentNumber' - The unique identifier of the fragment. This value monotonically
-- increases based on the ingestion order.
--
-- 'fragmentSizeInBytes', 'fragment_fragmentSizeInBytes' - The total fragment size, including information about the fragment and
-- contained media data.
--
-- 'serverTimestamp', 'fragment_serverTimestamp' - The timestamp from the AWS server corresponding to the fragment.
--
-- 'fragmentLengthInMilliseconds', 'fragment_fragmentLengthInMilliseconds' - The playback duration or other time value associated with the fragment.
newFragment ::
  Fragment
newFragment =
  Fragment'
    { producerTimestamp = Prelude.Nothing,
      fragmentNumber = Prelude.Nothing,
      fragmentSizeInBytes = Prelude.Nothing,
      serverTimestamp = Prelude.Nothing,
      fragmentLengthInMilliseconds = Prelude.Nothing
    }

-- | The timestamp from the producer corresponding to the fragment.
fragment_producerTimestamp :: Lens.Lens' Fragment (Prelude.Maybe Prelude.UTCTime)
fragment_producerTimestamp = Lens.lens (\Fragment' {producerTimestamp} -> producerTimestamp) (\s@Fragment' {} a -> s {producerTimestamp = a} :: Fragment) Prelude.. Lens.mapping Prelude._Time

-- | The unique identifier of the fragment. This value monotonically
-- increases based on the ingestion order.
fragment_fragmentNumber :: Lens.Lens' Fragment (Prelude.Maybe Prelude.Text)
fragment_fragmentNumber = Lens.lens (\Fragment' {fragmentNumber} -> fragmentNumber) (\s@Fragment' {} a -> s {fragmentNumber = a} :: Fragment)

-- | The total fragment size, including information about the fragment and
-- contained media data.
fragment_fragmentSizeInBytes :: Lens.Lens' Fragment (Prelude.Maybe Prelude.Integer)
fragment_fragmentSizeInBytes = Lens.lens (\Fragment' {fragmentSizeInBytes} -> fragmentSizeInBytes) (\s@Fragment' {} a -> s {fragmentSizeInBytes = a} :: Fragment)

-- | The timestamp from the AWS server corresponding to the fragment.
fragment_serverTimestamp :: Lens.Lens' Fragment (Prelude.Maybe Prelude.UTCTime)
fragment_serverTimestamp = Lens.lens (\Fragment' {serverTimestamp} -> serverTimestamp) (\s@Fragment' {} a -> s {serverTimestamp = a} :: Fragment) Prelude.. Lens.mapping Prelude._Time

-- | The playback duration or other time value associated with the fragment.
fragment_fragmentLengthInMilliseconds :: Lens.Lens' Fragment (Prelude.Maybe Prelude.Integer)
fragment_fragmentLengthInMilliseconds = Lens.lens (\Fragment' {fragmentLengthInMilliseconds} -> fragmentLengthInMilliseconds) (\s@Fragment' {} a -> s {fragmentLengthInMilliseconds = a} :: Fragment)

instance Prelude.FromJSON Fragment where
  parseJSON =
    Prelude.withObject
      "Fragment"
      ( \x ->
          Fragment'
            Prelude.<$> (x Prelude..:? "ProducerTimestamp")
            Prelude.<*> (x Prelude..:? "FragmentNumber")
            Prelude.<*> (x Prelude..:? "FragmentSizeInBytes")
            Prelude.<*> (x Prelude..:? "ServerTimestamp")
            Prelude.<*> (x Prelude..:? "FragmentLengthInMilliseconds")
      )

instance Prelude.Hashable Fragment

instance Prelude.NFData Fragment
