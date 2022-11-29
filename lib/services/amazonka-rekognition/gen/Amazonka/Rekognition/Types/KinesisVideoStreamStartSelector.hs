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
-- Module      : Amazonka.Rekognition.Types.KinesisVideoStreamStartSelector
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.KinesisVideoStreamStartSelector where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies the starting point in a Kinesis stream to start processing.
-- You can use the producer timestamp or the fragment number. One of either
-- producer timestamp or fragment number is required. If you use the
-- producer timestamp, you must put the time in milliseconds. For more
-- information about fragment numbers, see
-- <https://docs.aws.amazon.com/kinesisvideostreams/latest/dg/API_reader_Fragment.html Fragment>.
--
-- /See:/ 'newKinesisVideoStreamStartSelector' smart constructor.
data KinesisVideoStreamStartSelector = KinesisVideoStreamStartSelector'
  { -- | The timestamp from the producer corresponding to the fragment, in
    -- milliseconds, expressed in unix time format.
    producerTimestamp :: Prelude.Maybe Prelude.Natural,
    -- | The unique identifier of the fragment. This value monotonically
    -- increases based on the ingestion order.
    fragmentNumber :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KinesisVideoStreamStartSelector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'producerTimestamp', 'kinesisVideoStreamStartSelector_producerTimestamp' - The timestamp from the producer corresponding to the fragment, in
-- milliseconds, expressed in unix time format.
--
-- 'fragmentNumber', 'kinesisVideoStreamStartSelector_fragmentNumber' - The unique identifier of the fragment. This value monotonically
-- increases based on the ingestion order.
newKinesisVideoStreamStartSelector ::
  KinesisVideoStreamStartSelector
newKinesisVideoStreamStartSelector =
  KinesisVideoStreamStartSelector'
    { producerTimestamp =
        Prelude.Nothing,
      fragmentNumber = Prelude.Nothing
    }

-- | The timestamp from the producer corresponding to the fragment, in
-- milliseconds, expressed in unix time format.
kinesisVideoStreamStartSelector_producerTimestamp :: Lens.Lens' KinesisVideoStreamStartSelector (Prelude.Maybe Prelude.Natural)
kinesisVideoStreamStartSelector_producerTimestamp = Lens.lens (\KinesisVideoStreamStartSelector' {producerTimestamp} -> producerTimestamp) (\s@KinesisVideoStreamStartSelector' {} a -> s {producerTimestamp = a} :: KinesisVideoStreamStartSelector)

-- | The unique identifier of the fragment. This value monotonically
-- increases based on the ingestion order.
kinesisVideoStreamStartSelector_fragmentNumber :: Lens.Lens' KinesisVideoStreamStartSelector (Prelude.Maybe Prelude.Text)
kinesisVideoStreamStartSelector_fragmentNumber = Lens.lens (\KinesisVideoStreamStartSelector' {fragmentNumber} -> fragmentNumber) (\s@KinesisVideoStreamStartSelector' {} a -> s {fragmentNumber = a} :: KinesisVideoStreamStartSelector)

instance
  Prelude.Hashable
    KinesisVideoStreamStartSelector
  where
  hashWithSalt
    _salt
    KinesisVideoStreamStartSelector' {..} =
      _salt `Prelude.hashWithSalt` producerTimestamp
        `Prelude.hashWithSalt` fragmentNumber

instance
  Prelude.NFData
    KinesisVideoStreamStartSelector
  where
  rnf KinesisVideoStreamStartSelector' {..} =
    Prelude.rnf producerTimestamp
      `Prelude.seq` Prelude.rnf fragmentNumber

instance Core.ToJSON KinesisVideoStreamStartSelector where
  toJSON KinesisVideoStreamStartSelector' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ProducerTimestamp" Core..=)
              Prelude.<$> producerTimestamp,
            ("FragmentNumber" Core..=)
              Prelude.<$> fragmentNumber
          ]
      )
