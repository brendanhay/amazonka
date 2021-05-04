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
-- Module      : Network.AWS.Firehose.Types.SourceDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.SourceDescription where

import Network.AWS.Firehose.Types.KinesisStreamSourceDescription
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Details about a Kinesis data stream used as the source for a Kinesis
-- Data Firehose delivery stream.
--
-- /See:/ 'newSourceDescription' smart constructor.
data SourceDescription = SourceDescription'
  { -- | The KinesisStreamSourceDescription value for the source Kinesis data
    -- stream.
    kinesisStreamSourceDescription :: Prelude.Maybe KinesisStreamSourceDescription
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SourceDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kinesisStreamSourceDescription', 'sourceDescription_kinesisStreamSourceDescription' - The KinesisStreamSourceDescription value for the source Kinesis data
-- stream.
newSourceDescription ::
  SourceDescription
newSourceDescription =
  SourceDescription'
    { kinesisStreamSourceDescription =
        Prelude.Nothing
    }

-- | The KinesisStreamSourceDescription value for the source Kinesis data
-- stream.
sourceDescription_kinesisStreamSourceDescription :: Lens.Lens' SourceDescription (Prelude.Maybe KinesisStreamSourceDescription)
sourceDescription_kinesisStreamSourceDescription = Lens.lens (\SourceDescription' {kinesisStreamSourceDescription} -> kinesisStreamSourceDescription) (\s@SourceDescription' {} a -> s {kinesisStreamSourceDescription = a} :: SourceDescription)

instance Prelude.FromJSON SourceDescription where
  parseJSON =
    Prelude.withObject
      "SourceDescription"
      ( \x ->
          SourceDescription'
            Prelude.<$> (x Prelude..:? "KinesisStreamSourceDescription")
      )

instance Prelude.Hashable SourceDescription

instance Prelude.NFData SourceDescription
