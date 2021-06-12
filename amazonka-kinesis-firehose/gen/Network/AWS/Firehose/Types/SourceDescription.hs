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

import qualified Network.AWS.Core as Core
import Network.AWS.Firehose.Types.KinesisStreamSourceDescription
import qualified Network.AWS.Lens as Lens

-- | Details about a Kinesis data stream used as the source for a Kinesis
-- Data Firehose delivery stream.
--
-- /See:/ 'newSourceDescription' smart constructor.
data SourceDescription = SourceDescription'
  { -- | The KinesisStreamSourceDescription value for the source Kinesis data
    -- stream.
    kinesisStreamSourceDescription :: Core.Maybe KinesisStreamSourceDescription
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing
    }

-- | The KinesisStreamSourceDescription value for the source Kinesis data
-- stream.
sourceDescription_kinesisStreamSourceDescription :: Lens.Lens' SourceDescription (Core.Maybe KinesisStreamSourceDescription)
sourceDescription_kinesisStreamSourceDescription = Lens.lens (\SourceDescription' {kinesisStreamSourceDescription} -> kinesisStreamSourceDescription) (\s@SourceDescription' {} a -> s {kinesisStreamSourceDescription = a} :: SourceDescription)

instance Core.FromJSON SourceDescription where
  parseJSON =
    Core.withObject
      "SourceDescription"
      ( \x ->
          SourceDescription'
            Core.<$> (x Core..:? "KinesisStreamSourceDescription")
      )

instance Core.Hashable SourceDescription

instance Core.NFData SourceDescription
