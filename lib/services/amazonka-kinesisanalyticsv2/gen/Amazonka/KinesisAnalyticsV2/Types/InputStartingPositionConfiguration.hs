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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.InputStartingPositionConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.InputStartingPositionConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.KinesisAnalyticsV2.Types.InputStartingPosition
import qualified Amazonka.Prelude as Prelude

-- | Describes the point at which the application reads from the streaming
-- source.
--
-- /See:/ 'newInputStartingPositionConfiguration' smart constructor.
data InputStartingPositionConfiguration = InputStartingPositionConfiguration'
  { -- | The starting position on the stream.
    --
    -- -   @NOW@ - Start reading just after the most recent record in the
    --     stream, and start at the request timestamp that the customer issued.
    --
    -- -   @TRIM_HORIZON@ - Start reading at the last untrimmed record in the
    --     stream, which is the oldest record available in the stream. This
    --     option is not available for an Amazon Kinesis Data Firehose delivery
    --     stream.
    --
    -- -   @LAST_STOPPED_POINT@ - Resume reading from where the application
    --     last stopped reading.
    inputStartingPosition :: Prelude.Maybe InputStartingPosition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputStartingPositionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputStartingPosition', 'inputStartingPositionConfiguration_inputStartingPosition' - The starting position on the stream.
--
-- -   @NOW@ - Start reading just after the most recent record in the
--     stream, and start at the request timestamp that the customer issued.
--
-- -   @TRIM_HORIZON@ - Start reading at the last untrimmed record in the
--     stream, which is the oldest record available in the stream. This
--     option is not available for an Amazon Kinesis Data Firehose delivery
--     stream.
--
-- -   @LAST_STOPPED_POINT@ - Resume reading from where the application
--     last stopped reading.
newInputStartingPositionConfiguration ::
  InputStartingPositionConfiguration
newInputStartingPositionConfiguration =
  InputStartingPositionConfiguration'
    { inputStartingPosition =
        Prelude.Nothing
    }

-- | The starting position on the stream.
--
-- -   @NOW@ - Start reading just after the most recent record in the
--     stream, and start at the request timestamp that the customer issued.
--
-- -   @TRIM_HORIZON@ - Start reading at the last untrimmed record in the
--     stream, which is the oldest record available in the stream. This
--     option is not available for an Amazon Kinesis Data Firehose delivery
--     stream.
--
-- -   @LAST_STOPPED_POINT@ - Resume reading from where the application
--     last stopped reading.
inputStartingPositionConfiguration_inputStartingPosition :: Lens.Lens' InputStartingPositionConfiguration (Prelude.Maybe InputStartingPosition)
inputStartingPositionConfiguration_inputStartingPosition = Lens.lens (\InputStartingPositionConfiguration' {inputStartingPosition} -> inputStartingPosition) (\s@InputStartingPositionConfiguration' {} a -> s {inputStartingPosition = a} :: InputStartingPositionConfiguration)

instance
  Core.FromJSON
    InputStartingPositionConfiguration
  where
  parseJSON =
    Core.withObject
      "InputStartingPositionConfiguration"
      ( \x ->
          InputStartingPositionConfiguration'
            Prelude.<$> (x Core..:? "InputStartingPosition")
      )

instance
  Prelude.Hashable
    InputStartingPositionConfiguration
  where
  hashWithSalt
    _salt
    InputStartingPositionConfiguration' {..} =
      _salt `Prelude.hashWithSalt` inputStartingPosition

instance
  Prelude.NFData
    InputStartingPositionConfiguration
  where
  rnf InputStartingPositionConfiguration' {..} =
    Prelude.rnf inputStartingPosition

instance
  Core.ToJSON
    InputStartingPositionConfiguration
  where
  toJSON InputStartingPositionConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("InputStartingPosition" Core..=)
              Prelude.<$> inputStartingPosition
          ]
      )
