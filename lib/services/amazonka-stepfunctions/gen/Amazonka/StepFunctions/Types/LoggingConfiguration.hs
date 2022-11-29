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
-- Module      : Amazonka.StepFunctions.Types.LoggingConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StepFunctions.Types.LoggingConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.StepFunctions.Types.LogDestination
import Amazonka.StepFunctions.Types.LogLevel

-- | The @LoggingConfiguration@ data type is used to set CloudWatch Logs
-- options.
--
-- /See:/ 'newLoggingConfiguration' smart constructor.
data LoggingConfiguration = LoggingConfiguration'
  { -- | Determines whether execution data is included in your log. When set to
    -- @false@, data is excluded.
    includeExecutionData :: Prelude.Maybe Prelude.Bool,
    -- | Defines which category of execution history events are logged.
    level :: Prelude.Maybe LogLevel,
    -- | An array of objects that describes where your execution history events
    -- will be logged. Limited to size 1. Required, if your log level is not
    -- set to @OFF@.
    destinations :: Prelude.Maybe [LogDestination]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoggingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includeExecutionData', 'loggingConfiguration_includeExecutionData' - Determines whether execution data is included in your log. When set to
-- @false@, data is excluded.
--
-- 'level', 'loggingConfiguration_level' - Defines which category of execution history events are logged.
--
-- 'destinations', 'loggingConfiguration_destinations' - An array of objects that describes where your execution history events
-- will be logged. Limited to size 1. Required, if your log level is not
-- set to @OFF@.
newLoggingConfiguration ::
  LoggingConfiguration
newLoggingConfiguration =
  LoggingConfiguration'
    { includeExecutionData =
        Prelude.Nothing,
      level = Prelude.Nothing,
      destinations = Prelude.Nothing
    }

-- | Determines whether execution data is included in your log. When set to
-- @false@, data is excluded.
loggingConfiguration_includeExecutionData :: Lens.Lens' LoggingConfiguration (Prelude.Maybe Prelude.Bool)
loggingConfiguration_includeExecutionData = Lens.lens (\LoggingConfiguration' {includeExecutionData} -> includeExecutionData) (\s@LoggingConfiguration' {} a -> s {includeExecutionData = a} :: LoggingConfiguration)

-- | Defines which category of execution history events are logged.
loggingConfiguration_level :: Lens.Lens' LoggingConfiguration (Prelude.Maybe LogLevel)
loggingConfiguration_level = Lens.lens (\LoggingConfiguration' {level} -> level) (\s@LoggingConfiguration' {} a -> s {level = a} :: LoggingConfiguration)

-- | An array of objects that describes where your execution history events
-- will be logged. Limited to size 1. Required, if your log level is not
-- set to @OFF@.
loggingConfiguration_destinations :: Lens.Lens' LoggingConfiguration (Prelude.Maybe [LogDestination])
loggingConfiguration_destinations = Lens.lens (\LoggingConfiguration' {destinations} -> destinations) (\s@LoggingConfiguration' {} a -> s {destinations = a} :: LoggingConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON LoggingConfiguration where
  parseJSON =
    Core.withObject
      "LoggingConfiguration"
      ( \x ->
          LoggingConfiguration'
            Prelude.<$> (x Core..:? "includeExecutionData")
            Prelude.<*> (x Core..:? "level")
            Prelude.<*> (x Core..:? "destinations" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable LoggingConfiguration where
  hashWithSalt _salt LoggingConfiguration' {..} =
    _salt `Prelude.hashWithSalt` includeExecutionData
      `Prelude.hashWithSalt` level
      `Prelude.hashWithSalt` destinations

instance Prelude.NFData LoggingConfiguration where
  rnf LoggingConfiguration' {..} =
    Prelude.rnf includeExecutionData
      `Prelude.seq` Prelude.rnf level
      `Prelude.seq` Prelude.rnf destinations

instance Core.ToJSON LoggingConfiguration where
  toJSON LoggingConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("includeExecutionData" Core..=)
              Prelude.<$> includeExecutionData,
            ("level" Core..=) Prelude.<$> level,
            ("destinations" Core..=) Prelude.<$> destinations
          ]
      )
