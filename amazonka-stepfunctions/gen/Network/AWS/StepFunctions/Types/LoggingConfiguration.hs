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
-- Module      : Network.AWS.StepFunctions.Types.LoggingConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.LoggingConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.StepFunctions.Types.LogDestination
import Network.AWS.StepFunctions.Types.LogLevel

-- | The @LoggingConfiguration@ data type is used to set CloudWatch Logs
-- options.
--
-- /See:/ 'newLoggingConfiguration' smart constructor.
data LoggingConfiguration = LoggingConfiguration'
  { -- | An array of objects that describes where your execution history events
    -- will be logged. Limited to size 1. Required, if your log level is not
    -- set to @OFF@.
    destinations :: Core.Maybe [LogDestination],
    -- | Defines which category of execution history events are logged.
    level :: Core.Maybe LogLevel,
    -- | Determines whether execution data is included in your log. When set to
    -- @false@, data is excluded.
    includeExecutionData :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LoggingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinations', 'loggingConfiguration_destinations' - An array of objects that describes where your execution history events
-- will be logged. Limited to size 1. Required, if your log level is not
-- set to @OFF@.
--
-- 'level', 'loggingConfiguration_level' - Defines which category of execution history events are logged.
--
-- 'includeExecutionData', 'loggingConfiguration_includeExecutionData' - Determines whether execution data is included in your log. When set to
-- @false@, data is excluded.
newLoggingConfiguration ::
  LoggingConfiguration
newLoggingConfiguration =
  LoggingConfiguration'
    { destinations = Core.Nothing,
      level = Core.Nothing,
      includeExecutionData = Core.Nothing
    }

-- | An array of objects that describes where your execution history events
-- will be logged. Limited to size 1. Required, if your log level is not
-- set to @OFF@.
loggingConfiguration_destinations :: Lens.Lens' LoggingConfiguration (Core.Maybe [LogDestination])
loggingConfiguration_destinations = Lens.lens (\LoggingConfiguration' {destinations} -> destinations) (\s@LoggingConfiguration' {} a -> s {destinations = a} :: LoggingConfiguration) Core.. Lens.mapping Lens._Coerce

-- | Defines which category of execution history events are logged.
loggingConfiguration_level :: Lens.Lens' LoggingConfiguration (Core.Maybe LogLevel)
loggingConfiguration_level = Lens.lens (\LoggingConfiguration' {level} -> level) (\s@LoggingConfiguration' {} a -> s {level = a} :: LoggingConfiguration)

-- | Determines whether execution data is included in your log. When set to
-- @false@, data is excluded.
loggingConfiguration_includeExecutionData :: Lens.Lens' LoggingConfiguration (Core.Maybe Core.Bool)
loggingConfiguration_includeExecutionData = Lens.lens (\LoggingConfiguration' {includeExecutionData} -> includeExecutionData) (\s@LoggingConfiguration' {} a -> s {includeExecutionData = a} :: LoggingConfiguration)

instance Core.FromJSON LoggingConfiguration where
  parseJSON =
    Core.withObject
      "LoggingConfiguration"
      ( \x ->
          LoggingConfiguration'
            Core.<$> (x Core..:? "destinations" Core..!= Core.mempty)
            Core.<*> (x Core..:? "level")
            Core.<*> (x Core..:? "includeExecutionData")
      )

instance Core.Hashable LoggingConfiguration

instance Core.NFData LoggingConfiguration

instance Core.ToJSON LoggingConfiguration where
  toJSON LoggingConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("destinations" Core..=) Core.<$> destinations,
            ("level" Core..=) Core.<$> level,
            ("includeExecutionData" Core..=)
              Core.<$> includeExecutionData
          ]
      )
