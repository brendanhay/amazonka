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
-- Module      : Network.AWS.StepFunctions.Types.LoggingConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.LoggingConfiguration where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    destinations :: Prelude.Maybe [LogDestination],
    -- | Defines which category of execution history events are logged.
    level :: Prelude.Maybe LogLevel,
    -- | Determines whether execution data is included in your log. When set to
    -- @false@, data is excluded.
    includeExecutionData :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { destinations =
        Prelude.Nothing,
      level = Prelude.Nothing,
      includeExecutionData = Prelude.Nothing
    }

-- | An array of objects that describes where your execution history events
-- will be logged. Limited to size 1. Required, if your log level is not
-- set to @OFF@.
loggingConfiguration_destinations :: Lens.Lens' LoggingConfiguration (Prelude.Maybe [LogDestination])
loggingConfiguration_destinations = Lens.lens (\LoggingConfiguration' {destinations} -> destinations) (\s@LoggingConfiguration' {} a -> s {destinations = a} :: LoggingConfiguration) Prelude.. Lens.mapping Prelude._Coerce

-- | Defines which category of execution history events are logged.
loggingConfiguration_level :: Lens.Lens' LoggingConfiguration (Prelude.Maybe LogLevel)
loggingConfiguration_level = Lens.lens (\LoggingConfiguration' {level} -> level) (\s@LoggingConfiguration' {} a -> s {level = a} :: LoggingConfiguration)

-- | Determines whether execution data is included in your log. When set to
-- @false@, data is excluded.
loggingConfiguration_includeExecutionData :: Lens.Lens' LoggingConfiguration (Prelude.Maybe Prelude.Bool)
loggingConfiguration_includeExecutionData = Lens.lens (\LoggingConfiguration' {includeExecutionData} -> includeExecutionData) (\s@LoggingConfiguration' {} a -> s {includeExecutionData = a} :: LoggingConfiguration)

instance Prelude.FromJSON LoggingConfiguration where
  parseJSON =
    Prelude.withObject
      "LoggingConfiguration"
      ( \x ->
          LoggingConfiguration'
            Prelude.<$> ( x Prelude..:? "destinations"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "level")
            Prelude.<*> (x Prelude..:? "includeExecutionData")
      )

instance Prelude.Hashable LoggingConfiguration

instance Prelude.NFData LoggingConfiguration

instance Prelude.ToJSON LoggingConfiguration where
  toJSON LoggingConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("destinations" Prelude..=)
              Prelude.<$> destinations,
            ("level" Prelude..=) Prelude.<$> level,
            ("includeExecutionData" Prelude..=)
              Prelude.<$> includeExecutionData
          ]
      )
