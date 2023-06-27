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
-- Module      : Amazonka.SecurityHub.Types.AwsStepFunctionStateMachineLoggingConfigurationDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsStepFunctionStateMachineLoggingConfigurationDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsStepFunctionStateMachineLoggingConfigurationDestinationsDetails

-- | The @LoggingConfiguration@ data type is used to set CloudWatch Logs
-- options.
--
-- /See:/ 'newAwsStepFunctionStateMachineLoggingConfigurationDetails' smart constructor.
data AwsStepFunctionStateMachineLoggingConfigurationDetails = AwsStepFunctionStateMachineLoggingConfigurationDetails'
  { -- | An array of objects that describes where your execution history events
    -- will be logged.
    destinations :: Prelude.Maybe [AwsStepFunctionStateMachineLoggingConfigurationDestinationsDetails],
    -- | Determines whether execution data is included in your log. When set to
    -- false, data is excluded.
    includeExecutionData :: Prelude.Maybe Prelude.Bool,
    -- | Defines which category of execution history events are logged.
    level :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsStepFunctionStateMachineLoggingConfigurationDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinations', 'awsStepFunctionStateMachineLoggingConfigurationDetails_destinations' - An array of objects that describes where your execution history events
-- will be logged.
--
-- 'includeExecutionData', 'awsStepFunctionStateMachineLoggingConfigurationDetails_includeExecutionData' - Determines whether execution data is included in your log. When set to
-- false, data is excluded.
--
-- 'level', 'awsStepFunctionStateMachineLoggingConfigurationDetails_level' - Defines which category of execution history events are logged.
newAwsStepFunctionStateMachineLoggingConfigurationDetails ::
  AwsStepFunctionStateMachineLoggingConfigurationDetails
newAwsStepFunctionStateMachineLoggingConfigurationDetails =
  AwsStepFunctionStateMachineLoggingConfigurationDetails'
    { destinations =
        Prelude.Nothing,
      includeExecutionData =
        Prelude.Nothing,
      level =
        Prelude.Nothing
    }

-- | An array of objects that describes where your execution history events
-- will be logged.
awsStepFunctionStateMachineLoggingConfigurationDetails_destinations :: Lens.Lens' AwsStepFunctionStateMachineLoggingConfigurationDetails (Prelude.Maybe [AwsStepFunctionStateMachineLoggingConfigurationDestinationsDetails])
awsStepFunctionStateMachineLoggingConfigurationDetails_destinations = Lens.lens (\AwsStepFunctionStateMachineLoggingConfigurationDetails' {destinations} -> destinations) (\s@AwsStepFunctionStateMachineLoggingConfigurationDetails' {} a -> s {destinations = a} :: AwsStepFunctionStateMachineLoggingConfigurationDetails) Prelude.. Lens.mapping Lens.coerced

-- | Determines whether execution data is included in your log. When set to
-- false, data is excluded.
awsStepFunctionStateMachineLoggingConfigurationDetails_includeExecutionData :: Lens.Lens' AwsStepFunctionStateMachineLoggingConfigurationDetails (Prelude.Maybe Prelude.Bool)
awsStepFunctionStateMachineLoggingConfigurationDetails_includeExecutionData = Lens.lens (\AwsStepFunctionStateMachineLoggingConfigurationDetails' {includeExecutionData} -> includeExecutionData) (\s@AwsStepFunctionStateMachineLoggingConfigurationDetails' {} a -> s {includeExecutionData = a} :: AwsStepFunctionStateMachineLoggingConfigurationDetails)

-- | Defines which category of execution history events are logged.
awsStepFunctionStateMachineLoggingConfigurationDetails_level :: Lens.Lens' AwsStepFunctionStateMachineLoggingConfigurationDetails (Prelude.Maybe Prelude.Text)
awsStepFunctionStateMachineLoggingConfigurationDetails_level = Lens.lens (\AwsStepFunctionStateMachineLoggingConfigurationDetails' {level} -> level) (\s@AwsStepFunctionStateMachineLoggingConfigurationDetails' {} a -> s {level = a} :: AwsStepFunctionStateMachineLoggingConfigurationDetails)

instance
  Data.FromJSON
    AwsStepFunctionStateMachineLoggingConfigurationDetails
  where
  parseJSON =
    Data.withObject
      "AwsStepFunctionStateMachineLoggingConfigurationDetails"
      ( \x ->
          AwsStepFunctionStateMachineLoggingConfigurationDetails'
            Prelude.<$> (x Data..:? "Destinations" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "IncludeExecutionData")
            Prelude.<*> (x Data..:? "Level")
      )

instance
  Prelude.Hashable
    AwsStepFunctionStateMachineLoggingConfigurationDetails
  where
  hashWithSalt
    _salt
    AwsStepFunctionStateMachineLoggingConfigurationDetails' {..} =
      _salt
        `Prelude.hashWithSalt` destinations
        `Prelude.hashWithSalt` includeExecutionData
        `Prelude.hashWithSalt` level

instance
  Prelude.NFData
    AwsStepFunctionStateMachineLoggingConfigurationDetails
  where
  rnf
    AwsStepFunctionStateMachineLoggingConfigurationDetails' {..} =
      Prelude.rnf destinations
        `Prelude.seq` Prelude.rnf includeExecutionData
        `Prelude.seq` Prelude.rnf level

instance
  Data.ToJSON
    AwsStepFunctionStateMachineLoggingConfigurationDetails
  where
  toJSON
    AwsStepFunctionStateMachineLoggingConfigurationDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Destinations" Data..=) Prelude.<$> destinations,
              ("IncludeExecutionData" Data..=)
                Prelude.<$> includeExecutionData,
              ("Level" Data..=) Prelude.<$> level
            ]
        )
