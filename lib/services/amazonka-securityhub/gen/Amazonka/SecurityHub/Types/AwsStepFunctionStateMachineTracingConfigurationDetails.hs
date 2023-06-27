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
-- Module      : Amazonka.SecurityHub.Types.AwsStepFunctionStateMachineTracingConfigurationDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsStepFunctionStateMachineTracingConfigurationDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies whether X-Ray tracing is enabled.
--
-- /See:/ 'newAwsStepFunctionStateMachineTracingConfigurationDetails' smart constructor.
data AwsStepFunctionStateMachineTracingConfigurationDetails = AwsStepFunctionStateMachineTracingConfigurationDetails'
  { -- | When set to true, X-Ray tracing is enabled.
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsStepFunctionStateMachineTracingConfigurationDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'awsStepFunctionStateMachineTracingConfigurationDetails_enabled' - When set to true, X-Ray tracing is enabled.
newAwsStepFunctionStateMachineTracingConfigurationDetails ::
  AwsStepFunctionStateMachineTracingConfigurationDetails
newAwsStepFunctionStateMachineTracingConfigurationDetails =
  AwsStepFunctionStateMachineTracingConfigurationDetails'
    { enabled =
        Prelude.Nothing
    }

-- | When set to true, X-Ray tracing is enabled.
awsStepFunctionStateMachineTracingConfigurationDetails_enabled :: Lens.Lens' AwsStepFunctionStateMachineTracingConfigurationDetails (Prelude.Maybe Prelude.Bool)
awsStepFunctionStateMachineTracingConfigurationDetails_enabled = Lens.lens (\AwsStepFunctionStateMachineTracingConfigurationDetails' {enabled} -> enabled) (\s@AwsStepFunctionStateMachineTracingConfigurationDetails' {} a -> s {enabled = a} :: AwsStepFunctionStateMachineTracingConfigurationDetails)

instance
  Data.FromJSON
    AwsStepFunctionStateMachineTracingConfigurationDetails
  where
  parseJSON =
    Data.withObject
      "AwsStepFunctionStateMachineTracingConfigurationDetails"
      ( \x ->
          AwsStepFunctionStateMachineTracingConfigurationDetails'
            Prelude.<$> (x Data..:? "Enabled")
      )

instance
  Prelude.Hashable
    AwsStepFunctionStateMachineTracingConfigurationDetails
  where
  hashWithSalt
    _salt
    AwsStepFunctionStateMachineTracingConfigurationDetails' {..} =
      _salt `Prelude.hashWithSalt` enabled

instance
  Prelude.NFData
    AwsStepFunctionStateMachineTracingConfigurationDetails
  where
  rnf
    AwsStepFunctionStateMachineTracingConfigurationDetails' {..} =
      Prelude.rnf enabled

instance
  Data.ToJSON
    AwsStepFunctionStateMachineTracingConfigurationDetails
  where
  toJSON
    AwsStepFunctionStateMachineTracingConfigurationDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [("Enabled" Data..=) Prelude.<$> enabled]
        )
