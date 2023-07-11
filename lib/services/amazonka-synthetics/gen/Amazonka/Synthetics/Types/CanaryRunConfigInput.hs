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
-- Module      : Amazonka.Synthetics.Types.CanaryRunConfigInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Synthetics.Types.CanaryRunConfigInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains input information for a canary run.
--
-- /See:/ 'newCanaryRunConfigInput' smart constructor.
data CanaryRunConfigInput = CanaryRunConfigInput'
  { -- | Specifies whether this canary is to use active X-Ray tracing when it
    -- runs. Active tracing enables this canary run to be displayed in the
    -- ServiceLens and X-Ray service maps even if the canary does not hit an
    -- endpoint that has X-Ray tracing enabled. Using X-Ray tracing incurs
    -- charges. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch_Synthetics_Canaries_tracing.html Canaries and X-Ray tracing>.
    --
    -- You can enable active tracing only for canaries that use version
    -- @syn-nodejs-2.0@ or later for their canary runtime.
    activeTracing :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the keys and values to use for any environment variables used
    -- in the canary script. Use the following format:
    --
    -- { \"key1\" : \"value1\", \"key2\" : \"value2\", ...}
    --
    -- Keys must start with a letter and be at least two characters. The total
    -- size of your environment variables cannot exceed 4 KB. You can\'t
    -- specify any Lambda reserved environment variables as the keys for your
    -- environment variables. For more information about reserved keys, see
    -- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-envvars.html#configuration-envvars-runtime Runtime environment variables>.
    --
    -- The environment variables keys and values are not encrypted. Do not
    -- store sensitive information in this field.
    environmentVariables :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The maximum amount of memory available to the canary while it is
    -- running, in MB. This value must be a multiple of 64.
    memoryInMB :: Prelude.Maybe Prelude.Natural,
    -- | How long the canary is allowed to run before it must stop. You can\'t
    -- set this time to be longer than the frequency of the runs of this
    -- canary.
    --
    -- If you omit this field, the frequency of the canary is used as this
    -- value, up to a maximum of 14 minutes.
    timeoutInSeconds :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CanaryRunConfigInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeTracing', 'canaryRunConfigInput_activeTracing' - Specifies whether this canary is to use active X-Ray tracing when it
-- runs. Active tracing enables this canary run to be displayed in the
-- ServiceLens and X-Ray service maps even if the canary does not hit an
-- endpoint that has X-Ray tracing enabled. Using X-Ray tracing incurs
-- charges. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch_Synthetics_Canaries_tracing.html Canaries and X-Ray tracing>.
--
-- You can enable active tracing only for canaries that use version
-- @syn-nodejs-2.0@ or later for their canary runtime.
--
-- 'environmentVariables', 'canaryRunConfigInput_environmentVariables' - Specifies the keys and values to use for any environment variables used
-- in the canary script. Use the following format:
--
-- { \"key1\" : \"value1\", \"key2\" : \"value2\", ...}
--
-- Keys must start with a letter and be at least two characters. The total
-- size of your environment variables cannot exceed 4 KB. You can\'t
-- specify any Lambda reserved environment variables as the keys for your
-- environment variables. For more information about reserved keys, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-envvars.html#configuration-envvars-runtime Runtime environment variables>.
--
-- The environment variables keys and values are not encrypted. Do not
-- store sensitive information in this field.
--
-- 'memoryInMB', 'canaryRunConfigInput_memoryInMB' - The maximum amount of memory available to the canary while it is
-- running, in MB. This value must be a multiple of 64.
--
-- 'timeoutInSeconds', 'canaryRunConfigInput_timeoutInSeconds' - How long the canary is allowed to run before it must stop. You can\'t
-- set this time to be longer than the frequency of the runs of this
-- canary.
--
-- If you omit this field, the frequency of the canary is used as this
-- value, up to a maximum of 14 minutes.
newCanaryRunConfigInput ::
  CanaryRunConfigInput
newCanaryRunConfigInput =
  CanaryRunConfigInput'
    { activeTracing =
        Prelude.Nothing,
      environmentVariables = Prelude.Nothing,
      memoryInMB = Prelude.Nothing,
      timeoutInSeconds = Prelude.Nothing
    }

-- | Specifies whether this canary is to use active X-Ray tracing when it
-- runs. Active tracing enables this canary run to be displayed in the
-- ServiceLens and X-Ray service maps even if the canary does not hit an
-- endpoint that has X-Ray tracing enabled. Using X-Ray tracing incurs
-- charges. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch_Synthetics_Canaries_tracing.html Canaries and X-Ray tracing>.
--
-- You can enable active tracing only for canaries that use version
-- @syn-nodejs-2.0@ or later for their canary runtime.
canaryRunConfigInput_activeTracing :: Lens.Lens' CanaryRunConfigInput (Prelude.Maybe Prelude.Bool)
canaryRunConfigInput_activeTracing = Lens.lens (\CanaryRunConfigInput' {activeTracing} -> activeTracing) (\s@CanaryRunConfigInput' {} a -> s {activeTracing = a} :: CanaryRunConfigInput)

-- | Specifies the keys and values to use for any environment variables used
-- in the canary script. Use the following format:
--
-- { \"key1\" : \"value1\", \"key2\" : \"value2\", ...}
--
-- Keys must start with a letter and be at least two characters. The total
-- size of your environment variables cannot exceed 4 KB. You can\'t
-- specify any Lambda reserved environment variables as the keys for your
-- environment variables. For more information about reserved keys, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-envvars.html#configuration-envvars-runtime Runtime environment variables>.
--
-- The environment variables keys and values are not encrypted. Do not
-- store sensitive information in this field.
canaryRunConfigInput_environmentVariables :: Lens.Lens' CanaryRunConfigInput (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
canaryRunConfigInput_environmentVariables = Lens.lens (\CanaryRunConfigInput' {environmentVariables} -> environmentVariables) (\s@CanaryRunConfigInput' {} a -> s {environmentVariables = a} :: CanaryRunConfigInput) Prelude.. Lens.mapping Lens.coerced

-- | The maximum amount of memory available to the canary while it is
-- running, in MB. This value must be a multiple of 64.
canaryRunConfigInput_memoryInMB :: Lens.Lens' CanaryRunConfigInput (Prelude.Maybe Prelude.Natural)
canaryRunConfigInput_memoryInMB = Lens.lens (\CanaryRunConfigInput' {memoryInMB} -> memoryInMB) (\s@CanaryRunConfigInput' {} a -> s {memoryInMB = a} :: CanaryRunConfigInput)

-- | How long the canary is allowed to run before it must stop. You can\'t
-- set this time to be longer than the frequency of the runs of this
-- canary.
--
-- If you omit this field, the frequency of the canary is used as this
-- value, up to a maximum of 14 minutes.
canaryRunConfigInput_timeoutInSeconds :: Lens.Lens' CanaryRunConfigInput (Prelude.Maybe Prelude.Natural)
canaryRunConfigInput_timeoutInSeconds = Lens.lens (\CanaryRunConfigInput' {timeoutInSeconds} -> timeoutInSeconds) (\s@CanaryRunConfigInput' {} a -> s {timeoutInSeconds = a} :: CanaryRunConfigInput)

instance Prelude.Hashable CanaryRunConfigInput where
  hashWithSalt _salt CanaryRunConfigInput' {..} =
    _salt
      `Prelude.hashWithSalt` activeTracing
      `Prelude.hashWithSalt` environmentVariables
      `Prelude.hashWithSalt` memoryInMB
      `Prelude.hashWithSalt` timeoutInSeconds

instance Prelude.NFData CanaryRunConfigInput where
  rnf CanaryRunConfigInput' {..} =
    Prelude.rnf activeTracing
      `Prelude.seq` Prelude.rnf environmentVariables
      `Prelude.seq` Prelude.rnf memoryInMB
      `Prelude.seq` Prelude.rnf timeoutInSeconds

instance Data.ToJSON CanaryRunConfigInput where
  toJSON CanaryRunConfigInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ActiveTracing" Data..=) Prelude.<$> activeTracing,
            ("EnvironmentVariables" Data..=)
              Prelude.<$> environmentVariables,
            ("MemoryInMB" Data..=) Prelude.<$> memoryInMB,
            ("TimeoutInSeconds" Data..=)
              Prelude.<$> timeoutInSeconds
          ]
      )
