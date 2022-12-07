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
-- Module      : Amazonka.SecurityHub.Types.AwsLambdaFunctionTracingConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsLambdaFunctionTracingConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The function\'s X-Ray tracing configuration.
--
-- /See:/ 'newAwsLambdaFunctionTracingConfig' smart constructor.
data AwsLambdaFunctionTracingConfig = AwsLambdaFunctionTracingConfig'
  { -- | The tracing mode.
    mode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsLambdaFunctionTracingConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mode', 'awsLambdaFunctionTracingConfig_mode' - The tracing mode.
newAwsLambdaFunctionTracingConfig ::
  AwsLambdaFunctionTracingConfig
newAwsLambdaFunctionTracingConfig =
  AwsLambdaFunctionTracingConfig'
    { mode =
        Prelude.Nothing
    }

-- | The tracing mode.
awsLambdaFunctionTracingConfig_mode :: Lens.Lens' AwsLambdaFunctionTracingConfig (Prelude.Maybe Prelude.Text)
awsLambdaFunctionTracingConfig_mode = Lens.lens (\AwsLambdaFunctionTracingConfig' {mode} -> mode) (\s@AwsLambdaFunctionTracingConfig' {} a -> s {mode = a} :: AwsLambdaFunctionTracingConfig)

instance Data.FromJSON AwsLambdaFunctionTracingConfig where
  parseJSON =
    Data.withObject
      "AwsLambdaFunctionTracingConfig"
      ( \x ->
          AwsLambdaFunctionTracingConfig'
            Prelude.<$> (x Data..:? "Mode")
      )

instance
  Prelude.Hashable
    AwsLambdaFunctionTracingConfig
  where
  hashWithSalt
    _salt
    AwsLambdaFunctionTracingConfig' {..} =
      _salt `Prelude.hashWithSalt` mode

instance
  Prelude.NFData
    AwsLambdaFunctionTracingConfig
  where
  rnf AwsLambdaFunctionTracingConfig' {..} =
    Prelude.rnf mode

instance Data.ToJSON AwsLambdaFunctionTracingConfig where
  toJSON AwsLambdaFunctionTracingConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Mode" Data..=) Prelude.<$> mode]
      )
