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
-- Module      : Amazonka.Pipes.Types.PipeTargetLambdaFunctionParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Types.PipeTargetLambdaFunctionParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pipes.Types.PipeTargetInvocationType
import qualified Amazonka.Prelude as Prelude

-- | The parameters for using a Lambda function as a target.
--
-- /See:/ 'newPipeTargetLambdaFunctionParameters' smart constructor.
data PipeTargetLambdaFunctionParameters = PipeTargetLambdaFunctionParameters'
  { -- | Choose from the following options.
    --
    -- -   @RequestResponse@ (default) - Invoke the function synchronously.
    --     Keep the connection open until the function returns a response or
    --     times out. The API response includes the function response and
    --     additional data.
    --
    -- -   @Event@ - Invoke the function asynchronously. Send events that fail
    --     multiple times to the function\'s dead-letter queue (if it\'s
    --     configured). The API response only includes a status code.
    --
    -- -   @DryRun@ - Validate parameter values and verify that the user or
    --     role has permission to invoke the function.
    invocationType :: Prelude.Maybe PipeTargetInvocationType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PipeTargetLambdaFunctionParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'invocationType', 'pipeTargetLambdaFunctionParameters_invocationType' - Choose from the following options.
--
-- -   @RequestResponse@ (default) - Invoke the function synchronously.
--     Keep the connection open until the function returns a response or
--     times out. The API response includes the function response and
--     additional data.
--
-- -   @Event@ - Invoke the function asynchronously. Send events that fail
--     multiple times to the function\'s dead-letter queue (if it\'s
--     configured). The API response only includes a status code.
--
-- -   @DryRun@ - Validate parameter values and verify that the user or
--     role has permission to invoke the function.
newPipeTargetLambdaFunctionParameters ::
  PipeTargetLambdaFunctionParameters
newPipeTargetLambdaFunctionParameters =
  PipeTargetLambdaFunctionParameters'
    { invocationType =
        Prelude.Nothing
    }

-- | Choose from the following options.
--
-- -   @RequestResponse@ (default) - Invoke the function synchronously.
--     Keep the connection open until the function returns a response or
--     times out. The API response includes the function response and
--     additional data.
--
-- -   @Event@ - Invoke the function asynchronously. Send events that fail
--     multiple times to the function\'s dead-letter queue (if it\'s
--     configured). The API response only includes a status code.
--
-- -   @DryRun@ - Validate parameter values and verify that the user or
--     role has permission to invoke the function.
pipeTargetLambdaFunctionParameters_invocationType :: Lens.Lens' PipeTargetLambdaFunctionParameters (Prelude.Maybe PipeTargetInvocationType)
pipeTargetLambdaFunctionParameters_invocationType = Lens.lens (\PipeTargetLambdaFunctionParameters' {invocationType} -> invocationType) (\s@PipeTargetLambdaFunctionParameters' {} a -> s {invocationType = a} :: PipeTargetLambdaFunctionParameters)

instance
  Data.FromJSON
    PipeTargetLambdaFunctionParameters
  where
  parseJSON =
    Data.withObject
      "PipeTargetLambdaFunctionParameters"
      ( \x ->
          PipeTargetLambdaFunctionParameters'
            Prelude.<$> (x Data..:? "InvocationType")
      )

instance
  Prelude.Hashable
    PipeTargetLambdaFunctionParameters
  where
  hashWithSalt
    _salt
    PipeTargetLambdaFunctionParameters' {..} =
      _salt `Prelude.hashWithSalt` invocationType

instance
  Prelude.NFData
    PipeTargetLambdaFunctionParameters
  where
  rnf PipeTargetLambdaFunctionParameters' {..} =
    Prelude.rnf invocationType

instance
  Data.ToJSON
    PipeTargetLambdaFunctionParameters
  where
  toJSON PipeTargetLambdaFunctionParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("InvocationType" Data..=)
              Prelude.<$> invocationType
          ]
      )
