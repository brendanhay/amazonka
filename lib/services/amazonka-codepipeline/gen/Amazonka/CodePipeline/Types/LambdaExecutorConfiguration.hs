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
-- Module      : Amazonka.CodePipeline.Types.LambdaExecutorConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.LambdaExecutorConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about the configuration for the @Lambda@ action engine, or
-- executor.
--
-- /See:/ 'newLambdaExecutorConfiguration' smart constructor.
data LambdaExecutorConfiguration = LambdaExecutorConfiguration'
  { -- | The ARN of the Lambda function used by the action engine.
    lambdaFunctionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LambdaExecutorConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lambdaFunctionArn', 'lambdaExecutorConfiguration_lambdaFunctionArn' - The ARN of the Lambda function used by the action engine.
newLambdaExecutorConfiguration ::
  -- | 'lambdaFunctionArn'
  Prelude.Text ->
  LambdaExecutorConfiguration
newLambdaExecutorConfiguration pLambdaFunctionArn_ =
  LambdaExecutorConfiguration'
    { lambdaFunctionArn =
        pLambdaFunctionArn_
    }

-- | The ARN of the Lambda function used by the action engine.
lambdaExecutorConfiguration_lambdaFunctionArn :: Lens.Lens' LambdaExecutorConfiguration Prelude.Text
lambdaExecutorConfiguration_lambdaFunctionArn = Lens.lens (\LambdaExecutorConfiguration' {lambdaFunctionArn} -> lambdaFunctionArn) (\s@LambdaExecutorConfiguration' {} a -> s {lambdaFunctionArn = a} :: LambdaExecutorConfiguration)

instance Data.FromJSON LambdaExecutorConfiguration where
  parseJSON =
    Data.withObject
      "LambdaExecutorConfiguration"
      ( \x ->
          LambdaExecutorConfiguration'
            Prelude.<$> (x Data..: "lambdaFunctionArn")
      )

instance Prelude.Hashable LambdaExecutorConfiguration where
  hashWithSalt _salt LambdaExecutorConfiguration' {..} =
    _salt `Prelude.hashWithSalt` lambdaFunctionArn

instance Prelude.NFData LambdaExecutorConfiguration where
  rnf LambdaExecutorConfiguration' {..} =
    Prelude.rnf lambdaFunctionArn

instance Data.ToJSON LambdaExecutorConfiguration where
  toJSON LambdaExecutorConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("lambdaFunctionArn" Data..= lambdaFunctionArn)
          ]
      )
