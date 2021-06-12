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
-- Module      : Network.AWS.CodePipeline.Types.LambdaExecutorConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.LambdaExecutorConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Details about the configuration for the @Lambda@ action engine, or
-- executor.
--
-- /See:/ 'newLambdaExecutorConfiguration' smart constructor.
data LambdaExecutorConfiguration = LambdaExecutorConfiguration'
  { -- | The ARN of the Lambda function used by the action engine.
    lambdaFunctionArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  LambdaExecutorConfiguration
newLambdaExecutorConfiguration pLambdaFunctionArn_ =
  LambdaExecutorConfiguration'
    { lambdaFunctionArn =
        pLambdaFunctionArn_
    }

-- | The ARN of the Lambda function used by the action engine.
lambdaExecutorConfiguration_lambdaFunctionArn :: Lens.Lens' LambdaExecutorConfiguration Core.Text
lambdaExecutorConfiguration_lambdaFunctionArn = Lens.lens (\LambdaExecutorConfiguration' {lambdaFunctionArn} -> lambdaFunctionArn) (\s@LambdaExecutorConfiguration' {} a -> s {lambdaFunctionArn = a} :: LambdaExecutorConfiguration)

instance Core.FromJSON LambdaExecutorConfiguration where
  parseJSON =
    Core.withObject
      "LambdaExecutorConfiguration"
      ( \x ->
          LambdaExecutorConfiguration'
            Core.<$> (x Core..: "lambdaFunctionArn")
      )

instance Core.Hashable LambdaExecutorConfiguration

instance Core.NFData LambdaExecutorConfiguration

instance Core.ToJSON LambdaExecutorConfiguration where
  toJSON LambdaExecutorConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("lambdaFunctionArn" Core..= lambdaFunctionArn)
          ]
      )
