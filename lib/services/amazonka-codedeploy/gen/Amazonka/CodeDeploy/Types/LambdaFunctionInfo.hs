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
-- Module      : Amazonka.CodeDeploy.Types.LambdaFunctionInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.LambdaFunctionInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a Lambda function specified in a deployment.
--
-- /See:/ 'newLambdaFunctionInfo' smart constructor.
data LambdaFunctionInfo = LambdaFunctionInfo'
  { -- | The version of a Lambda function that production traffic points to.
    currentVersion :: Prelude.Maybe Prelude.Text,
    -- | The alias of a Lambda function. For more information, see
    -- <https://docs.aws.amazon.com/lambda/latest/dg/aliases-intro.html Lambda Function Aliases>
    -- in the /Lambda Developer Guide/.
    functionAlias :: Prelude.Maybe Prelude.Text,
    -- | The name of a Lambda function.
    functionName :: Prelude.Maybe Prelude.Text,
    -- | The version of a Lambda function that production traffic points to after
    -- the Lambda function is deployed.
    targetVersion :: Prelude.Maybe Prelude.Text,
    -- | The percentage of production traffic that the target version of a Lambda
    -- function receives.
    targetVersionWeight :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LambdaFunctionInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'currentVersion', 'lambdaFunctionInfo_currentVersion' - The version of a Lambda function that production traffic points to.
--
-- 'functionAlias', 'lambdaFunctionInfo_functionAlias' - The alias of a Lambda function. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/aliases-intro.html Lambda Function Aliases>
-- in the /Lambda Developer Guide/.
--
-- 'functionName', 'lambdaFunctionInfo_functionName' - The name of a Lambda function.
--
-- 'targetVersion', 'lambdaFunctionInfo_targetVersion' - The version of a Lambda function that production traffic points to after
-- the Lambda function is deployed.
--
-- 'targetVersionWeight', 'lambdaFunctionInfo_targetVersionWeight' - The percentage of production traffic that the target version of a Lambda
-- function receives.
newLambdaFunctionInfo ::
  LambdaFunctionInfo
newLambdaFunctionInfo =
  LambdaFunctionInfo'
    { currentVersion =
        Prelude.Nothing,
      functionAlias = Prelude.Nothing,
      functionName = Prelude.Nothing,
      targetVersion = Prelude.Nothing,
      targetVersionWeight = Prelude.Nothing
    }

-- | The version of a Lambda function that production traffic points to.
lambdaFunctionInfo_currentVersion :: Lens.Lens' LambdaFunctionInfo (Prelude.Maybe Prelude.Text)
lambdaFunctionInfo_currentVersion = Lens.lens (\LambdaFunctionInfo' {currentVersion} -> currentVersion) (\s@LambdaFunctionInfo' {} a -> s {currentVersion = a} :: LambdaFunctionInfo)

-- | The alias of a Lambda function. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/aliases-intro.html Lambda Function Aliases>
-- in the /Lambda Developer Guide/.
lambdaFunctionInfo_functionAlias :: Lens.Lens' LambdaFunctionInfo (Prelude.Maybe Prelude.Text)
lambdaFunctionInfo_functionAlias = Lens.lens (\LambdaFunctionInfo' {functionAlias} -> functionAlias) (\s@LambdaFunctionInfo' {} a -> s {functionAlias = a} :: LambdaFunctionInfo)

-- | The name of a Lambda function.
lambdaFunctionInfo_functionName :: Lens.Lens' LambdaFunctionInfo (Prelude.Maybe Prelude.Text)
lambdaFunctionInfo_functionName = Lens.lens (\LambdaFunctionInfo' {functionName} -> functionName) (\s@LambdaFunctionInfo' {} a -> s {functionName = a} :: LambdaFunctionInfo)

-- | The version of a Lambda function that production traffic points to after
-- the Lambda function is deployed.
lambdaFunctionInfo_targetVersion :: Lens.Lens' LambdaFunctionInfo (Prelude.Maybe Prelude.Text)
lambdaFunctionInfo_targetVersion = Lens.lens (\LambdaFunctionInfo' {targetVersion} -> targetVersion) (\s@LambdaFunctionInfo' {} a -> s {targetVersion = a} :: LambdaFunctionInfo)

-- | The percentage of production traffic that the target version of a Lambda
-- function receives.
lambdaFunctionInfo_targetVersionWeight :: Lens.Lens' LambdaFunctionInfo (Prelude.Maybe Prelude.Double)
lambdaFunctionInfo_targetVersionWeight = Lens.lens (\LambdaFunctionInfo' {targetVersionWeight} -> targetVersionWeight) (\s@LambdaFunctionInfo' {} a -> s {targetVersionWeight = a} :: LambdaFunctionInfo)

instance Data.FromJSON LambdaFunctionInfo where
  parseJSON =
    Data.withObject
      "LambdaFunctionInfo"
      ( \x ->
          LambdaFunctionInfo'
            Prelude.<$> (x Data..:? "currentVersion")
            Prelude.<*> (x Data..:? "functionAlias")
            Prelude.<*> (x Data..:? "functionName")
            Prelude.<*> (x Data..:? "targetVersion")
            Prelude.<*> (x Data..:? "targetVersionWeight")
      )

instance Prelude.Hashable LambdaFunctionInfo where
  hashWithSalt _salt LambdaFunctionInfo' {..} =
    _salt `Prelude.hashWithSalt` currentVersion
      `Prelude.hashWithSalt` functionAlias
      `Prelude.hashWithSalt` functionName
      `Prelude.hashWithSalt` targetVersion
      `Prelude.hashWithSalt` targetVersionWeight

instance Prelude.NFData LambdaFunctionInfo where
  rnf LambdaFunctionInfo' {..} =
    Prelude.rnf currentVersion
      `Prelude.seq` Prelude.rnf functionAlias
      `Prelude.seq` Prelude.rnf functionName
      `Prelude.seq` Prelude.rnf targetVersion
      `Prelude.seq` Prelude.rnf targetVersionWeight
