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
-- Module      : Network.AWS.CodeDeploy.Types.LambdaFunctionInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.LambdaFunctionInfo where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a Lambda function specified in a deployment.
--
-- /See:/ 'newLambdaFunctionInfo' smart constructor.
data LambdaFunctionInfo = LambdaFunctionInfo'
  { -- | The alias of a Lambda function. For more information, see
    -- <https://docs.aws.amazon.com/lambda/latest/dg/aliases-intro.html AWS Lambda Function Aliases>
    -- in the /AWS Lambda Developer Guide/.
    functionAlias :: Prelude.Maybe Prelude.Text,
    -- | The version of a Lambda function that production traffic points to after
    -- the Lambda function is deployed.
    targetVersion :: Prelude.Maybe Prelude.Text,
    -- | The percentage of production traffic that the target version of a Lambda
    -- function receives.
    targetVersionWeight :: Prelude.Maybe Prelude.Double,
    -- | The version of a Lambda function that production traffic points to.
    currentVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of a Lambda function.
    functionName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LambdaFunctionInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'functionAlias', 'lambdaFunctionInfo_functionAlias' - The alias of a Lambda function. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/aliases-intro.html AWS Lambda Function Aliases>
-- in the /AWS Lambda Developer Guide/.
--
-- 'targetVersion', 'lambdaFunctionInfo_targetVersion' - The version of a Lambda function that production traffic points to after
-- the Lambda function is deployed.
--
-- 'targetVersionWeight', 'lambdaFunctionInfo_targetVersionWeight' - The percentage of production traffic that the target version of a Lambda
-- function receives.
--
-- 'currentVersion', 'lambdaFunctionInfo_currentVersion' - The version of a Lambda function that production traffic points to.
--
-- 'functionName', 'lambdaFunctionInfo_functionName' - The name of a Lambda function.
newLambdaFunctionInfo ::
  LambdaFunctionInfo
newLambdaFunctionInfo =
  LambdaFunctionInfo'
    { functionAlias =
        Prelude.Nothing,
      targetVersion = Prelude.Nothing,
      targetVersionWeight = Prelude.Nothing,
      currentVersion = Prelude.Nothing,
      functionName = Prelude.Nothing
    }

-- | The alias of a Lambda function. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/aliases-intro.html AWS Lambda Function Aliases>
-- in the /AWS Lambda Developer Guide/.
lambdaFunctionInfo_functionAlias :: Lens.Lens' LambdaFunctionInfo (Prelude.Maybe Prelude.Text)
lambdaFunctionInfo_functionAlias = Lens.lens (\LambdaFunctionInfo' {functionAlias} -> functionAlias) (\s@LambdaFunctionInfo' {} a -> s {functionAlias = a} :: LambdaFunctionInfo)

-- | The version of a Lambda function that production traffic points to after
-- the Lambda function is deployed.
lambdaFunctionInfo_targetVersion :: Lens.Lens' LambdaFunctionInfo (Prelude.Maybe Prelude.Text)
lambdaFunctionInfo_targetVersion = Lens.lens (\LambdaFunctionInfo' {targetVersion} -> targetVersion) (\s@LambdaFunctionInfo' {} a -> s {targetVersion = a} :: LambdaFunctionInfo)

-- | The percentage of production traffic that the target version of a Lambda
-- function receives.
lambdaFunctionInfo_targetVersionWeight :: Lens.Lens' LambdaFunctionInfo (Prelude.Maybe Prelude.Double)
lambdaFunctionInfo_targetVersionWeight = Lens.lens (\LambdaFunctionInfo' {targetVersionWeight} -> targetVersionWeight) (\s@LambdaFunctionInfo' {} a -> s {targetVersionWeight = a} :: LambdaFunctionInfo)

-- | The version of a Lambda function that production traffic points to.
lambdaFunctionInfo_currentVersion :: Lens.Lens' LambdaFunctionInfo (Prelude.Maybe Prelude.Text)
lambdaFunctionInfo_currentVersion = Lens.lens (\LambdaFunctionInfo' {currentVersion} -> currentVersion) (\s@LambdaFunctionInfo' {} a -> s {currentVersion = a} :: LambdaFunctionInfo)

-- | The name of a Lambda function.
lambdaFunctionInfo_functionName :: Lens.Lens' LambdaFunctionInfo (Prelude.Maybe Prelude.Text)
lambdaFunctionInfo_functionName = Lens.lens (\LambdaFunctionInfo' {functionName} -> functionName) (\s@LambdaFunctionInfo' {} a -> s {functionName = a} :: LambdaFunctionInfo)

instance Prelude.FromJSON LambdaFunctionInfo where
  parseJSON =
    Prelude.withObject
      "LambdaFunctionInfo"
      ( \x ->
          LambdaFunctionInfo'
            Prelude.<$> (x Prelude..:? "functionAlias")
            Prelude.<*> (x Prelude..:? "targetVersion")
            Prelude.<*> (x Prelude..:? "targetVersionWeight")
            Prelude.<*> (x Prelude..:? "currentVersion")
            Prelude.<*> (x Prelude..:? "functionName")
      )

instance Prelude.Hashable LambdaFunctionInfo

instance Prelude.NFData LambdaFunctionInfo
