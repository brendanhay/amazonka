{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.LambdaFunctionInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.LambdaFunctionInfo
  ( LambdaFunctionInfo (..),

    -- * Smart constructor
    mkLambdaFunctionInfo,

    -- * Lenses
    lfiCurrentVersion,
    lfiFunctionAlias,
    lfiFunctionName,
    lfiTargetVersion,
    lfiTargetVersionWeight,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a Lambda function specified in a deployment.
--
-- /See:/ 'mkLambdaFunctionInfo' smart constructor.
data LambdaFunctionInfo = LambdaFunctionInfo'
  { -- | The version of a Lambda function that production traffic points to.
    currentVersion :: Lude.Maybe Lude.Text,
    -- | The alias of a Lambda function. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/aliases-intro.html AWS Lambda Function Aliases> in the /AWS Lambda Developer Guide/ .
    functionAlias :: Lude.Maybe Lude.Text,
    -- | The name of a Lambda function.
    functionName :: Lude.Maybe Lude.Text,
    -- | The version of a Lambda function that production traffic points to after the Lambda function is deployed.
    targetVersion :: Lude.Maybe Lude.Text,
    -- | The percentage of production traffic that the target version of a Lambda function receives.
    targetVersionWeight :: Lude.Maybe Lude.Double
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LambdaFunctionInfo' with the minimum fields required to make a request.
--
-- * 'currentVersion' - The version of a Lambda function that production traffic points to.
-- * 'functionAlias' - The alias of a Lambda function. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/aliases-intro.html AWS Lambda Function Aliases> in the /AWS Lambda Developer Guide/ .
-- * 'functionName' - The name of a Lambda function.
-- * 'targetVersion' - The version of a Lambda function that production traffic points to after the Lambda function is deployed.
-- * 'targetVersionWeight' - The percentage of production traffic that the target version of a Lambda function receives.
mkLambdaFunctionInfo ::
  LambdaFunctionInfo
mkLambdaFunctionInfo =
  LambdaFunctionInfo'
    { currentVersion = Lude.Nothing,
      functionAlias = Lude.Nothing,
      functionName = Lude.Nothing,
      targetVersion = Lude.Nothing,
      targetVersionWeight = Lude.Nothing
    }

-- | The version of a Lambda function that production traffic points to.
--
-- /Note:/ Consider using 'currentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfiCurrentVersion :: Lens.Lens' LambdaFunctionInfo (Lude.Maybe Lude.Text)
lfiCurrentVersion = Lens.lens (currentVersion :: LambdaFunctionInfo -> Lude.Maybe Lude.Text) (\s a -> s {currentVersion = a} :: LambdaFunctionInfo)
{-# DEPRECATED lfiCurrentVersion "Use generic-lens or generic-optics with 'currentVersion' instead." #-}

-- | The alias of a Lambda function. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/aliases-intro.html AWS Lambda Function Aliases> in the /AWS Lambda Developer Guide/ .
--
-- /Note:/ Consider using 'functionAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfiFunctionAlias :: Lens.Lens' LambdaFunctionInfo (Lude.Maybe Lude.Text)
lfiFunctionAlias = Lens.lens (functionAlias :: LambdaFunctionInfo -> Lude.Maybe Lude.Text) (\s a -> s {functionAlias = a} :: LambdaFunctionInfo)
{-# DEPRECATED lfiFunctionAlias "Use generic-lens or generic-optics with 'functionAlias' instead." #-}

-- | The name of a Lambda function.
--
-- /Note:/ Consider using 'functionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfiFunctionName :: Lens.Lens' LambdaFunctionInfo (Lude.Maybe Lude.Text)
lfiFunctionName = Lens.lens (functionName :: LambdaFunctionInfo -> Lude.Maybe Lude.Text) (\s a -> s {functionName = a} :: LambdaFunctionInfo)
{-# DEPRECATED lfiFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

-- | The version of a Lambda function that production traffic points to after the Lambda function is deployed.
--
-- /Note:/ Consider using 'targetVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfiTargetVersion :: Lens.Lens' LambdaFunctionInfo (Lude.Maybe Lude.Text)
lfiTargetVersion = Lens.lens (targetVersion :: LambdaFunctionInfo -> Lude.Maybe Lude.Text) (\s a -> s {targetVersion = a} :: LambdaFunctionInfo)
{-# DEPRECATED lfiTargetVersion "Use generic-lens or generic-optics with 'targetVersion' instead." #-}

-- | The percentage of production traffic that the target version of a Lambda function receives.
--
-- /Note:/ Consider using 'targetVersionWeight' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfiTargetVersionWeight :: Lens.Lens' LambdaFunctionInfo (Lude.Maybe Lude.Double)
lfiTargetVersionWeight = Lens.lens (targetVersionWeight :: LambdaFunctionInfo -> Lude.Maybe Lude.Double) (\s a -> s {targetVersionWeight = a} :: LambdaFunctionInfo)
{-# DEPRECATED lfiTargetVersionWeight "Use generic-lens or generic-optics with 'targetVersionWeight' instead." #-}

instance Lude.FromJSON LambdaFunctionInfo where
  parseJSON =
    Lude.withObject
      "LambdaFunctionInfo"
      ( \x ->
          LambdaFunctionInfo'
            Lude.<$> (x Lude..:? "currentVersion")
            Lude.<*> (x Lude..:? "functionAlias")
            Lude.<*> (x Lude..:? "functionName")
            Lude.<*> (x Lude..:? "targetVersion")
            Lude.<*> (x Lude..:? "targetVersionWeight")
      )
