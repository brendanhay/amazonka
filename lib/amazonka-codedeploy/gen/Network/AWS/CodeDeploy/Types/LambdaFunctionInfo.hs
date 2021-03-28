{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.LambdaFunctionInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.LambdaFunctionInfo
  ( LambdaFunctionInfo (..)
  -- * Smart constructor
  , mkLambdaFunctionInfo
  -- * Lenses
  , lfiCurrentVersion
  , lfiFunctionAlias
  , lfiFunctionName
  , lfiTargetVersion
  , lfiTargetVersionWeight
  ) where

import qualified Network.AWS.CodeDeploy.Types.FunctionAlias as Types
import qualified Network.AWS.CodeDeploy.Types.LambdaFunctionName as Types
import qualified Network.AWS.CodeDeploy.Types.Version as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a Lambda function specified in a deployment. 
--
-- /See:/ 'mkLambdaFunctionInfo' smart constructor.
data LambdaFunctionInfo = LambdaFunctionInfo'
  { currentVersion :: Core.Maybe Types.Version
    -- ^ The version of a Lambda function that production traffic points to. 
  , functionAlias :: Core.Maybe Types.FunctionAlias
    -- ^ The alias of a Lambda function. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/aliases-intro.html AWS Lambda Function Aliases> in the /AWS Lambda Developer Guide/ .
  , functionName :: Core.Maybe Types.LambdaFunctionName
    -- ^ The name of a Lambda function. 
  , targetVersion :: Core.Maybe Types.Version
    -- ^ The version of a Lambda function that production traffic points to after the Lambda function is deployed. 
  , targetVersionWeight :: Core.Maybe Core.Double
    -- ^ The percentage of production traffic that the target version of a Lambda function receives. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LambdaFunctionInfo' value with any optional fields omitted.
mkLambdaFunctionInfo
    :: LambdaFunctionInfo
mkLambdaFunctionInfo
  = LambdaFunctionInfo'{currentVersion = Core.Nothing,
                        functionAlias = Core.Nothing, functionName = Core.Nothing,
                        targetVersion = Core.Nothing, targetVersionWeight = Core.Nothing}

-- | The version of a Lambda function that production traffic points to. 
--
-- /Note:/ Consider using 'currentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfiCurrentVersion :: Lens.Lens' LambdaFunctionInfo (Core.Maybe Types.Version)
lfiCurrentVersion = Lens.field @"currentVersion"
{-# INLINEABLE lfiCurrentVersion #-}
{-# DEPRECATED currentVersion "Use generic-lens or generic-optics with 'currentVersion' instead"  #-}

-- | The alias of a Lambda function. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/aliases-intro.html AWS Lambda Function Aliases> in the /AWS Lambda Developer Guide/ .
--
-- /Note:/ Consider using 'functionAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfiFunctionAlias :: Lens.Lens' LambdaFunctionInfo (Core.Maybe Types.FunctionAlias)
lfiFunctionAlias = Lens.field @"functionAlias"
{-# INLINEABLE lfiFunctionAlias #-}
{-# DEPRECATED functionAlias "Use generic-lens or generic-optics with 'functionAlias' instead"  #-}

-- | The name of a Lambda function. 
--
-- /Note:/ Consider using 'functionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfiFunctionName :: Lens.Lens' LambdaFunctionInfo (Core.Maybe Types.LambdaFunctionName)
lfiFunctionName = Lens.field @"functionName"
{-# INLINEABLE lfiFunctionName #-}
{-# DEPRECATED functionName "Use generic-lens or generic-optics with 'functionName' instead"  #-}

-- | The version of a Lambda function that production traffic points to after the Lambda function is deployed. 
--
-- /Note:/ Consider using 'targetVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfiTargetVersion :: Lens.Lens' LambdaFunctionInfo (Core.Maybe Types.Version)
lfiTargetVersion = Lens.field @"targetVersion"
{-# INLINEABLE lfiTargetVersion #-}
{-# DEPRECATED targetVersion "Use generic-lens or generic-optics with 'targetVersion' instead"  #-}

-- | The percentage of production traffic that the target version of a Lambda function receives. 
--
-- /Note:/ Consider using 'targetVersionWeight' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfiTargetVersionWeight :: Lens.Lens' LambdaFunctionInfo (Core.Maybe Core.Double)
lfiTargetVersionWeight = Lens.field @"targetVersionWeight"
{-# INLINEABLE lfiTargetVersionWeight #-}
{-# DEPRECATED targetVersionWeight "Use generic-lens or generic-optics with 'targetVersionWeight' instead"  #-}

instance Core.FromJSON LambdaFunctionInfo where
        parseJSON
          = Core.withObject "LambdaFunctionInfo" Core.$
              \ x ->
                LambdaFunctionInfo' Core.<$>
                  (x Core..:? "currentVersion") Core.<*> x Core..:? "functionAlias"
                    Core.<*> x Core..:? "functionName"
                    Core.<*> x Core..:? "targetVersion"
                    Core.<*> x Core..:? "targetVersionWeight"
