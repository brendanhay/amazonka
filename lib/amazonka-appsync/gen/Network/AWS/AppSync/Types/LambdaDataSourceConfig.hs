{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.LambdaDataSourceConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.LambdaDataSourceConfig
  ( LambdaDataSourceConfig (..),

    -- * Smart constructor
    mkLambdaDataSourceConfig,

    -- * Lenses
    ldscLambdaFunctionArn,
  )
where

import qualified Network.AWS.AppSync.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an AWS Lambda data source configuration.
--
-- /See:/ 'mkLambdaDataSourceConfig' smart constructor.
newtype LambdaDataSourceConfig = LambdaDataSourceConfig'
  { -- | The ARN for the Lambda function.
    lambdaFunctionArn :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'LambdaDataSourceConfig' value with any optional fields omitted.
mkLambdaDataSourceConfig ::
  -- | 'lambdaFunctionArn'
  Types.String ->
  LambdaDataSourceConfig
mkLambdaDataSourceConfig lambdaFunctionArn =
  LambdaDataSourceConfig' {lambdaFunctionArn}

-- | The ARN for the Lambda function.
--
-- /Note:/ Consider using 'lambdaFunctionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldscLambdaFunctionArn :: Lens.Lens' LambdaDataSourceConfig Types.String
ldscLambdaFunctionArn = Lens.field @"lambdaFunctionArn"
{-# DEPRECATED ldscLambdaFunctionArn "Use generic-lens or generic-optics with 'lambdaFunctionArn' instead." #-}

instance Core.FromJSON LambdaDataSourceConfig where
  toJSON LambdaDataSourceConfig {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("lambdaFunctionArn" Core..= lambdaFunctionArn)]
      )

instance Core.FromJSON LambdaDataSourceConfig where
  parseJSON =
    Core.withObject "LambdaDataSourceConfig" Core.$
      \x ->
        LambdaDataSourceConfig' Core.<$> (x Core..: "lambdaFunctionArn")
