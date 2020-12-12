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
    ldscLambdaFunctionARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an AWS Lambda data source configuration.
--
-- /See:/ 'mkLambdaDataSourceConfig' smart constructor.
newtype LambdaDataSourceConfig = LambdaDataSourceConfig'
  { lambdaFunctionARN ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LambdaDataSourceConfig' with the minimum fields required to make a request.
--
-- * 'lambdaFunctionARN' - The ARN for the Lambda function.
mkLambdaDataSourceConfig ::
  -- | 'lambdaFunctionARN'
  Lude.Text ->
  LambdaDataSourceConfig
mkLambdaDataSourceConfig pLambdaFunctionARN_ =
  LambdaDataSourceConfig' {lambdaFunctionARN = pLambdaFunctionARN_}

-- | The ARN for the Lambda function.
--
-- /Note:/ Consider using 'lambdaFunctionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldscLambdaFunctionARN :: Lens.Lens' LambdaDataSourceConfig Lude.Text
ldscLambdaFunctionARN = Lens.lens (lambdaFunctionARN :: LambdaDataSourceConfig -> Lude.Text) (\s a -> s {lambdaFunctionARN = a} :: LambdaDataSourceConfig)
{-# DEPRECATED ldscLambdaFunctionARN "Use generic-lens or generic-optics with 'lambdaFunctionARN' instead." #-}

instance Lude.FromJSON LambdaDataSourceConfig where
  parseJSON =
    Lude.withObject
      "LambdaDataSourceConfig"
      ( \x ->
          LambdaDataSourceConfig' Lude.<$> (x Lude..: "lambdaFunctionArn")
      )

instance Lude.ToJSON LambdaDataSourceConfig where
  toJSON LambdaDataSourceConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("lambdaFunctionArn" Lude..= lambdaFunctionARN)]
      )
