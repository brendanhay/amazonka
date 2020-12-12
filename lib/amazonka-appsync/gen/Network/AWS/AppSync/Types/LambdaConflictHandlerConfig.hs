{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.LambdaConflictHandlerConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.LambdaConflictHandlerConfig
  ( LambdaConflictHandlerConfig (..),

    -- * Smart constructor
    mkLambdaConflictHandlerConfig,

    -- * Lenses
    lchcLambdaConflictHandlerARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The @LambdaConflictHandlerConfig@ object when configuring LAMBDA as the Conflict Handler.
--
-- /See:/ 'mkLambdaConflictHandlerConfig' smart constructor.
newtype LambdaConflictHandlerConfig = LambdaConflictHandlerConfig'
  { lambdaConflictHandlerARN ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LambdaConflictHandlerConfig' with the minimum fields required to make a request.
--
-- * 'lambdaConflictHandlerARN' - The Arn for the Lambda function to use as the Conflict Handler.
mkLambdaConflictHandlerConfig ::
  LambdaConflictHandlerConfig
mkLambdaConflictHandlerConfig =
  LambdaConflictHandlerConfig'
    { lambdaConflictHandlerARN =
        Lude.Nothing
    }

-- | The Arn for the Lambda function to use as the Conflict Handler.
--
-- /Note:/ Consider using 'lambdaConflictHandlerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lchcLambdaConflictHandlerARN :: Lens.Lens' LambdaConflictHandlerConfig (Lude.Maybe Lude.Text)
lchcLambdaConflictHandlerARN = Lens.lens (lambdaConflictHandlerARN :: LambdaConflictHandlerConfig -> Lude.Maybe Lude.Text) (\s a -> s {lambdaConflictHandlerARN = a} :: LambdaConflictHandlerConfig)
{-# DEPRECATED lchcLambdaConflictHandlerARN "Use generic-lens or generic-optics with 'lambdaConflictHandlerARN' instead." #-}

instance Lude.FromJSON LambdaConflictHandlerConfig where
  parseJSON =
    Lude.withObject
      "LambdaConflictHandlerConfig"
      ( \x ->
          LambdaConflictHandlerConfig'
            Lude.<$> (x Lude..:? "lambdaConflictHandlerArn")
      )

instance Lude.ToJSON LambdaConflictHandlerConfig where
  toJSON LambdaConflictHandlerConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("lambdaConflictHandlerArn" Lude..=)
              Lude.<$> lambdaConflictHandlerARN
          ]
      )
