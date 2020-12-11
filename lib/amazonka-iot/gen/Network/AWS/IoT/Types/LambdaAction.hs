-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.LambdaAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.LambdaAction
  ( LambdaAction (..),

    -- * Smart constructor
    mkLambdaAction,

    -- * Lenses
    laFunctionARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an action to invoke a Lambda function.
--
-- /See:/ 'mkLambdaAction' smart constructor.
newtype LambdaAction = LambdaAction' {functionARN :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LambdaAction' with the minimum fields required to make a request.
--
-- * 'functionARN' - The ARN of the Lambda function.
mkLambdaAction ::
  -- | 'functionARN'
  Lude.Text ->
  LambdaAction
mkLambdaAction pFunctionARN_ =
  LambdaAction' {functionARN = pFunctionARN_}

-- | The ARN of the Lambda function.
--
-- /Note:/ Consider using 'functionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laFunctionARN :: Lens.Lens' LambdaAction Lude.Text
laFunctionARN = Lens.lens (functionARN :: LambdaAction -> Lude.Text) (\s a -> s {functionARN = a} :: LambdaAction)
{-# DEPRECATED laFunctionARN "Use generic-lens or generic-optics with 'functionARN' instead." #-}

instance Lude.FromJSON LambdaAction where
  parseJSON =
    Lude.withObject
      "LambdaAction"
      (\x -> LambdaAction' Lude.<$> (x Lude..: "functionArn"))

instance Lude.ToJSON LambdaAction where
  toJSON LambdaAction' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("functionArn" Lude..= functionARN)])
