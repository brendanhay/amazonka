{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.LambdaActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.LambdaActivity
  ( LambdaActivity (..),

    -- * Smart constructor
    mkLambdaActivity,

    -- * Lenses
    laNext,
    laName,
    laLambdaName,
    laBatchSize,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An activity that runs a Lambda function to modify the message.
--
-- /See:/ 'mkLambdaActivity' smart constructor.
data LambdaActivity = LambdaActivity'
  { next :: Lude.Maybe Lude.Text,
    name :: Lude.Text,
    lambdaName :: Lude.Text,
    batchSize :: Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LambdaActivity' with the minimum fields required to make a request.
--
-- * 'batchSize' - The number of messages passed to the Lambda function for processing.
--
-- The Lambda function must be able to process all of these messages within five minutes, which is the maximum timeout duration for Lambda functions.
-- * 'lambdaName' - The name of the Lambda function that is run on the message.
-- * 'name' - The name of the lambda activity.
-- * 'next' - The next activity in the pipeline.
mkLambdaActivity ::
  -- | 'name'
  Lude.Text ->
  -- | 'lambdaName'
  Lude.Text ->
  -- | 'batchSize'
  Lude.Natural ->
  LambdaActivity
mkLambdaActivity pName_ pLambdaName_ pBatchSize_ =
  LambdaActivity'
    { next = Lude.Nothing,
      name = pName_,
      lambdaName = pLambdaName_,
      batchSize = pBatchSize_
    }

-- | The next activity in the pipeline.
--
-- /Note:/ Consider using 'next' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laNext :: Lens.Lens' LambdaActivity (Lude.Maybe Lude.Text)
laNext = Lens.lens (next :: LambdaActivity -> Lude.Maybe Lude.Text) (\s a -> s {next = a} :: LambdaActivity)
{-# DEPRECATED laNext "Use generic-lens or generic-optics with 'next' instead." #-}

-- | The name of the lambda activity.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laName :: Lens.Lens' LambdaActivity Lude.Text
laName = Lens.lens (name :: LambdaActivity -> Lude.Text) (\s a -> s {name = a} :: LambdaActivity)
{-# DEPRECATED laName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The name of the Lambda function that is run on the message.
--
-- /Note:/ Consider using 'lambdaName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laLambdaName :: Lens.Lens' LambdaActivity Lude.Text
laLambdaName = Lens.lens (lambdaName :: LambdaActivity -> Lude.Text) (\s a -> s {lambdaName = a} :: LambdaActivity)
{-# DEPRECATED laLambdaName "Use generic-lens or generic-optics with 'lambdaName' instead." #-}

-- | The number of messages passed to the Lambda function for processing.
--
-- The Lambda function must be able to process all of these messages within five minutes, which is the maximum timeout duration for Lambda functions.
--
-- /Note:/ Consider using 'batchSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laBatchSize :: Lens.Lens' LambdaActivity Lude.Natural
laBatchSize = Lens.lens (batchSize :: LambdaActivity -> Lude.Natural) (\s a -> s {batchSize = a} :: LambdaActivity)
{-# DEPRECATED laBatchSize "Use generic-lens or generic-optics with 'batchSize' instead." #-}

instance Lude.FromJSON LambdaActivity where
  parseJSON =
    Lude.withObject
      "LambdaActivity"
      ( \x ->
          LambdaActivity'
            Lude.<$> (x Lude..:? "next")
            Lude.<*> (x Lude..: "name")
            Lude.<*> (x Lude..: "lambdaName")
            Lude.<*> (x Lude..: "batchSize")
      )

instance Lude.ToJSON LambdaActivity where
  toJSON LambdaActivity' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("next" Lude..=) Lude.<$> next,
            Lude.Just ("name" Lude..= name),
            Lude.Just ("lambdaName" Lude..= lambdaName),
            Lude.Just ("batchSize" Lude..= batchSize)
          ]
      )
