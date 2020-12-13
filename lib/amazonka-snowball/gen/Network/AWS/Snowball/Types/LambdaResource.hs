{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.LambdaResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.LambdaResource
  ( LambdaResource (..),

    -- * Smart constructor
    mkLambdaResource,

    -- * Lenses
    lrEventTriggers,
    lrLambdaARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Snowball.Types.EventTriggerDefinition

-- | Identifies
--
-- /See:/ 'mkLambdaResource' smart constructor.
data LambdaResource = LambdaResource'
  { -- | The array of ARNs for 'S3Resource' objects to trigger the 'LambdaResource' objects associated with this job.
    eventTriggers :: Lude.Maybe [EventTriggerDefinition],
    -- | An Amazon Resource Name (ARN) that represents an AWS Lambda function to be triggered by PUT object actions on the associated local Amazon S3 resource.
    lambdaARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LambdaResource' with the minimum fields required to make a request.
--
-- * 'eventTriggers' - The array of ARNs for 'S3Resource' objects to trigger the 'LambdaResource' objects associated with this job.
-- * 'lambdaARN' - An Amazon Resource Name (ARN) that represents an AWS Lambda function to be triggered by PUT object actions on the associated local Amazon S3 resource.
mkLambdaResource ::
  LambdaResource
mkLambdaResource =
  LambdaResource'
    { eventTriggers = Lude.Nothing,
      lambdaARN = Lude.Nothing
    }

-- | The array of ARNs for 'S3Resource' objects to trigger the 'LambdaResource' objects associated with this job.
--
-- /Note:/ Consider using 'eventTriggers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrEventTriggers :: Lens.Lens' LambdaResource (Lude.Maybe [EventTriggerDefinition])
lrEventTriggers = Lens.lens (eventTriggers :: LambdaResource -> Lude.Maybe [EventTriggerDefinition]) (\s a -> s {eventTriggers = a} :: LambdaResource)
{-# DEPRECATED lrEventTriggers "Use generic-lens or generic-optics with 'eventTriggers' instead." #-}

-- | An Amazon Resource Name (ARN) that represents an AWS Lambda function to be triggered by PUT object actions on the associated local Amazon S3 resource.
--
-- /Note:/ Consider using 'lambdaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrLambdaARN :: Lens.Lens' LambdaResource (Lude.Maybe Lude.Text)
lrLambdaARN = Lens.lens (lambdaARN :: LambdaResource -> Lude.Maybe Lude.Text) (\s a -> s {lambdaARN = a} :: LambdaResource)
{-# DEPRECATED lrLambdaARN "Use generic-lens or generic-optics with 'lambdaARN' instead." #-}

instance Lude.FromJSON LambdaResource where
  parseJSON =
    Lude.withObject
      "LambdaResource"
      ( \x ->
          LambdaResource'
            Lude.<$> (x Lude..:? "EventTriggers" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "LambdaArn")
      )

instance Lude.ToJSON LambdaResource where
  toJSON LambdaResource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("EventTriggers" Lude..=) Lude.<$> eventTriggers,
            ("LambdaArn" Lude..=) Lude.<$> lambdaARN
          ]
      )
