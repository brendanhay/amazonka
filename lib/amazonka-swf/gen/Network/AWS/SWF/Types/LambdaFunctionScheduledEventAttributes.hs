-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.LambdaFunctionScheduledEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.LambdaFunctionScheduledEventAttributes
  ( LambdaFunctionScheduledEventAttributes (..),

    -- * Smart constructor
    mkLambdaFunctionScheduledEventAttributes,

    -- * Lenses
    lfseaControl,
    lfseaInput,
    lfseaStartToCloseTimeout,
    lfseaId,
    lfseaName,
    lfseaDecisionTaskCompletedEventId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides the details of the @LambdaFunctionScheduled@ event. It isn't set for other event types.
--
-- /See:/ 'mkLambdaFunctionScheduledEventAttributes' smart constructor.
data LambdaFunctionScheduledEventAttributes = LambdaFunctionScheduledEventAttributes'
  { control ::
      Lude.Maybe
        Lude.Text,
    input ::
      Lude.Maybe
        Lude.Text,
    startToCloseTimeout ::
      Lude.Maybe
        Lude.Text,
    id ::
      Lude.Text,
    name ::
      Lude.Text,
    decisionTaskCompletedEventId ::
      Lude.Integer
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LambdaFunctionScheduledEventAttributes' with the minimum fields required to make a request.
--
-- * 'control' - Data attached to the event that the decider can use in subsequent workflow tasks. This data isn't sent to the Lambda task.
-- * 'decisionTaskCompletedEventId' - The ID of the @LambdaFunctionCompleted@ event corresponding to the decision that resulted in scheduling this activity task. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
-- * 'id' - The unique ID of the Lambda task.
-- * 'input' - The input provided to the Lambda task.
-- * 'name' - The name of the Lambda function.
-- * 'startToCloseTimeout' - The maximum amount of time a worker can take to process the Lambda task.
mkLambdaFunctionScheduledEventAttributes ::
  -- | 'id'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  -- | 'decisionTaskCompletedEventId'
  Lude.Integer ->
  LambdaFunctionScheduledEventAttributes
mkLambdaFunctionScheduledEventAttributes
  pId_
  pName_
  pDecisionTaskCompletedEventId_ =
    LambdaFunctionScheduledEventAttributes'
      { control = Lude.Nothing,
        input = Lude.Nothing,
        startToCloseTimeout = Lude.Nothing,
        id = pId_,
        name = pName_,
        decisionTaskCompletedEventId =
          pDecisionTaskCompletedEventId_
      }

-- | Data attached to the event that the decider can use in subsequent workflow tasks. This data isn't sent to the Lambda task.
--
-- /Note:/ Consider using 'control' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfseaControl :: Lens.Lens' LambdaFunctionScheduledEventAttributes (Lude.Maybe Lude.Text)
lfseaControl = Lens.lens (control :: LambdaFunctionScheduledEventAttributes -> Lude.Maybe Lude.Text) (\s a -> s {control = a} :: LambdaFunctionScheduledEventAttributes)
{-# DEPRECATED lfseaControl "Use generic-lens or generic-optics with 'control' instead." #-}

-- | The input provided to the Lambda task.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfseaInput :: Lens.Lens' LambdaFunctionScheduledEventAttributes (Lude.Maybe Lude.Text)
lfseaInput = Lens.lens (input :: LambdaFunctionScheduledEventAttributes -> Lude.Maybe Lude.Text) (\s a -> s {input = a} :: LambdaFunctionScheduledEventAttributes)
{-# DEPRECATED lfseaInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | The maximum amount of time a worker can take to process the Lambda task.
--
-- /Note:/ Consider using 'startToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfseaStartToCloseTimeout :: Lens.Lens' LambdaFunctionScheduledEventAttributes (Lude.Maybe Lude.Text)
lfseaStartToCloseTimeout = Lens.lens (startToCloseTimeout :: LambdaFunctionScheduledEventAttributes -> Lude.Maybe Lude.Text) (\s a -> s {startToCloseTimeout = a} :: LambdaFunctionScheduledEventAttributes)
{-# DEPRECATED lfseaStartToCloseTimeout "Use generic-lens or generic-optics with 'startToCloseTimeout' instead." #-}

-- | The unique ID of the Lambda task.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfseaId :: Lens.Lens' LambdaFunctionScheduledEventAttributes Lude.Text
lfseaId = Lens.lens (id :: LambdaFunctionScheduledEventAttributes -> Lude.Text) (\s a -> s {id = a} :: LambdaFunctionScheduledEventAttributes)
{-# DEPRECATED lfseaId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The name of the Lambda function.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfseaName :: Lens.Lens' LambdaFunctionScheduledEventAttributes Lude.Text
lfseaName = Lens.lens (name :: LambdaFunctionScheduledEventAttributes -> Lude.Text) (\s a -> s {name = a} :: LambdaFunctionScheduledEventAttributes)
{-# DEPRECATED lfseaName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID of the @LambdaFunctionCompleted@ event corresponding to the decision that resulted in scheduling this activity task. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'decisionTaskCompletedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfseaDecisionTaskCompletedEventId :: Lens.Lens' LambdaFunctionScheduledEventAttributes Lude.Integer
lfseaDecisionTaskCompletedEventId = Lens.lens (decisionTaskCompletedEventId :: LambdaFunctionScheduledEventAttributes -> Lude.Integer) (\s a -> s {decisionTaskCompletedEventId = a} :: LambdaFunctionScheduledEventAttributes)
{-# DEPRECATED lfseaDecisionTaskCompletedEventId "Use generic-lens or generic-optics with 'decisionTaskCompletedEventId' instead." #-}

instance Lude.FromJSON LambdaFunctionScheduledEventAttributes where
  parseJSON =
    Lude.withObject
      "LambdaFunctionScheduledEventAttributes"
      ( \x ->
          LambdaFunctionScheduledEventAttributes'
            Lude.<$> (x Lude..:? "control")
            Lude.<*> (x Lude..:? "input")
            Lude.<*> (x Lude..:? "startToCloseTimeout")
            Lude.<*> (x Lude..: "id")
            Lude.<*> (x Lude..: "name")
            Lude.<*> (x Lude..: "decisionTaskCompletedEventId")
      )
