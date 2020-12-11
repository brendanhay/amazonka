-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ScheduleLambdaFunctionDecisionAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ScheduleLambdaFunctionDecisionAttributes
  ( ScheduleLambdaFunctionDecisionAttributes (..),

    -- * Smart constructor
    mkScheduleLambdaFunctionDecisionAttributes,

    -- * Lenses
    slfdaControl,
    slfdaInput,
    slfdaStartToCloseTimeout,
    slfdaId,
    slfdaName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Decision attributes specified in @scheduleLambdaFunctionDecisionAttributes@ within the list of decisions @decisions@ passed to 'RespondDecisionTaskCompleted' .
--
-- /See:/ 'mkScheduleLambdaFunctionDecisionAttributes' smart constructor.
data ScheduleLambdaFunctionDecisionAttributes = ScheduleLambdaFunctionDecisionAttributes'
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
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ScheduleLambdaFunctionDecisionAttributes' with the minimum fields required to make a request.
--
-- * 'control' - The data attached to the event that the decider can use in subsequent workflow tasks. This data isn't sent to the Lambda task.
-- * 'id' - A string that identifies the Lambda function execution in the event history.
-- * 'input' - The optional input data to be supplied to the Lambda function.
-- * 'name' - The name, or ARN, of the Lambda function to schedule.
-- * 'startToCloseTimeout' - The timeout value, in seconds, after which the Lambda function is considered to be failed once it has started. This can be any integer from 1-300 (1s-5m). If no value is supplied, than a default value of 300s is assumed.
mkScheduleLambdaFunctionDecisionAttributes ::
  -- | 'id'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  ScheduleLambdaFunctionDecisionAttributes
mkScheduleLambdaFunctionDecisionAttributes pId_ pName_ =
  ScheduleLambdaFunctionDecisionAttributes'
    { control = Lude.Nothing,
      input = Lude.Nothing,
      startToCloseTimeout = Lude.Nothing,
      id = pId_,
      name = pName_
    }

-- | The data attached to the event that the decider can use in subsequent workflow tasks. This data isn't sent to the Lambda task.
--
-- /Note:/ Consider using 'control' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slfdaControl :: Lens.Lens' ScheduleLambdaFunctionDecisionAttributes (Lude.Maybe Lude.Text)
slfdaControl = Lens.lens (control :: ScheduleLambdaFunctionDecisionAttributes -> Lude.Maybe Lude.Text) (\s a -> s {control = a} :: ScheduleLambdaFunctionDecisionAttributes)
{-# DEPRECATED slfdaControl "Use generic-lens or generic-optics with 'control' instead." #-}

-- | The optional input data to be supplied to the Lambda function.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slfdaInput :: Lens.Lens' ScheduleLambdaFunctionDecisionAttributes (Lude.Maybe Lude.Text)
slfdaInput = Lens.lens (input :: ScheduleLambdaFunctionDecisionAttributes -> Lude.Maybe Lude.Text) (\s a -> s {input = a} :: ScheduleLambdaFunctionDecisionAttributes)
{-# DEPRECATED slfdaInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | The timeout value, in seconds, after which the Lambda function is considered to be failed once it has started. This can be any integer from 1-300 (1s-5m). If no value is supplied, than a default value of 300s is assumed.
--
-- /Note:/ Consider using 'startToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slfdaStartToCloseTimeout :: Lens.Lens' ScheduleLambdaFunctionDecisionAttributes (Lude.Maybe Lude.Text)
slfdaStartToCloseTimeout = Lens.lens (startToCloseTimeout :: ScheduleLambdaFunctionDecisionAttributes -> Lude.Maybe Lude.Text) (\s a -> s {startToCloseTimeout = a} :: ScheduleLambdaFunctionDecisionAttributes)
{-# DEPRECATED slfdaStartToCloseTimeout "Use generic-lens or generic-optics with 'startToCloseTimeout' instead." #-}

-- | A string that identifies the Lambda function execution in the event history.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slfdaId :: Lens.Lens' ScheduleLambdaFunctionDecisionAttributes Lude.Text
slfdaId = Lens.lens (id :: ScheduleLambdaFunctionDecisionAttributes -> Lude.Text) (\s a -> s {id = a} :: ScheduleLambdaFunctionDecisionAttributes)
{-# DEPRECATED slfdaId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The name, or ARN, of the Lambda function to schedule.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slfdaName :: Lens.Lens' ScheduleLambdaFunctionDecisionAttributes Lude.Text
slfdaName = Lens.lens (name :: ScheduleLambdaFunctionDecisionAttributes -> Lude.Text) (\s a -> s {name = a} :: ScheduleLambdaFunctionDecisionAttributes)
{-# DEPRECATED slfdaName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.ToJSON ScheduleLambdaFunctionDecisionAttributes where
  toJSON ScheduleLambdaFunctionDecisionAttributes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("control" Lude..=) Lude.<$> control,
            ("input" Lude..=) Lude.<$> input,
            ("startToCloseTimeout" Lude..=) Lude.<$> startToCloseTimeout,
            Lude.Just ("id" Lude..= id),
            Lude.Just ("name" Lude..= name)
          ]
      )
