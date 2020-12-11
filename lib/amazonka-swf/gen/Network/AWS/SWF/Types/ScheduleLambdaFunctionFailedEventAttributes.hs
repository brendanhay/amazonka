-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ScheduleLambdaFunctionFailedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ScheduleLambdaFunctionFailedEventAttributes
  ( ScheduleLambdaFunctionFailedEventAttributes (..),

    -- * Smart constructor
    mkScheduleLambdaFunctionFailedEventAttributes,

    -- * Lenses
    slffeaId,
    slffeaName,
    slffeaCause,
    slffeaDecisionTaskCompletedEventId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SWF.Types.ScheduleLambdaFunctionFailedCause

-- | Provides the details of the @ScheduleLambdaFunctionFailed@ event. It isn't set for other event types.
--
-- /See:/ 'mkScheduleLambdaFunctionFailedEventAttributes' smart constructor.
data ScheduleLambdaFunctionFailedEventAttributes = ScheduleLambdaFunctionFailedEventAttributes'
  { id ::
      Lude.Text,
    name ::
      Lude.Text,
    cause ::
      ScheduleLambdaFunctionFailedCause,
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

-- | Creates a value of 'ScheduleLambdaFunctionFailedEventAttributes' with the minimum fields required to make a request.
--
-- * 'cause' - The cause of the failure. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
-- * 'decisionTaskCompletedEventId' - The ID of the @LambdaFunctionCompleted@ event corresponding to the decision that resulted in scheduling this Lambda task. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
-- * 'id' - The ID provided in the @ScheduleLambdaFunction@ decision that failed.
-- * 'name' - The name of the Lambda function.
mkScheduleLambdaFunctionFailedEventAttributes ::
  -- | 'id'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  -- | 'cause'
  ScheduleLambdaFunctionFailedCause ->
  -- | 'decisionTaskCompletedEventId'
  Lude.Integer ->
  ScheduleLambdaFunctionFailedEventAttributes
mkScheduleLambdaFunctionFailedEventAttributes
  pId_
  pName_
  pCause_
  pDecisionTaskCompletedEventId_ =
    ScheduleLambdaFunctionFailedEventAttributes'
      { id = pId_,
        name = pName_,
        cause = pCause_,
        decisionTaskCompletedEventId =
          pDecisionTaskCompletedEventId_
      }

-- | The ID provided in the @ScheduleLambdaFunction@ decision that failed.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slffeaId :: Lens.Lens' ScheduleLambdaFunctionFailedEventAttributes Lude.Text
slffeaId = Lens.lens (id :: ScheduleLambdaFunctionFailedEventAttributes -> Lude.Text) (\s a -> s {id = a} :: ScheduleLambdaFunctionFailedEventAttributes)
{-# DEPRECATED slffeaId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The name of the Lambda function.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slffeaName :: Lens.Lens' ScheduleLambdaFunctionFailedEventAttributes Lude.Text
slffeaName = Lens.lens (name :: ScheduleLambdaFunctionFailedEventAttributes -> Lude.Text) (\s a -> s {name = a} :: ScheduleLambdaFunctionFailedEventAttributes)
{-# DEPRECATED slffeaName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The cause of the failure. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'cause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slffeaCause :: Lens.Lens' ScheduleLambdaFunctionFailedEventAttributes ScheduleLambdaFunctionFailedCause
slffeaCause = Lens.lens (cause :: ScheduleLambdaFunctionFailedEventAttributes -> ScheduleLambdaFunctionFailedCause) (\s a -> s {cause = a} :: ScheduleLambdaFunctionFailedEventAttributes)
{-# DEPRECATED slffeaCause "Use generic-lens or generic-optics with 'cause' instead." #-}

-- | The ID of the @LambdaFunctionCompleted@ event corresponding to the decision that resulted in scheduling this Lambda task. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'decisionTaskCompletedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slffeaDecisionTaskCompletedEventId :: Lens.Lens' ScheduleLambdaFunctionFailedEventAttributes Lude.Integer
slffeaDecisionTaskCompletedEventId = Lens.lens (decisionTaskCompletedEventId :: ScheduleLambdaFunctionFailedEventAttributes -> Lude.Integer) (\s a -> s {decisionTaskCompletedEventId = a} :: ScheduleLambdaFunctionFailedEventAttributes)
{-# DEPRECATED slffeaDecisionTaskCompletedEventId "Use generic-lens or generic-optics with 'decisionTaskCompletedEventId' instead." #-}

instance Lude.FromJSON ScheduleLambdaFunctionFailedEventAttributes where
  parseJSON =
    Lude.withObject
      "ScheduleLambdaFunctionFailedEventAttributes"
      ( \x ->
          ScheduleLambdaFunctionFailedEventAttributes'
            Lude.<$> (x Lude..: "id")
            Lude.<*> (x Lude..: "name")
            Lude.<*> (x Lude..: "cause")
            Lude.<*> (x Lude..: "decisionTaskCompletedEventId")
      )
