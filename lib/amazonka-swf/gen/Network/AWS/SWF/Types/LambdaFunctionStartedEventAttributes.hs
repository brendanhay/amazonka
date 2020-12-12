{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.LambdaFunctionStartedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.LambdaFunctionStartedEventAttributes
  ( LambdaFunctionStartedEventAttributes (..),

    -- * Smart constructor
    mkLambdaFunctionStartedEventAttributes,

    -- * Lenses
    lfseaScheduledEventId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides the details of the @LambdaFunctionStarted@ event. It isn't set for other event types.
--
-- /See:/ 'mkLambdaFunctionStartedEventAttributes' smart constructor.
newtype LambdaFunctionStartedEventAttributes = LambdaFunctionStartedEventAttributes'
  { scheduledEventId ::
      Lude.Integer
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LambdaFunctionStartedEventAttributes' with the minimum fields required to make a request.
--
-- * 'scheduledEventId' - The ID of the @LambdaFunctionScheduled@ event that was recorded when this activity task was scheduled. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
mkLambdaFunctionStartedEventAttributes ::
  -- | 'scheduledEventId'
  Lude.Integer ->
  LambdaFunctionStartedEventAttributes
mkLambdaFunctionStartedEventAttributes pScheduledEventId_ =
  LambdaFunctionStartedEventAttributes'
    { scheduledEventId =
        pScheduledEventId_
    }

-- | The ID of the @LambdaFunctionScheduled@ event that was recorded when this activity task was scheduled. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'scheduledEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfseaScheduledEventId :: Lens.Lens' LambdaFunctionStartedEventAttributes Lude.Integer
lfseaScheduledEventId = Lens.lens (scheduledEventId :: LambdaFunctionStartedEventAttributes -> Lude.Integer) (\s a -> s {scheduledEventId = a} :: LambdaFunctionStartedEventAttributes)
{-# DEPRECATED lfseaScheduledEventId "Use generic-lens or generic-optics with 'scheduledEventId' instead." #-}

instance Lude.FromJSON LambdaFunctionStartedEventAttributes where
  parseJSON =
    Lude.withObject
      "LambdaFunctionStartedEventAttributes"
      ( \x ->
          LambdaFunctionStartedEventAttributes'
            Lude.<$> (x Lude..: "scheduledEventId")
      )
