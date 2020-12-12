{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.AssessmentRunNotification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.AssessmentRunNotification
  ( AssessmentRunNotification (..),

    -- * Smart constructor
    mkAssessmentRunNotification,

    -- * Lenses
    arnSnsTopicARN,
    arnSnsPublishStatusCode,
    arnMessage,
    arnDate,
    arnEvent,
    arnError,
  )
where

import Network.AWS.Inspector.Types.AssessmentRunNotificationSNSStatusCode
import Network.AWS.Inspector.Types.InspectorEvent
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Used as one of the elements of the 'AssessmentRun' data type.
--
-- /See:/ 'mkAssessmentRunNotification' smart constructor.
data AssessmentRunNotification = AssessmentRunNotification'
  { snsTopicARN ::
      Lude.Maybe Lude.Text,
    snsPublishStatusCode ::
      Lude.Maybe
        AssessmentRunNotificationSNSStatusCode,
    message :: Lude.Maybe Lude.Text,
    date :: Lude.Timestamp,
    event :: InspectorEvent,
    error :: Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssessmentRunNotification' with the minimum fields required to make a request.
--
-- * 'date' - The date of the notification.
-- * 'error' - The Boolean value that specifies whether the notification represents an error.
-- * 'event' - The event for which a notification is sent.
-- * 'message' - The message included in the notification.
-- * 'snsPublishStatusCode' - The status code of the SNS notification.
-- * 'snsTopicARN' - The SNS topic to which the SNS notification is sent.
mkAssessmentRunNotification ::
  -- | 'date'
  Lude.Timestamp ->
  -- | 'event'
  InspectorEvent ->
  -- | 'error'
  Lude.Bool ->
  AssessmentRunNotification
mkAssessmentRunNotification pDate_ pEvent_ pError_ =
  AssessmentRunNotification'
    { snsTopicARN = Lude.Nothing,
      snsPublishStatusCode = Lude.Nothing,
      message = Lude.Nothing,
      date = pDate_,
      event = pEvent_,
      error = pError_
    }

-- | The SNS topic to which the SNS notification is sent.
--
-- /Note:/ Consider using 'snsTopicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arnSnsTopicARN :: Lens.Lens' AssessmentRunNotification (Lude.Maybe Lude.Text)
arnSnsTopicARN = Lens.lens (snsTopicARN :: AssessmentRunNotification -> Lude.Maybe Lude.Text) (\s a -> s {snsTopicARN = a} :: AssessmentRunNotification)
{-# DEPRECATED arnSnsTopicARN "Use generic-lens or generic-optics with 'snsTopicARN' instead." #-}

-- | The status code of the SNS notification.
--
-- /Note:/ Consider using 'snsPublishStatusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arnSnsPublishStatusCode :: Lens.Lens' AssessmentRunNotification (Lude.Maybe AssessmentRunNotificationSNSStatusCode)
arnSnsPublishStatusCode = Lens.lens (snsPublishStatusCode :: AssessmentRunNotification -> Lude.Maybe AssessmentRunNotificationSNSStatusCode) (\s a -> s {snsPublishStatusCode = a} :: AssessmentRunNotification)
{-# DEPRECATED arnSnsPublishStatusCode "Use generic-lens or generic-optics with 'snsPublishStatusCode' instead." #-}

-- | The message included in the notification.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arnMessage :: Lens.Lens' AssessmentRunNotification (Lude.Maybe Lude.Text)
arnMessage = Lens.lens (message :: AssessmentRunNotification -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: AssessmentRunNotification)
{-# DEPRECATED arnMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The date of the notification.
--
-- /Note:/ Consider using 'date' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arnDate :: Lens.Lens' AssessmentRunNotification Lude.Timestamp
arnDate = Lens.lens (date :: AssessmentRunNotification -> Lude.Timestamp) (\s a -> s {date = a} :: AssessmentRunNotification)
{-# DEPRECATED arnDate "Use generic-lens or generic-optics with 'date' instead." #-}

-- | The event for which a notification is sent.
--
-- /Note:/ Consider using 'event' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arnEvent :: Lens.Lens' AssessmentRunNotification InspectorEvent
arnEvent = Lens.lens (event :: AssessmentRunNotification -> InspectorEvent) (\s a -> s {event = a} :: AssessmentRunNotification)
{-# DEPRECATED arnEvent "Use generic-lens or generic-optics with 'event' instead." #-}

-- | The Boolean value that specifies whether the notification represents an error.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arnError :: Lens.Lens' AssessmentRunNotification Lude.Bool
arnError = Lens.lens (error :: AssessmentRunNotification -> Lude.Bool) (\s a -> s {error = a} :: AssessmentRunNotification)
{-# DEPRECATED arnError "Use generic-lens or generic-optics with 'error' instead." #-}

instance Lude.FromJSON AssessmentRunNotification where
  parseJSON =
    Lude.withObject
      "AssessmentRunNotification"
      ( \x ->
          AssessmentRunNotification'
            Lude.<$> (x Lude..:? "snsTopicArn")
            Lude.<*> (x Lude..:? "snsPublishStatusCode")
            Lude.<*> (x Lude..:? "message")
            Lude.<*> (x Lude..: "date")
            Lude.<*> (x Lude..: "event")
            Lude.<*> (x Lude..: "error")
      )
