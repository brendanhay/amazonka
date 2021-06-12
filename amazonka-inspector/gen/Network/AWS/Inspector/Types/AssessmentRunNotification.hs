{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.AssessmentRunNotification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.AssessmentRunNotification where

import qualified Network.AWS.Core as Core
import Network.AWS.Inspector.Types.AssessmentRunNotificationSnsStatusCode
import Network.AWS.Inspector.Types.InspectorEvent
import qualified Network.AWS.Lens as Lens

-- | Used as one of the elements of the AssessmentRun data type.
--
-- /See:/ 'newAssessmentRunNotification' smart constructor.
data AssessmentRunNotification = AssessmentRunNotification'
  { -- | The message included in the notification.
    message :: Core.Maybe Core.Text,
    -- | The status code of the SNS notification.
    snsPublishStatusCode :: Core.Maybe AssessmentRunNotificationSnsStatusCode,
    -- | The SNS topic to which the SNS notification is sent.
    snsTopicArn :: Core.Maybe Core.Text,
    -- | The date of the notification.
    date :: Core.POSIX,
    -- | The event for which a notification is sent.
    event :: InspectorEvent,
    -- | The Boolean value that specifies whether the notification represents an
    -- error.
    error :: Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssessmentRunNotification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'assessmentRunNotification_message' - The message included in the notification.
--
-- 'snsPublishStatusCode', 'assessmentRunNotification_snsPublishStatusCode' - The status code of the SNS notification.
--
-- 'snsTopicArn', 'assessmentRunNotification_snsTopicArn' - The SNS topic to which the SNS notification is sent.
--
-- 'date', 'assessmentRunNotification_date' - The date of the notification.
--
-- 'event', 'assessmentRunNotification_event' - The event for which a notification is sent.
--
-- 'error', 'assessmentRunNotification_error' - The Boolean value that specifies whether the notification represents an
-- error.
newAssessmentRunNotification ::
  -- | 'date'
  Core.UTCTime ->
  -- | 'event'
  InspectorEvent ->
  -- | 'error'
  Core.Bool ->
  AssessmentRunNotification
newAssessmentRunNotification pDate_ pEvent_ pError_ =
  AssessmentRunNotification'
    { message = Core.Nothing,
      snsPublishStatusCode = Core.Nothing,
      snsTopicArn = Core.Nothing,
      date = Core._Time Lens.# pDate_,
      event = pEvent_,
      error = pError_
    }

-- | The message included in the notification.
assessmentRunNotification_message :: Lens.Lens' AssessmentRunNotification (Core.Maybe Core.Text)
assessmentRunNotification_message = Lens.lens (\AssessmentRunNotification' {message} -> message) (\s@AssessmentRunNotification' {} a -> s {message = a} :: AssessmentRunNotification)

-- | The status code of the SNS notification.
assessmentRunNotification_snsPublishStatusCode :: Lens.Lens' AssessmentRunNotification (Core.Maybe AssessmentRunNotificationSnsStatusCode)
assessmentRunNotification_snsPublishStatusCode = Lens.lens (\AssessmentRunNotification' {snsPublishStatusCode} -> snsPublishStatusCode) (\s@AssessmentRunNotification' {} a -> s {snsPublishStatusCode = a} :: AssessmentRunNotification)

-- | The SNS topic to which the SNS notification is sent.
assessmentRunNotification_snsTopicArn :: Lens.Lens' AssessmentRunNotification (Core.Maybe Core.Text)
assessmentRunNotification_snsTopicArn = Lens.lens (\AssessmentRunNotification' {snsTopicArn} -> snsTopicArn) (\s@AssessmentRunNotification' {} a -> s {snsTopicArn = a} :: AssessmentRunNotification)

-- | The date of the notification.
assessmentRunNotification_date :: Lens.Lens' AssessmentRunNotification Core.UTCTime
assessmentRunNotification_date = Lens.lens (\AssessmentRunNotification' {date} -> date) (\s@AssessmentRunNotification' {} a -> s {date = a} :: AssessmentRunNotification) Core.. Core._Time

-- | The event for which a notification is sent.
assessmentRunNotification_event :: Lens.Lens' AssessmentRunNotification InspectorEvent
assessmentRunNotification_event = Lens.lens (\AssessmentRunNotification' {event} -> event) (\s@AssessmentRunNotification' {} a -> s {event = a} :: AssessmentRunNotification)

-- | The Boolean value that specifies whether the notification represents an
-- error.
assessmentRunNotification_error :: Lens.Lens' AssessmentRunNotification Core.Bool
assessmentRunNotification_error = Lens.lens (\AssessmentRunNotification' {error} -> error) (\s@AssessmentRunNotification' {} a -> s {error = a} :: AssessmentRunNotification)

instance Core.FromJSON AssessmentRunNotification where
  parseJSON =
    Core.withObject
      "AssessmentRunNotification"
      ( \x ->
          AssessmentRunNotification'
            Core.<$> (x Core..:? "message")
            Core.<*> (x Core..:? "snsPublishStatusCode")
            Core.<*> (x Core..:? "snsTopicArn")
            Core.<*> (x Core..: "date")
            Core.<*> (x Core..: "event")
            Core.<*> (x Core..: "error")
      )

instance Core.Hashable AssessmentRunNotification

instance Core.NFData AssessmentRunNotification
