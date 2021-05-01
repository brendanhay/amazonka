{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.Inspector.Types.AssessmentRunNotificationSnsStatusCode
import Network.AWS.Inspector.Types.InspectorEvent
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Used as one of the elements of the AssessmentRun data type.
--
-- /See:/ 'newAssessmentRunNotification' smart constructor.
data AssessmentRunNotification = AssessmentRunNotification'
  { -- | The message included in the notification.
    message :: Prelude.Maybe Prelude.Text,
    -- | The status code of the SNS notification.
    snsPublishStatusCode :: Prelude.Maybe AssessmentRunNotificationSnsStatusCode,
    -- | The SNS topic to which the SNS notification is sent.
    snsTopicArn :: Prelude.Maybe Prelude.Text,
    -- | The date of the notification.
    date :: Prelude.POSIX,
    -- | The event for which a notification is sent.
    event :: InspectorEvent,
    -- | The Boolean value that specifies whether the notification represents an
    -- error.
    error :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.UTCTime ->
  -- | 'event'
  InspectorEvent ->
  -- | 'error'
  Prelude.Bool ->
  AssessmentRunNotification
newAssessmentRunNotification pDate_ pEvent_ pError_ =
  AssessmentRunNotification'
    { message =
        Prelude.Nothing,
      snsPublishStatusCode = Prelude.Nothing,
      snsTopicArn = Prelude.Nothing,
      date = Prelude._Time Lens.# pDate_,
      event = pEvent_,
      error = pError_
    }

-- | The message included in the notification.
assessmentRunNotification_message :: Lens.Lens' AssessmentRunNotification (Prelude.Maybe Prelude.Text)
assessmentRunNotification_message = Lens.lens (\AssessmentRunNotification' {message} -> message) (\s@AssessmentRunNotification' {} a -> s {message = a} :: AssessmentRunNotification)

-- | The status code of the SNS notification.
assessmentRunNotification_snsPublishStatusCode :: Lens.Lens' AssessmentRunNotification (Prelude.Maybe AssessmentRunNotificationSnsStatusCode)
assessmentRunNotification_snsPublishStatusCode = Lens.lens (\AssessmentRunNotification' {snsPublishStatusCode} -> snsPublishStatusCode) (\s@AssessmentRunNotification' {} a -> s {snsPublishStatusCode = a} :: AssessmentRunNotification)

-- | The SNS topic to which the SNS notification is sent.
assessmentRunNotification_snsTopicArn :: Lens.Lens' AssessmentRunNotification (Prelude.Maybe Prelude.Text)
assessmentRunNotification_snsTopicArn = Lens.lens (\AssessmentRunNotification' {snsTopicArn} -> snsTopicArn) (\s@AssessmentRunNotification' {} a -> s {snsTopicArn = a} :: AssessmentRunNotification)

-- | The date of the notification.
assessmentRunNotification_date :: Lens.Lens' AssessmentRunNotification Prelude.UTCTime
assessmentRunNotification_date = Lens.lens (\AssessmentRunNotification' {date} -> date) (\s@AssessmentRunNotification' {} a -> s {date = a} :: AssessmentRunNotification) Prelude.. Prelude._Time

-- | The event for which a notification is sent.
assessmentRunNotification_event :: Lens.Lens' AssessmentRunNotification InspectorEvent
assessmentRunNotification_event = Lens.lens (\AssessmentRunNotification' {event} -> event) (\s@AssessmentRunNotification' {} a -> s {event = a} :: AssessmentRunNotification)

-- | The Boolean value that specifies whether the notification represents an
-- error.
assessmentRunNotification_error :: Lens.Lens' AssessmentRunNotification Prelude.Bool
assessmentRunNotification_error = Lens.lens (\AssessmentRunNotification' {error} -> error) (\s@AssessmentRunNotification' {} a -> s {error = a} :: AssessmentRunNotification)

instance Prelude.FromJSON AssessmentRunNotification where
  parseJSON =
    Prelude.withObject
      "AssessmentRunNotification"
      ( \x ->
          AssessmentRunNotification'
            Prelude.<$> (x Prelude..:? "message")
            Prelude.<*> (x Prelude..:? "snsPublishStatusCode")
            Prelude.<*> (x Prelude..:? "snsTopicArn")
            Prelude.<*> (x Prelude..: "date")
            Prelude.<*> (x Prelude..: "event")
            Prelude.<*> (x Prelude..: "error")
      )

instance Prelude.Hashable AssessmentRunNotification

instance Prelude.NFData AssessmentRunNotification
