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
-- Module      : Amazonka.Rekognition.Types.StreamProcessorNotificationChannel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.StreamProcessorNotificationChannel where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The Amazon Simple Notification Service topic to which Amazon Rekognition
-- publishes the object detection results and completion status of a video
-- analysis operation.
--
-- Amazon Rekognition publishes a notification the first time an object of
-- interest or a person is detected in the video stream. For example, if
-- Amazon Rekognition detects a person at second 2, a pet at second 4, and
-- a person again at second 5, Amazon Rekognition sends 2 object class
-- detected notifications, one for a person at second 2 and one for a pet
-- at second 4.
--
-- Amazon Rekognition also publishes an an end-of-session notification with
-- a summary when the stream processing session is complete.
--
-- /See:/ 'newStreamProcessorNotificationChannel' smart constructor.
data StreamProcessorNotificationChannel = StreamProcessorNotificationChannel'
  { -- | The Amazon Resource Number (ARN) of the Amazon Amazon Simple
    -- Notification Service topic to which Amazon Rekognition posts the
    -- completion status.
    sNSTopicArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StreamProcessorNotificationChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sNSTopicArn', 'streamProcessorNotificationChannel_sNSTopicArn' - The Amazon Resource Number (ARN) of the Amazon Amazon Simple
-- Notification Service topic to which Amazon Rekognition posts the
-- completion status.
newStreamProcessorNotificationChannel ::
  -- | 'sNSTopicArn'
  Prelude.Text ->
  StreamProcessorNotificationChannel
newStreamProcessorNotificationChannel pSNSTopicArn_ =
  StreamProcessorNotificationChannel'
    { sNSTopicArn =
        pSNSTopicArn_
    }

-- | The Amazon Resource Number (ARN) of the Amazon Amazon Simple
-- Notification Service topic to which Amazon Rekognition posts the
-- completion status.
streamProcessorNotificationChannel_sNSTopicArn :: Lens.Lens' StreamProcessorNotificationChannel Prelude.Text
streamProcessorNotificationChannel_sNSTopicArn = Lens.lens (\StreamProcessorNotificationChannel' {sNSTopicArn} -> sNSTopicArn) (\s@StreamProcessorNotificationChannel' {} a -> s {sNSTopicArn = a} :: StreamProcessorNotificationChannel)

instance
  Core.FromJSON
    StreamProcessorNotificationChannel
  where
  parseJSON =
    Core.withObject
      "StreamProcessorNotificationChannel"
      ( \x ->
          StreamProcessorNotificationChannel'
            Prelude.<$> (x Core..: "SNSTopicArn")
      )

instance
  Prelude.Hashable
    StreamProcessorNotificationChannel
  where
  hashWithSalt
    _salt
    StreamProcessorNotificationChannel' {..} =
      _salt `Prelude.hashWithSalt` sNSTopicArn

instance
  Prelude.NFData
    StreamProcessorNotificationChannel
  where
  rnf StreamProcessorNotificationChannel' {..} =
    Prelude.rnf sNSTopicArn

instance
  Core.ToJSON
    StreamProcessorNotificationChannel
  where
  toJSON StreamProcessorNotificationChannel' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("SNSTopicArn" Core..= sNSTopicArn)]
      )
