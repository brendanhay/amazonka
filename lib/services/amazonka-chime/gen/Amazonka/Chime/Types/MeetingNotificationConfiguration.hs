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
-- Module      : Amazonka.Chime.Types.MeetingNotificationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.MeetingNotificationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The resource target configurations for receiving Amazon Chime SDK
-- meeting and attendee event notifications. The Amazon Chime SDK supports
-- resource targets located in the US East (N. Virginia) AWS Region
-- (@us-east-1@).
--
-- /See:/ 'newMeetingNotificationConfiguration' smart constructor.
data MeetingNotificationConfiguration = MeetingNotificationConfiguration'
  { -- | The SNS topic ARN.
    snsTopicArn :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The SQS queue ARN.
    sqsQueueArn :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MeetingNotificationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snsTopicArn', 'meetingNotificationConfiguration_snsTopicArn' - The SNS topic ARN.
--
-- 'sqsQueueArn', 'meetingNotificationConfiguration_sqsQueueArn' - The SQS queue ARN.
newMeetingNotificationConfiguration ::
  MeetingNotificationConfiguration
newMeetingNotificationConfiguration =
  MeetingNotificationConfiguration'
    { snsTopicArn =
        Prelude.Nothing,
      sqsQueueArn = Prelude.Nothing
    }

-- | The SNS topic ARN.
meetingNotificationConfiguration_snsTopicArn :: Lens.Lens' MeetingNotificationConfiguration (Prelude.Maybe Prelude.Text)
meetingNotificationConfiguration_snsTopicArn = Lens.lens (\MeetingNotificationConfiguration' {snsTopicArn} -> snsTopicArn) (\s@MeetingNotificationConfiguration' {} a -> s {snsTopicArn = a} :: MeetingNotificationConfiguration) Prelude.. Lens.mapping Data._Sensitive

-- | The SQS queue ARN.
meetingNotificationConfiguration_sqsQueueArn :: Lens.Lens' MeetingNotificationConfiguration (Prelude.Maybe Prelude.Text)
meetingNotificationConfiguration_sqsQueueArn = Lens.lens (\MeetingNotificationConfiguration' {sqsQueueArn} -> sqsQueueArn) (\s@MeetingNotificationConfiguration' {} a -> s {sqsQueueArn = a} :: MeetingNotificationConfiguration) Prelude.. Lens.mapping Data._Sensitive

instance
  Prelude.Hashable
    MeetingNotificationConfiguration
  where
  hashWithSalt
    _salt
    MeetingNotificationConfiguration' {..} =
      _salt `Prelude.hashWithSalt` snsTopicArn
        `Prelude.hashWithSalt` sqsQueueArn

instance
  Prelude.NFData
    MeetingNotificationConfiguration
  where
  rnf MeetingNotificationConfiguration' {..} =
    Prelude.rnf snsTopicArn
      `Prelude.seq` Prelude.rnf sqsQueueArn

instance Data.ToJSON MeetingNotificationConfiguration where
  toJSON MeetingNotificationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SnsTopicArn" Data..=) Prelude.<$> snsTopicArn,
            ("SqsQueueArn" Data..=) Prelude.<$> sqsQueueArn
          ]
      )
