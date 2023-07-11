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
-- Module      : Amazonka.Textract.Types.NotificationChannel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Textract.Types.NotificationChannel where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Amazon Simple Notification Service (Amazon SNS) topic to which
-- Amazon Textract publishes the completion status of an asynchronous
-- document operation.
--
-- /See:/ 'newNotificationChannel' smart constructor.
data NotificationChannel = NotificationChannel'
  { -- | The Amazon SNS topic that Amazon Textract posts the completion status
    -- to.
    sNSTopicArn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of an IAM role that gives Amazon Textract
    -- publishing permissions to the Amazon SNS topic.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NotificationChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sNSTopicArn', 'notificationChannel_sNSTopicArn' - The Amazon SNS topic that Amazon Textract posts the completion status
-- to.
--
-- 'roleArn', 'notificationChannel_roleArn' - The Amazon Resource Name (ARN) of an IAM role that gives Amazon Textract
-- publishing permissions to the Amazon SNS topic.
newNotificationChannel ::
  -- | 'sNSTopicArn'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  NotificationChannel
newNotificationChannel pSNSTopicArn_ pRoleArn_ =
  NotificationChannel'
    { sNSTopicArn = pSNSTopicArn_,
      roleArn = pRoleArn_
    }

-- | The Amazon SNS topic that Amazon Textract posts the completion status
-- to.
notificationChannel_sNSTopicArn :: Lens.Lens' NotificationChannel Prelude.Text
notificationChannel_sNSTopicArn = Lens.lens (\NotificationChannel' {sNSTopicArn} -> sNSTopicArn) (\s@NotificationChannel' {} a -> s {sNSTopicArn = a} :: NotificationChannel)

-- | The Amazon Resource Name (ARN) of an IAM role that gives Amazon Textract
-- publishing permissions to the Amazon SNS topic.
notificationChannel_roleArn :: Lens.Lens' NotificationChannel Prelude.Text
notificationChannel_roleArn = Lens.lens (\NotificationChannel' {roleArn} -> roleArn) (\s@NotificationChannel' {} a -> s {roleArn = a} :: NotificationChannel)

instance Prelude.Hashable NotificationChannel where
  hashWithSalt _salt NotificationChannel' {..} =
    _salt
      `Prelude.hashWithSalt` sNSTopicArn
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData NotificationChannel where
  rnf NotificationChannel' {..} =
    Prelude.rnf sNSTopicArn
      `Prelude.seq` Prelude.rnf roleArn

instance Data.ToJSON NotificationChannel where
  toJSON NotificationChannel' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("SNSTopicArn" Data..= sNSTopicArn),
            Prelude.Just ("RoleArn" Data..= roleArn)
          ]
      )
