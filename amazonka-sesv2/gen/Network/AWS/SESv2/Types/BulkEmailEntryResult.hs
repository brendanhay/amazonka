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
-- Module      : Network.AWS.SESv2.Types.BulkEmailEntryResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SESv2.Types.BulkEmailEntryResult where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SESv2.Types.BulkEmailStatus

-- | The result of the @SendBulkEmail@ operation of each specified
-- @BulkEmailEntry@.
--
-- /See:/ 'newBulkEmailEntryResult' smart constructor.
data BulkEmailEntryResult = BulkEmailEntryResult'
  { -- | The status of a message sent using the @SendBulkTemplatedEmail@
    -- operation.
    --
    -- Possible values for this parameter include:
    --
    -- -   SUCCESS: Amazon SES accepted the message, and will attempt to
    --     deliver it to the recipients.
    --
    -- -   MESSAGE_REJECTED: The message was rejected because it contained a
    --     virus.
    --
    -- -   MAIL_FROM_DOMAIN_NOT_VERIFIED: The sender\'s email address or domain
    --     was not verified.
    --
    -- -   CONFIGURATION_SET_DOES_NOT_EXIST: The configuration set you
    --     specified does not exist.
    --
    -- -   TEMPLATE_DOES_NOT_EXIST: The template you specified does not exist.
    --
    -- -   ACCOUNT_SUSPENDED: Your account has been shut down because of issues
    --     related to your email sending practices.
    --
    -- -   ACCOUNT_THROTTLED: The number of emails you can send has been
    --     reduced because your account has exceeded its allocated sending
    --     limit.
    --
    -- -   ACCOUNT_DAILY_QUOTA_EXCEEDED: You have reached or exceeded the
    --     maximum number of emails you can send from your account in a 24-hour
    --     period.
    --
    -- -   INVALID_SENDING_POOL_NAME: The configuration set you specified
    --     refers to an IP pool that does not exist.
    --
    -- -   ACCOUNT_SENDING_PAUSED: Email sending for the Amazon SES account was
    --     disabled using the
    --     <https://docs.aws.amazon.com/ses/latest/APIReference/API_UpdateAccountSendingEnabled.html UpdateAccountSendingEnabled>
    --     operation.
    --
    -- -   CONFIGURATION_SET_SENDING_PAUSED: Email sending for this
    --     configuration set was disabled using the
    --     <https://docs.aws.amazon.com/ses/latest/APIReference/API_UpdateConfigurationSetSendingEnabled.html UpdateConfigurationSetSendingEnabled>
    --     operation.
    --
    -- -   INVALID_PARAMETER_VALUE: One or more of the parameters you specified
    --     when calling this operation was invalid. See the error message for
    --     additional information.
    --
    -- -   TRANSIENT_FAILURE: Amazon SES was unable to process your request
    --     because of a temporary issue.
    --
    -- -   FAILED: Amazon SES was unable to process your request. See the error
    --     message for additional information.
    status :: Prelude.Maybe BulkEmailStatus,
    -- | The unique message identifier returned from the @SendBulkTemplatedEmail@
    -- operation.
    messageId :: Prelude.Maybe Prelude.Text,
    -- | A description of an error that prevented a message being sent using the
    -- @SendBulkTemplatedEmail@ operation.
    error :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BulkEmailEntryResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'bulkEmailEntryResult_status' - The status of a message sent using the @SendBulkTemplatedEmail@
-- operation.
--
-- Possible values for this parameter include:
--
-- -   SUCCESS: Amazon SES accepted the message, and will attempt to
--     deliver it to the recipients.
--
-- -   MESSAGE_REJECTED: The message was rejected because it contained a
--     virus.
--
-- -   MAIL_FROM_DOMAIN_NOT_VERIFIED: The sender\'s email address or domain
--     was not verified.
--
-- -   CONFIGURATION_SET_DOES_NOT_EXIST: The configuration set you
--     specified does not exist.
--
-- -   TEMPLATE_DOES_NOT_EXIST: The template you specified does not exist.
--
-- -   ACCOUNT_SUSPENDED: Your account has been shut down because of issues
--     related to your email sending practices.
--
-- -   ACCOUNT_THROTTLED: The number of emails you can send has been
--     reduced because your account has exceeded its allocated sending
--     limit.
--
-- -   ACCOUNT_DAILY_QUOTA_EXCEEDED: You have reached or exceeded the
--     maximum number of emails you can send from your account in a 24-hour
--     period.
--
-- -   INVALID_SENDING_POOL_NAME: The configuration set you specified
--     refers to an IP pool that does not exist.
--
-- -   ACCOUNT_SENDING_PAUSED: Email sending for the Amazon SES account was
--     disabled using the
--     <https://docs.aws.amazon.com/ses/latest/APIReference/API_UpdateAccountSendingEnabled.html UpdateAccountSendingEnabled>
--     operation.
--
-- -   CONFIGURATION_SET_SENDING_PAUSED: Email sending for this
--     configuration set was disabled using the
--     <https://docs.aws.amazon.com/ses/latest/APIReference/API_UpdateConfigurationSetSendingEnabled.html UpdateConfigurationSetSendingEnabled>
--     operation.
--
-- -   INVALID_PARAMETER_VALUE: One or more of the parameters you specified
--     when calling this operation was invalid. See the error message for
--     additional information.
--
-- -   TRANSIENT_FAILURE: Amazon SES was unable to process your request
--     because of a temporary issue.
--
-- -   FAILED: Amazon SES was unable to process your request. See the error
--     message for additional information.
--
-- 'messageId', 'bulkEmailEntryResult_messageId' - The unique message identifier returned from the @SendBulkTemplatedEmail@
-- operation.
--
-- 'error', 'bulkEmailEntryResult_error' - A description of an error that prevented a message being sent using the
-- @SendBulkTemplatedEmail@ operation.
newBulkEmailEntryResult ::
  BulkEmailEntryResult
newBulkEmailEntryResult =
  BulkEmailEntryResult'
    { status = Prelude.Nothing,
      messageId = Prelude.Nothing,
      error = Prelude.Nothing
    }

-- | The status of a message sent using the @SendBulkTemplatedEmail@
-- operation.
--
-- Possible values for this parameter include:
--
-- -   SUCCESS: Amazon SES accepted the message, and will attempt to
--     deliver it to the recipients.
--
-- -   MESSAGE_REJECTED: The message was rejected because it contained a
--     virus.
--
-- -   MAIL_FROM_DOMAIN_NOT_VERIFIED: The sender\'s email address or domain
--     was not verified.
--
-- -   CONFIGURATION_SET_DOES_NOT_EXIST: The configuration set you
--     specified does not exist.
--
-- -   TEMPLATE_DOES_NOT_EXIST: The template you specified does not exist.
--
-- -   ACCOUNT_SUSPENDED: Your account has been shut down because of issues
--     related to your email sending practices.
--
-- -   ACCOUNT_THROTTLED: The number of emails you can send has been
--     reduced because your account has exceeded its allocated sending
--     limit.
--
-- -   ACCOUNT_DAILY_QUOTA_EXCEEDED: You have reached or exceeded the
--     maximum number of emails you can send from your account in a 24-hour
--     period.
--
-- -   INVALID_SENDING_POOL_NAME: The configuration set you specified
--     refers to an IP pool that does not exist.
--
-- -   ACCOUNT_SENDING_PAUSED: Email sending for the Amazon SES account was
--     disabled using the
--     <https://docs.aws.amazon.com/ses/latest/APIReference/API_UpdateAccountSendingEnabled.html UpdateAccountSendingEnabled>
--     operation.
--
-- -   CONFIGURATION_SET_SENDING_PAUSED: Email sending for this
--     configuration set was disabled using the
--     <https://docs.aws.amazon.com/ses/latest/APIReference/API_UpdateConfigurationSetSendingEnabled.html UpdateConfigurationSetSendingEnabled>
--     operation.
--
-- -   INVALID_PARAMETER_VALUE: One or more of the parameters you specified
--     when calling this operation was invalid. See the error message for
--     additional information.
--
-- -   TRANSIENT_FAILURE: Amazon SES was unable to process your request
--     because of a temporary issue.
--
-- -   FAILED: Amazon SES was unable to process your request. See the error
--     message for additional information.
bulkEmailEntryResult_status :: Lens.Lens' BulkEmailEntryResult (Prelude.Maybe BulkEmailStatus)
bulkEmailEntryResult_status = Lens.lens (\BulkEmailEntryResult' {status} -> status) (\s@BulkEmailEntryResult' {} a -> s {status = a} :: BulkEmailEntryResult)

-- | The unique message identifier returned from the @SendBulkTemplatedEmail@
-- operation.
bulkEmailEntryResult_messageId :: Lens.Lens' BulkEmailEntryResult (Prelude.Maybe Prelude.Text)
bulkEmailEntryResult_messageId = Lens.lens (\BulkEmailEntryResult' {messageId} -> messageId) (\s@BulkEmailEntryResult' {} a -> s {messageId = a} :: BulkEmailEntryResult)

-- | A description of an error that prevented a message being sent using the
-- @SendBulkTemplatedEmail@ operation.
bulkEmailEntryResult_error :: Lens.Lens' BulkEmailEntryResult (Prelude.Maybe Prelude.Text)
bulkEmailEntryResult_error = Lens.lens (\BulkEmailEntryResult' {error} -> error) (\s@BulkEmailEntryResult' {} a -> s {error = a} :: BulkEmailEntryResult)

instance Core.FromJSON BulkEmailEntryResult where
  parseJSON =
    Core.withObject
      "BulkEmailEntryResult"
      ( \x ->
          BulkEmailEntryResult'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "MessageId")
            Prelude.<*> (x Core..:? "Error")
      )

instance Prelude.Hashable BulkEmailEntryResult

instance Prelude.NFData BulkEmailEntryResult
