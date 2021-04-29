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
-- Module      : Network.AWS.SES.Types.BulkEmailDestinationStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.BulkEmailDestinationStatus where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SES.Types.BulkEmailStatus

-- | An object that contains the response from the @SendBulkTemplatedEmail@
-- operation.
--
-- /See:/ 'newBulkEmailDestinationStatus' smart constructor.
data BulkEmailDestinationStatus = BulkEmailDestinationStatus'
  { -- | The status of a message sent using the @SendBulkTemplatedEmail@
    -- operation.
    --
    -- Possible values for this parameter include:
    --
    -- -   @Success@: Amazon SES accepted the message, and will attempt to
    --     deliver it to the recipients.
    --
    -- -   @MessageRejected@: The message was rejected because it contained a
    --     virus.
    --
    -- -   @MailFromDomainNotVerified@: The sender\'s email address or domain
    --     was not verified.
    --
    -- -   @ConfigurationSetDoesNotExist@: The configuration set you specified
    --     does not exist.
    --
    -- -   @TemplateDoesNotExist@: The template you specified does not exist.
    --
    -- -   @AccountSuspended@: Your account has been shut down because of
    --     issues related to your email sending practices.
    --
    -- -   @AccountThrottled@: The number of emails you can send has been
    --     reduced because your account has exceeded its allocated sending
    --     limit.
    --
    -- -   @AccountDailyQuotaExceeded@: You have reached or exceeded the
    --     maximum number of emails you can send from your account in a 24-hour
    --     period.
    --
    -- -   @InvalidSendingPoolName@: The configuration set you specified refers
    --     to an IP pool that does not exist.
    --
    -- -   @AccountSendingPaused@: Email sending for the Amazon SES account was
    --     disabled using the UpdateAccountSendingEnabled operation.
    --
    -- -   @ConfigurationSetSendingPaused@: Email sending for this
    --     configuration set was disabled using the
    --     UpdateConfigurationSetSendingEnabled operation.
    --
    -- -   @InvalidParameterValue@: One or more of the parameters you specified
    --     when calling this operation was invalid. See the error message for
    --     additional information.
    --
    -- -   @TransientFailure@: Amazon SES was unable to process your request
    --     because of a temporary issue.
    --
    -- -   @Failed@: Amazon SES was unable to process your request. See the
    --     error message for additional information.
    status :: Prelude.Maybe BulkEmailStatus,
    -- | The unique message identifier returned from the @SendBulkTemplatedEmail@
    -- operation.
    messageId :: Prelude.Maybe Prelude.Text,
    -- | A description of an error that prevented a message being sent using the
    -- @SendBulkTemplatedEmail@ operation.
    error :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BulkEmailDestinationStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'bulkEmailDestinationStatus_status' - The status of a message sent using the @SendBulkTemplatedEmail@
-- operation.
--
-- Possible values for this parameter include:
--
-- -   @Success@: Amazon SES accepted the message, and will attempt to
--     deliver it to the recipients.
--
-- -   @MessageRejected@: The message was rejected because it contained a
--     virus.
--
-- -   @MailFromDomainNotVerified@: The sender\'s email address or domain
--     was not verified.
--
-- -   @ConfigurationSetDoesNotExist@: The configuration set you specified
--     does not exist.
--
-- -   @TemplateDoesNotExist@: The template you specified does not exist.
--
-- -   @AccountSuspended@: Your account has been shut down because of
--     issues related to your email sending practices.
--
-- -   @AccountThrottled@: The number of emails you can send has been
--     reduced because your account has exceeded its allocated sending
--     limit.
--
-- -   @AccountDailyQuotaExceeded@: You have reached or exceeded the
--     maximum number of emails you can send from your account in a 24-hour
--     period.
--
-- -   @InvalidSendingPoolName@: The configuration set you specified refers
--     to an IP pool that does not exist.
--
-- -   @AccountSendingPaused@: Email sending for the Amazon SES account was
--     disabled using the UpdateAccountSendingEnabled operation.
--
-- -   @ConfigurationSetSendingPaused@: Email sending for this
--     configuration set was disabled using the
--     UpdateConfigurationSetSendingEnabled operation.
--
-- -   @InvalidParameterValue@: One or more of the parameters you specified
--     when calling this operation was invalid. See the error message for
--     additional information.
--
-- -   @TransientFailure@: Amazon SES was unable to process your request
--     because of a temporary issue.
--
-- -   @Failed@: Amazon SES was unable to process your request. See the
--     error message for additional information.
--
-- 'messageId', 'bulkEmailDestinationStatus_messageId' - The unique message identifier returned from the @SendBulkTemplatedEmail@
-- operation.
--
-- 'error', 'bulkEmailDestinationStatus_error' - A description of an error that prevented a message being sent using the
-- @SendBulkTemplatedEmail@ operation.
newBulkEmailDestinationStatus ::
  BulkEmailDestinationStatus
newBulkEmailDestinationStatus =
  BulkEmailDestinationStatus'
    { status =
        Prelude.Nothing,
      messageId = Prelude.Nothing,
      error = Prelude.Nothing
    }

-- | The status of a message sent using the @SendBulkTemplatedEmail@
-- operation.
--
-- Possible values for this parameter include:
--
-- -   @Success@: Amazon SES accepted the message, and will attempt to
--     deliver it to the recipients.
--
-- -   @MessageRejected@: The message was rejected because it contained a
--     virus.
--
-- -   @MailFromDomainNotVerified@: The sender\'s email address or domain
--     was not verified.
--
-- -   @ConfigurationSetDoesNotExist@: The configuration set you specified
--     does not exist.
--
-- -   @TemplateDoesNotExist@: The template you specified does not exist.
--
-- -   @AccountSuspended@: Your account has been shut down because of
--     issues related to your email sending practices.
--
-- -   @AccountThrottled@: The number of emails you can send has been
--     reduced because your account has exceeded its allocated sending
--     limit.
--
-- -   @AccountDailyQuotaExceeded@: You have reached or exceeded the
--     maximum number of emails you can send from your account in a 24-hour
--     period.
--
-- -   @InvalidSendingPoolName@: The configuration set you specified refers
--     to an IP pool that does not exist.
--
-- -   @AccountSendingPaused@: Email sending for the Amazon SES account was
--     disabled using the UpdateAccountSendingEnabled operation.
--
-- -   @ConfigurationSetSendingPaused@: Email sending for this
--     configuration set was disabled using the
--     UpdateConfigurationSetSendingEnabled operation.
--
-- -   @InvalidParameterValue@: One or more of the parameters you specified
--     when calling this operation was invalid. See the error message for
--     additional information.
--
-- -   @TransientFailure@: Amazon SES was unable to process your request
--     because of a temporary issue.
--
-- -   @Failed@: Amazon SES was unable to process your request. See the
--     error message for additional information.
bulkEmailDestinationStatus_status :: Lens.Lens' BulkEmailDestinationStatus (Prelude.Maybe BulkEmailStatus)
bulkEmailDestinationStatus_status = Lens.lens (\BulkEmailDestinationStatus' {status} -> status) (\s@BulkEmailDestinationStatus' {} a -> s {status = a} :: BulkEmailDestinationStatus)

-- | The unique message identifier returned from the @SendBulkTemplatedEmail@
-- operation.
bulkEmailDestinationStatus_messageId :: Lens.Lens' BulkEmailDestinationStatus (Prelude.Maybe Prelude.Text)
bulkEmailDestinationStatus_messageId = Lens.lens (\BulkEmailDestinationStatus' {messageId} -> messageId) (\s@BulkEmailDestinationStatus' {} a -> s {messageId = a} :: BulkEmailDestinationStatus)

-- | A description of an error that prevented a message being sent using the
-- @SendBulkTemplatedEmail@ operation.
bulkEmailDestinationStatus_error :: Lens.Lens' BulkEmailDestinationStatus (Prelude.Maybe Prelude.Text)
bulkEmailDestinationStatus_error = Lens.lens (\BulkEmailDestinationStatus' {error} -> error) (\s@BulkEmailDestinationStatus' {} a -> s {error = a} :: BulkEmailDestinationStatus)

instance Prelude.FromXML BulkEmailDestinationStatus where
  parseXML x =
    BulkEmailDestinationStatus'
      Prelude.<$> (x Prelude..@? "Status")
      Prelude.<*> (x Prelude..@? "MessageId")
      Prelude.<*> (x Prelude..@? "Error")

instance Prelude.Hashable BulkEmailDestinationStatus

instance Prelude.NFData BulkEmailDestinationStatus
