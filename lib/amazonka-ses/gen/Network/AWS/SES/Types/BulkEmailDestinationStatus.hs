{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.BulkEmailDestinationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.BulkEmailDestinationStatus
  ( BulkEmailDestinationStatus (..),

    -- * Smart constructor
    mkBulkEmailDestinationStatus,

    -- * Lenses
    bedsError,
    bedsMessageId,
    bedsStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SES.Types.BulkEmailStatus as Types
import qualified Network.AWS.SES.Types.Error as Types
import qualified Network.AWS.SES.Types.MessageId as Types

-- | An object that contains the response from the @SendBulkTemplatedEmail@ operation.
--
-- /See:/ 'mkBulkEmailDestinationStatus' smart constructor.
data BulkEmailDestinationStatus = BulkEmailDestinationStatus'
  { -- | A description of an error that prevented a message being sent using the @SendBulkTemplatedEmail@ operation.
    error :: Core.Maybe Types.Error,
    -- | The unique message identifier returned from the @SendBulkTemplatedEmail@ operation.
    messageId :: Core.Maybe Types.MessageId,
    -- | The status of a message sent using the @SendBulkTemplatedEmail@ operation.
    --
    -- Possible values for this parameter include:
    --
    --     * @Success@ : Amazon SES accepted the message, and will attempt to deliver it to the recipients.
    --
    --
    --     * @MessageRejected@ : The message was rejected because it contained a virus.
    --
    --
    --     * @MailFromDomainNotVerified@ : The sender's email address or domain was not verified.
    --
    --
    --     * @ConfigurationSetDoesNotExist@ : The configuration set you specified does not exist.
    --
    --
    --     * @TemplateDoesNotExist@ : The template you specified does not exist.
    --
    --
    --     * @AccountSuspended@ : Your account has been shut down because of issues related to your email sending practices.
    --
    --
    --     * @AccountThrottled@ : The number of emails you can send has been reduced because your account has exceeded its allocated sending limit.
    --
    --
    --     * @AccountDailyQuotaExceeded@ : You have reached or exceeded the maximum number of emails you can send from your account in a 24-hour period.
    --
    --
    --     * @InvalidSendingPoolName@ : The configuration set you specified refers to an IP pool that does not exist.
    --
    --
    --     * @AccountSendingPaused@ : Email sending for the Amazon SES account was disabled using the 'UpdateAccountSendingEnabled' operation.
    --
    --
    --     * @ConfigurationSetSendingPaused@ : Email sending for this configuration set was disabled using the 'UpdateConfigurationSetSendingEnabled' operation.
    --
    --
    --     * @InvalidParameterValue@ : One or more of the parameters you specified when calling this operation was invalid. See the error message for additional information.
    --
    --
    --     * @TransientFailure@ : Amazon SES was unable to process your request because of a temporary issue.
    --
    --
    --     * @Failed@ : Amazon SES was unable to process your request. See the error message for additional information.
    status :: Core.Maybe Types.BulkEmailStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BulkEmailDestinationStatus' value with any optional fields omitted.
mkBulkEmailDestinationStatus ::
  BulkEmailDestinationStatus
mkBulkEmailDestinationStatus =
  BulkEmailDestinationStatus'
    { error = Core.Nothing,
      messageId = Core.Nothing,
      status = Core.Nothing
    }

-- | A description of an error that prevented a message being sent using the @SendBulkTemplatedEmail@ operation.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bedsError :: Lens.Lens' BulkEmailDestinationStatus (Core.Maybe Types.Error)
bedsError = Lens.field @"error"
{-# DEPRECATED bedsError "Use generic-lens or generic-optics with 'error' instead." #-}

-- | The unique message identifier returned from the @SendBulkTemplatedEmail@ operation.
--
-- /Note:/ Consider using 'messageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bedsMessageId :: Lens.Lens' BulkEmailDestinationStatus (Core.Maybe Types.MessageId)
bedsMessageId = Lens.field @"messageId"
{-# DEPRECATED bedsMessageId "Use generic-lens or generic-optics with 'messageId' instead." #-}

-- | The status of a message sent using the @SendBulkTemplatedEmail@ operation.
--
-- Possible values for this parameter include:
--
--     * @Success@ : Amazon SES accepted the message, and will attempt to deliver it to the recipients.
--
--
--     * @MessageRejected@ : The message was rejected because it contained a virus.
--
--
--     * @MailFromDomainNotVerified@ : The sender's email address or domain was not verified.
--
--
--     * @ConfigurationSetDoesNotExist@ : The configuration set you specified does not exist.
--
--
--     * @TemplateDoesNotExist@ : The template you specified does not exist.
--
--
--     * @AccountSuspended@ : Your account has been shut down because of issues related to your email sending practices.
--
--
--     * @AccountThrottled@ : The number of emails you can send has been reduced because your account has exceeded its allocated sending limit.
--
--
--     * @AccountDailyQuotaExceeded@ : You have reached or exceeded the maximum number of emails you can send from your account in a 24-hour period.
--
--
--     * @InvalidSendingPoolName@ : The configuration set you specified refers to an IP pool that does not exist.
--
--
--     * @AccountSendingPaused@ : Email sending for the Amazon SES account was disabled using the 'UpdateAccountSendingEnabled' operation.
--
--
--     * @ConfigurationSetSendingPaused@ : Email sending for this configuration set was disabled using the 'UpdateConfigurationSetSendingEnabled' operation.
--
--
--     * @InvalidParameterValue@ : One or more of the parameters you specified when calling this operation was invalid. See the error message for additional information.
--
--
--     * @TransientFailure@ : Amazon SES was unable to process your request because of a temporary issue.
--
--
--     * @Failed@ : Amazon SES was unable to process your request. See the error message for additional information.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bedsStatus :: Lens.Lens' BulkEmailDestinationStatus (Core.Maybe Types.BulkEmailStatus)
bedsStatus = Lens.field @"status"
{-# DEPRECATED bedsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromXML BulkEmailDestinationStatus where
  parseXML x =
    BulkEmailDestinationStatus'
      Core.<$> (x Core..@? "Error")
      Core.<*> (x Core..@? "MessageId")
      Core.<*> (x Core..@? "Status")
