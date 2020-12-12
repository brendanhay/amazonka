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
    bedsStatus,
    bedsError,
    bedsMessageId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SES.Types.BulkEmailStatus

-- | An object that contains the response from the @SendBulkTemplatedEmail@ operation.
--
-- /See:/ 'mkBulkEmailDestinationStatus' smart constructor.
data BulkEmailDestinationStatus = BulkEmailDestinationStatus'
  { status ::
      Lude.Maybe BulkEmailStatus,
    error :: Lude.Maybe Lude.Text,
    messageId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BulkEmailDestinationStatus' with the minimum fields required to make a request.
--
-- * 'error' - A description of an error that prevented a message being sent using the @SendBulkTemplatedEmail@ operation.
-- * 'messageId' - The unique message identifier returned from the @SendBulkTemplatedEmail@ operation.
-- * 'status' - The status of a message sent using the @SendBulkTemplatedEmail@ operation.
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
mkBulkEmailDestinationStatus ::
  BulkEmailDestinationStatus
mkBulkEmailDestinationStatus =
  BulkEmailDestinationStatus'
    { status = Lude.Nothing,
      error = Lude.Nothing,
      messageId = Lude.Nothing
    }

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
bedsStatus :: Lens.Lens' BulkEmailDestinationStatus (Lude.Maybe BulkEmailStatus)
bedsStatus = Lens.lens (status :: BulkEmailDestinationStatus -> Lude.Maybe BulkEmailStatus) (\s a -> s {status = a} :: BulkEmailDestinationStatus)
{-# DEPRECATED bedsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | A description of an error that prevented a message being sent using the @SendBulkTemplatedEmail@ operation.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bedsError :: Lens.Lens' BulkEmailDestinationStatus (Lude.Maybe Lude.Text)
bedsError = Lens.lens (error :: BulkEmailDestinationStatus -> Lude.Maybe Lude.Text) (\s a -> s {error = a} :: BulkEmailDestinationStatus)
{-# DEPRECATED bedsError "Use generic-lens or generic-optics with 'error' instead." #-}

-- | The unique message identifier returned from the @SendBulkTemplatedEmail@ operation.
--
-- /Note:/ Consider using 'messageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bedsMessageId :: Lens.Lens' BulkEmailDestinationStatus (Lude.Maybe Lude.Text)
bedsMessageId = Lens.lens (messageId :: BulkEmailDestinationStatus -> Lude.Maybe Lude.Text) (\s a -> s {messageId = a} :: BulkEmailDestinationStatus)
{-# DEPRECATED bedsMessageId "Use generic-lens or generic-optics with 'messageId' instead." #-}

instance Lude.FromXML BulkEmailDestinationStatus where
  parseXML x =
    BulkEmailDestinationStatus'
      Lude.<$> (x Lude..@? "Status")
      Lude.<*> (x Lude..@? "Error")
      Lude.<*> (x Lude..@? "MessageId")
