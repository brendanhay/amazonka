{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.BulkEmailDestinationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.BulkEmailDestinationStatus where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SES.Types.BulkEmailStatus

-- | An object that contains the response from the @SendBulkTemplatedEmail@ operation.
--
--
--
-- /See:/ 'bulkEmailDestinationStatus' smart constructor.
data BulkEmailDestinationStatus = BulkEmailDestinationStatus'
  { _bedsStatus ::
      !(Maybe BulkEmailStatus),
    _bedsError :: !(Maybe Text),
    _bedsMessageId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BulkEmailDestinationStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bedsStatus' - The status of a message sent using the @SendBulkTemplatedEmail@ operation. Possible values for this parameter include:     * @Success@ : Amazon SES accepted the message, and will attempt to deliver it to the recipients.     * @MessageRejected@ : The message was rejected because it contained a virus.     * @MailFromDomainNotVerified@ : The sender's email address or domain was not verified.     * @ConfigurationSetDoesNotExist@ : The configuration set you specified does not exist.     * @TemplateDoesNotExist@ : The template you specified does not exist.     * @AccountSuspended@ : Your account has been shut down because of issues related to your email sending practices.     * @AccountThrottled@ : The number of emails you can send has been reduced because your account has exceeded its allocated sending limit.     * @AccountDailyQuotaExceeded@ : You have reached or exceeded the maximum number of emails you can send from your account in a 24-hour period.     * @InvalidSendingPoolName@ : The configuration set you specified refers to an IP pool that does not exist.     * @AccountSendingPaused@ : Email sending for the Amazon SES account was disabled using the 'UpdateAccountSendingEnabled' operation.     * @ConfigurationSetSendingPaused@ : Email sending for this configuration set was disabled using the 'UpdateConfigurationSetSendingEnabled' operation.     * @InvalidParameterValue@ : One or more of the parameters you specified when calling this operation was invalid. See the error message for additional information.     * @TransientFailure@ : Amazon SES was unable to process your request because of a temporary issue.     * @Failed@ : Amazon SES was unable to process your request. See the error message for additional information.
--
-- * 'bedsError' - A description of an error that prevented a message being sent using the @SendBulkTemplatedEmail@ operation.
--
-- * 'bedsMessageId' - The unique message identifier returned from the @SendBulkTemplatedEmail@ operation.
bulkEmailDestinationStatus ::
  BulkEmailDestinationStatus
bulkEmailDestinationStatus =
  BulkEmailDestinationStatus'
    { _bedsStatus = Nothing,
      _bedsError = Nothing,
      _bedsMessageId = Nothing
    }

-- | The status of a message sent using the @SendBulkTemplatedEmail@ operation. Possible values for this parameter include:     * @Success@ : Amazon SES accepted the message, and will attempt to deliver it to the recipients.     * @MessageRejected@ : The message was rejected because it contained a virus.     * @MailFromDomainNotVerified@ : The sender's email address or domain was not verified.     * @ConfigurationSetDoesNotExist@ : The configuration set you specified does not exist.     * @TemplateDoesNotExist@ : The template you specified does not exist.     * @AccountSuspended@ : Your account has been shut down because of issues related to your email sending practices.     * @AccountThrottled@ : The number of emails you can send has been reduced because your account has exceeded its allocated sending limit.     * @AccountDailyQuotaExceeded@ : You have reached or exceeded the maximum number of emails you can send from your account in a 24-hour period.     * @InvalidSendingPoolName@ : The configuration set you specified refers to an IP pool that does not exist.     * @AccountSendingPaused@ : Email sending for the Amazon SES account was disabled using the 'UpdateAccountSendingEnabled' operation.     * @ConfigurationSetSendingPaused@ : Email sending for this configuration set was disabled using the 'UpdateConfigurationSetSendingEnabled' operation.     * @InvalidParameterValue@ : One or more of the parameters you specified when calling this operation was invalid. See the error message for additional information.     * @TransientFailure@ : Amazon SES was unable to process your request because of a temporary issue.     * @Failed@ : Amazon SES was unable to process your request. See the error message for additional information.
bedsStatus :: Lens' BulkEmailDestinationStatus (Maybe BulkEmailStatus)
bedsStatus = lens _bedsStatus (\s a -> s {_bedsStatus = a})

-- | A description of an error that prevented a message being sent using the @SendBulkTemplatedEmail@ operation.
bedsError :: Lens' BulkEmailDestinationStatus (Maybe Text)
bedsError = lens _bedsError (\s a -> s {_bedsError = a})

-- | The unique message identifier returned from the @SendBulkTemplatedEmail@ operation.
bedsMessageId :: Lens' BulkEmailDestinationStatus (Maybe Text)
bedsMessageId = lens _bedsMessageId (\s a -> s {_bedsMessageId = a})

instance FromXML BulkEmailDestinationStatus where
  parseXML x =
    BulkEmailDestinationStatus'
      <$> (x .@? "Status") <*> (x .@? "Error") <*> (x .@? "MessageId")

instance Hashable BulkEmailDestinationStatus

instance NFData BulkEmailDestinationStatus
