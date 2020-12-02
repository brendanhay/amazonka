{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.IdentityNotificationAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.IdentityNotificationAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the notification attributes of an identity, including whether an identity has Amazon Simple Notification Service (Amazon SNS) topics set for bounce, complaint, and/or delivery notifications, and whether feedback forwarding is enabled for bounce and complaint notifications.
--
--
--
-- /See:/ 'identityNotificationAttributes' smart constructor.
data IdentityNotificationAttributes = IdentityNotificationAttributes'
  { _inaHeadersInDeliveryNotificationsEnabled ::
      !(Maybe Bool),
    _inaHeadersInComplaintNotificationsEnabled ::
      !(Maybe Bool),
    _inaHeadersInBounceNotificationsEnabled ::
      !(Maybe Bool),
    _inaBounceTopic :: !Text,
    _inaComplaintTopic :: !Text,
    _inaDeliveryTopic :: !Text,
    _inaForwardingEnabled ::
      !Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'IdentityNotificationAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'inaHeadersInDeliveryNotificationsEnabled' - Describes whether Amazon SES includes the original email headers in Amazon SNS notifications of type @Delivery@ . A value of @true@ specifies that Amazon SES will include headers in delivery notifications, and a value of @false@ specifies that Amazon SES will not include headers in delivery notifications.
--
-- * 'inaHeadersInComplaintNotificationsEnabled' - Describes whether Amazon SES includes the original email headers in Amazon SNS notifications of type @Complaint@ . A value of @true@ specifies that Amazon SES will include headers in complaint notifications, and a value of @false@ specifies that Amazon SES will not include headers in complaint notifications.
--
-- * 'inaHeadersInBounceNotificationsEnabled' - Describes whether Amazon SES includes the original email headers in Amazon SNS notifications of type @Bounce@ . A value of @true@ specifies that Amazon SES will include headers in bounce notifications, and a value of @false@ specifies that Amazon SES will not include headers in bounce notifications.
--
-- * 'inaBounceTopic' - The Amazon Resource Name (ARN) of the Amazon SNS topic where Amazon SES will publish bounce notifications.
--
-- * 'inaComplaintTopic' - The Amazon Resource Name (ARN) of the Amazon SNS topic where Amazon SES will publish complaint notifications.
--
-- * 'inaDeliveryTopic' - The Amazon Resource Name (ARN) of the Amazon SNS topic where Amazon SES will publish delivery notifications.
--
-- * 'inaForwardingEnabled' - Describes whether Amazon SES will forward bounce and complaint notifications as email. @true@ indicates that Amazon SES will forward bounce and complaint notifications as email, while @false@ indicates that bounce and complaint notifications will be published only to the specified bounce and complaint Amazon SNS topics.
identityNotificationAttributes ::
  -- | 'inaBounceTopic'
  Text ->
  -- | 'inaComplaintTopic'
  Text ->
  -- | 'inaDeliveryTopic'
  Text ->
  -- | 'inaForwardingEnabled'
  Bool ->
  IdentityNotificationAttributes
identityNotificationAttributes
  pBounceTopic_
  pComplaintTopic_
  pDeliveryTopic_
  pForwardingEnabled_ =
    IdentityNotificationAttributes'
      { _inaHeadersInDeliveryNotificationsEnabled =
          Nothing,
        _inaHeadersInComplaintNotificationsEnabled = Nothing,
        _inaHeadersInBounceNotificationsEnabled = Nothing,
        _inaBounceTopic = pBounceTopic_,
        _inaComplaintTopic = pComplaintTopic_,
        _inaDeliveryTopic = pDeliveryTopic_,
        _inaForwardingEnabled = pForwardingEnabled_
      }

-- | Describes whether Amazon SES includes the original email headers in Amazon SNS notifications of type @Delivery@ . A value of @true@ specifies that Amazon SES will include headers in delivery notifications, and a value of @false@ specifies that Amazon SES will not include headers in delivery notifications.
inaHeadersInDeliveryNotificationsEnabled :: Lens' IdentityNotificationAttributes (Maybe Bool)
inaHeadersInDeliveryNotificationsEnabled = lens _inaHeadersInDeliveryNotificationsEnabled (\s a -> s {_inaHeadersInDeliveryNotificationsEnabled = a})

-- | Describes whether Amazon SES includes the original email headers in Amazon SNS notifications of type @Complaint@ . A value of @true@ specifies that Amazon SES will include headers in complaint notifications, and a value of @false@ specifies that Amazon SES will not include headers in complaint notifications.
inaHeadersInComplaintNotificationsEnabled :: Lens' IdentityNotificationAttributes (Maybe Bool)
inaHeadersInComplaintNotificationsEnabled = lens _inaHeadersInComplaintNotificationsEnabled (\s a -> s {_inaHeadersInComplaintNotificationsEnabled = a})

-- | Describes whether Amazon SES includes the original email headers in Amazon SNS notifications of type @Bounce@ . A value of @true@ specifies that Amazon SES will include headers in bounce notifications, and a value of @false@ specifies that Amazon SES will not include headers in bounce notifications.
inaHeadersInBounceNotificationsEnabled :: Lens' IdentityNotificationAttributes (Maybe Bool)
inaHeadersInBounceNotificationsEnabled = lens _inaHeadersInBounceNotificationsEnabled (\s a -> s {_inaHeadersInBounceNotificationsEnabled = a})

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic where Amazon SES will publish bounce notifications.
inaBounceTopic :: Lens' IdentityNotificationAttributes Text
inaBounceTopic = lens _inaBounceTopic (\s a -> s {_inaBounceTopic = a})

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic where Amazon SES will publish complaint notifications.
inaComplaintTopic :: Lens' IdentityNotificationAttributes Text
inaComplaintTopic = lens _inaComplaintTopic (\s a -> s {_inaComplaintTopic = a})

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic where Amazon SES will publish delivery notifications.
inaDeliveryTopic :: Lens' IdentityNotificationAttributes Text
inaDeliveryTopic = lens _inaDeliveryTopic (\s a -> s {_inaDeliveryTopic = a})

-- | Describes whether Amazon SES will forward bounce and complaint notifications as email. @true@ indicates that Amazon SES will forward bounce and complaint notifications as email, while @false@ indicates that bounce and complaint notifications will be published only to the specified bounce and complaint Amazon SNS topics.
inaForwardingEnabled :: Lens' IdentityNotificationAttributes Bool
inaForwardingEnabled = lens _inaForwardingEnabled (\s a -> s {_inaForwardingEnabled = a})

instance FromXML IdentityNotificationAttributes where
  parseXML x =
    IdentityNotificationAttributes'
      <$> (x .@? "HeadersInDeliveryNotificationsEnabled")
      <*> (x .@? "HeadersInComplaintNotificationsEnabled")
      <*> (x .@? "HeadersInBounceNotificationsEnabled")
      <*> (x .@ "BounceTopic")
      <*> (x .@ "ComplaintTopic")
      <*> (x .@ "DeliveryTopic")
      <*> (x .@ "ForwardingEnabled")

instance Hashable IdentityNotificationAttributes

instance NFData IdentityNotificationAttributes
