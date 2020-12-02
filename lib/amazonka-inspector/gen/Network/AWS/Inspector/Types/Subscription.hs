{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.Subscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.Subscription where

import Network.AWS.Inspector.Types.EventSubscription
import Network.AWS.Lens
import Network.AWS.Prelude

-- | This data type is used as a response element in the 'ListEventSubscriptions' action.
--
--
--
-- /See:/ 'subscription' smart constructor.
data Subscription = Subscription'
  { _sResourceARN :: !Text,
    _sTopicARN :: !Text,
    _sEventSubscriptions :: !(List1 EventSubscription)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Subscription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sResourceARN' - The ARN of the assessment template that is used during the event for which the SNS notification is sent.
--
-- * 'sTopicARN' - The ARN of the Amazon Simple Notification Service (SNS) topic to which the SNS notifications are sent.
--
-- * 'sEventSubscriptions' - The list of existing event subscriptions.
subscription ::
  -- | 'sResourceARN'
  Text ->
  -- | 'sTopicARN'
  Text ->
  -- | 'sEventSubscriptions'
  NonEmpty EventSubscription ->
  Subscription
subscription pResourceARN_ pTopicARN_ pEventSubscriptions_ =
  Subscription'
    { _sResourceARN = pResourceARN_,
      _sTopicARN = pTopicARN_,
      _sEventSubscriptions = _List1 # pEventSubscriptions_
    }

-- | The ARN of the assessment template that is used during the event for which the SNS notification is sent.
sResourceARN :: Lens' Subscription Text
sResourceARN = lens _sResourceARN (\s a -> s {_sResourceARN = a})

-- | The ARN of the Amazon Simple Notification Service (SNS) topic to which the SNS notifications are sent.
sTopicARN :: Lens' Subscription Text
sTopicARN = lens _sTopicARN (\s a -> s {_sTopicARN = a})

-- | The list of existing event subscriptions.
sEventSubscriptions :: Lens' Subscription (NonEmpty EventSubscription)
sEventSubscriptions = lens _sEventSubscriptions (\s a -> s {_sEventSubscriptions = a}) . _List1

instance FromJSON Subscription where
  parseJSON =
    withObject
      "Subscription"
      ( \x ->
          Subscription'
            <$> (x .: "resourceArn")
            <*> (x .: "topicArn")
            <*> (x .: "eventSubscriptions")
      )

instance Hashable Subscription

instance NFData Subscription
