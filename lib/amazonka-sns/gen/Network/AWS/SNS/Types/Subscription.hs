{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.Types.Subscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SNS.Types.Subscription where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A wrapper type for the attributes of an Amazon SNS subscription.
--
--
--
-- /See:/ 'subscription' smart constructor.
data Subscription = Subscription'
  { _sProtocol :: !(Maybe Text),
    _sOwner :: !(Maybe Text),
    _sTopicARN :: !(Maybe Text),
    _sEndpoint :: !(Maybe Text),
    _sSubscriptionARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Subscription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sProtocol' - The subscription's protocol.
--
-- * 'sOwner' - The subscription's owner.
--
-- * 'sTopicARN' - The ARN of the subscription's topic.
--
-- * 'sEndpoint' - The subscription's endpoint (format depends on the protocol).
--
-- * 'sSubscriptionARN' - The subscription's ARN.
subscription ::
  Subscription
subscription =
  Subscription'
    { _sProtocol = Nothing,
      _sOwner = Nothing,
      _sTopicARN = Nothing,
      _sEndpoint = Nothing,
      _sSubscriptionARN = Nothing
    }

-- | The subscription's protocol.
sProtocol :: Lens' Subscription (Maybe Text)
sProtocol = lens _sProtocol (\s a -> s {_sProtocol = a})

-- | The subscription's owner.
sOwner :: Lens' Subscription (Maybe Text)
sOwner = lens _sOwner (\s a -> s {_sOwner = a})

-- | The ARN of the subscription's topic.
sTopicARN :: Lens' Subscription (Maybe Text)
sTopicARN = lens _sTopicARN (\s a -> s {_sTopicARN = a})

-- | The subscription's endpoint (format depends on the protocol).
sEndpoint :: Lens' Subscription (Maybe Text)
sEndpoint = lens _sEndpoint (\s a -> s {_sEndpoint = a})

-- | The subscription's ARN.
sSubscriptionARN :: Lens' Subscription (Maybe Text)
sSubscriptionARN = lens _sSubscriptionARN (\s a -> s {_sSubscriptionARN = a})

instance FromXML Subscription where
  parseXML x =
    Subscription'
      <$> (x .@? "Protocol")
      <*> (x .@? "Owner")
      <*> (x .@? "TopicArn")
      <*> (x .@? "Endpoint")
      <*> (x .@? "SubscriptionArn")

instance Hashable Subscription

instance NFData Subscription
