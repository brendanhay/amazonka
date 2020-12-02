{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.Subscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.Subscription where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WorkDocs.Types.SubscriptionProtocolType

-- | Describes a subscription.
--
--
--
-- /See:/ 'subscription' smart constructor.
data Subscription = Subscription'
  { _sProtocol ::
      !(Maybe SubscriptionProtocolType),
    _sEndPoint :: !(Maybe Text),
    _sSubscriptionId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Subscription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sProtocol' - The protocol of the subscription.
--
-- * 'sEndPoint' - The endpoint of the subscription.
--
-- * 'sSubscriptionId' - The ID of the subscription.
subscription ::
  Subscription
subscription =
  Subscription'
    { _sProtocol = Nothing,
      _sEndPoint = Nothing,
      _sSubscriptionId = Nothing
    }

-- | The protocol of the subscription.
sProtocol :: Lens' Subscription (Maybe SubscriptionProtocolType)
sProtocol = lens _sProtocol (\s a -> s {_sProtocol = a})

-- | The endpoint of the subscription.
sEndPoint :: Lens' Subscription (Maybe Text)
sEndPoint = lens _sEndPoint (\s a -> s {_sEndPoint = a})

-- | The ID of the subscription.
sSubscriptionId :: Lens' Subscription (Maybe Text)
sSubscriptionId = lens _sSubscriptionId (\s a -> s {_sSubscriptionId = a})

instance FromJSON Subscription where
  parseJSON =
    withObject
      "Subscription"
      ( \x ->
          Subscription'
            <$> (x .:? "Protocol")
            <*> (x .:? "EndPoint")
            <*> (x .:? "SubscriptionId")
      )

instance Hashable Subscription

instance NFData Subscription
