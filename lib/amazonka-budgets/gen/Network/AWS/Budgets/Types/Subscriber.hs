{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.Subscriber
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.Subscriber where

import Network.AWS.Budgets.Types.SubscriptionType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The subscriber to a budget notification. The subscriber consists of a subscription type and either an Amazon SNS topic or an email address.
--
--
-- For example, an email subscriber would have the following parameters:
--
--     * A @subscriptionType@ of @EMAIL@
--
--     * An @address@ of @example@example.com@
--
--
--
--
-- /See:/ 'subscriber' smart constructor.
data Subscriber = Subscriber'
  { _sSubscriptionType ::
      !SubscriptionType,
    _sAddress :: !(Sensitive Text)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'Subscriber' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sSubscriptionType' - The type of notification that AWS sends to a subscriber.
--
-- * 'sAddress' - The address that AWS sends budget notifications to, either an SNS topic or an email. When you create a subscriber, the value of @Address@ can't contain line breaks.
subscriber ::
  -- | 'sSubscriptionType'
  SubscriptionType ->
  -- | 'sAddress'
  Text ->
  Subscriber
subscriber pSubscriptionType_ pAddress_ =
  Subscriber'
    { _sSubscriptionType = pSubscriptionType_,
      _sAddress = _Sensitive # pAddress_
    }

-- | The type of notification that AWS sends to a subscriber.
sSubscriptionType :: Lens' Subscriber SubscriptionType
sSubscriptionType = lens _sSubscriptionType (\s a -> s {_sSubscriptionType = a})

-- | The address that AWS sends budget notifications to, either an SNS topic or an email. When you create a subscriber, the value of @Address@ can't contain line breaks.
sAddress :: Lens' Subscriber Text
sAddress = lens _sAddress (\s a -> s {_sAddress = a}) . _Sensitive

instance FromJSON Subscriber where
  parseJSON =
    withObject
      "Subscriber"
      ( \x ->
          Subscriber' <$> (x .: "SubscriptionType") <*> (x .: "Address")
      )

instance Hashable Subscriber

instance NFData Subscriber

instance ToJSON Subscriber where
  toJSON Subscriber' {..} =
    object
      ( catMaybes
          [ Just ("SubscriptionType" .= _sSubscriptionType),
            Just ("Address" .= _sAddress)
          ]
      )
