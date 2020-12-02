{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.Subscriber
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.Subscriber where

import Network.AWS.CostExplorer.Types.SubscriberStatus
import Network.AWS.CostExplorer.Types.SubscriberType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The recipient of @AnomalySubscription@ notifications.
--
--
--
-- /See:/ 'subscriber' smart constructor.
data Subscriber = Subscriber'
  { _sStatus ::
      !(Maybe SubscriberStatus),
    _sAddress :: !(Maybe Text),
    _sType :: !(Maybe SubscriberType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Subscriber' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sStatus' - Indicates if the subscriber accepts the notifications.
--
-- * 'sAddress' - The email address or SNS Amazon Resource Name (ARN), depending on the @Type@ .
--
-- * 'sType' - The notification delivery channel.
subscriber ::
  Subscriber
subscriber =
  Subscriber'
    { _sStatus = Nothing,
      _sAddress = Nothing,
      _sType = Nothing
    }

-- | Indicates if the subscriber accepts the notifications.
sStatus :: Lens' Subscriber (Maybe SubscriberStatus)
sStatus = lens _sStatus (\s a -> s {_sStatus = a})

-- | The email address or SNS Amazon Resource Name (ARN), depending on the @Type@ .
sAddress :: Lens' Subscriber (Maybe Text)
sAddress = lens _sAddress (\s a -> s {_sAddress = a})

-- | The notification delivery channel.
sType :: Lens' Subscriber (Maybe SubscriberType)
sType = lens _sType (\s a -> s {_sType = a})

instance FromJSON Subscriber where
  parseJSON =
    withObject
      "Subscriber"
      ( \x ->
          Subscriber'
            <$> (x .:? "Status") <*> (x .:? "Address") <*> (x .:? "Type")
      )

instance Hashable Subscriber

instance NFData Subscriber

instance ToJSON Subscriber where
  toJSON Subscriber' {..} =
    object
      ( catMaybes
          [ ("Status" .=) <$> _sStatus,
            ("Address" .=) <$> _sAddress,
            ("Type" .=) <$> _sType
          ]
      )
