{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SMSChannelRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SMSChannelRequest where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the status and settings of the SMS channel for an application.
--
--
--
-- /See:/ 'sMSChannelRequest' smart constructor.
data SMSChannelRequest = SMSChannelRequest'
  { _smscrShortCode ::
      !(Maybe Text),
    _smscrEnabled :: !(Maybe Bool),
    _smscrSenderId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SMSChannelRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smscrShortCode' - The registered short code that you want to use when you send messages through the SMS channel.
--
-- * 'smscrEnabled' - Specifies whether to enable the SMS channel for the application.
--
-- * 'smscrSenderId' - The identity that you want to display on recipients' devices when they receive messages from the SMS channel.
sMSChannelRequest ::
  SMSChannelRequest
sMSChannelRequest =
  SMSChannelRequest'
    { _smscrShortCode = Nothing,
      _smscrEnabled = Nothing,
      _smscrSenderId = Nothing
    }

-- | The registered short code that you want to use when you send messages through the SMS channel.
smscrShortCode :: Lens' SMSChannelRequest (Maybe Text)
smscrShortCode = lens _smscrShortCode (\s a -> s {_smscrShortCode = a})

-- | Specifies whether to enable the SMS channel for the application.
smscrEnabled :: Lens' SMSChannelRequest (Maybe Bool)
smscrEnabled = lens _smscrEnabled (\s a -> s {_smscrEnabled = a})

-- | The identity that you want to display on recipients' devices when they receive messages from the SMS channel.
smscrSenderId :: Lens' SMSChannelRequest (Maybe Text)
smscrSenderId = lens _smscrSenderId (\s a -> s {_smscrSenderId = a})

instance Hashable SMSChannelRequest

instance NFData SMSChannelRequest

instance ToJSON SMSChannelRequest where
  toJSON SMSChannelRequest' {..} =
    object
      ( catMaybes
          [ ("ShortCode" .=) <$> _smscrShortCode,
            ("Enabled" .=) <$> _smscrEnabled,
            ("SenderId" .=) <$> _smscrSenderId
          ]
      )
