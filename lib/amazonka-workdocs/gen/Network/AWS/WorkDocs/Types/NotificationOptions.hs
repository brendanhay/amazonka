{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.NotificationOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.NotificationOptions where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Set of options which defines notification preferences of given action.
--
--
--
-- /See:/ 'notificationOptions' smart constructor.
data NotificationOptions = NotificationOptions'
  { _noEmailMessage ::
      !(Maybe (Sensitive Text)),
    _noSendEmail :: !(Maybe Bool)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'NotificationOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'noEmailMessage' - Text value to be included in the email body.
--
-- * 'noSendEmail' - Boolean value to indicate an email notification should be sent to the receipients.
notificationOptions ::
  NotificationOptions
notificationOptions =
  NotificationOptions'
    { _noEmailMessage = Nothing,
      _noSendEmail = Nothing
    }

-- | Text value to be included in the email body.
noEmailMessage :: Lens' NotificationOptions (Maybe Text)
noEmailMessage = lens _noEmailMessage (\s a -> s {_noEmailMessage = a}) . mapping _Sensitive

-- | Boolean value to indicate an email notification should be sent to the receipients.
noSendEmail :: Lens' NotificationOptions (Maybe Bool)
noSendEmail = lens _noSendEmail (\s a -> s {_noSendEmail = a})

instance Hashable NotificationOptions

instance NFData NotificationOptions

instance ToJSON NotificationOptions where
  toJSON NotificationOptions' {..} =
    object
      ( catMaybes
          [ ("EmailMessage" .=) <$> _noEmailMessage,
            ("SendEmail" .=) <$> _noSendEmail
          ]
      )
