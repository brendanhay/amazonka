{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.NotifyEmailType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.NotifyEmailType where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The notify email type.
--
--
--
-- /See:/ 'notifyEmailType' smart constructor.
data NotifyEmailType = NotifyEmailType'
  { _netTextBody ::
      !(Maybe Text),
    _netHTMLBody :: !(Maybe Text),
    _netSubject :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NotifyEmailType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'netTextBody' - The text body.
--
-- * 'netHTMLBody' - The HTML body.
--
-- * 'netSubject' - The subject.
notifyEmailType ::
  -- | 'netSubject'
  Text ->
  NotifyEmailType
notifyEmailType pSubject_ =
  NotifyEmailType'
    { _netTextBody = Nothing,
      _netHTMLBody = Nothing,
      _netSubject = pSubject_
    }

-- | The text body.
netTextBody :: Lens' NotifyEmailType (Maybe Text)
netTextBody = lens _netTextBody (\s a -> s {_netTextBody = a})

-- | The HTML body.
netHTMLBody :: Lens' NotifyEmailType (Maybe Text)
netHTMLBody = lens _netHTMLBody (\s a -> s {_netHTMLBody = a})

-- | The subject.
netSubject :: Lens' NotifyEmailType Text
netSubject = lens _netSubject (\s a -> s {_netSubject = a})

instance FromJSON NotifyEmailType where
  parseJSON =
    withObject
      "NotifyEmailType"
      ( \x ->
          NotifyEmailType'
            <$> (x .:? "TextBody") <*> (x .:? "HtmlBody") <*> (x .: "Subject")
      )

instance Hashable NotifyEmailType

instance NFData NotifyEmailType

instance ToJSON NotifyEmailType where
  toJSON NotifyEmailType' {..} =
    object
      ( catMaybes
          [ ("TextBody" .=) <$> _netTextBody,
            ("HtmlBody" .=) <$> _netHTMLBody,
            Just ("Subject" .= _netSubject)
          ]
      )
