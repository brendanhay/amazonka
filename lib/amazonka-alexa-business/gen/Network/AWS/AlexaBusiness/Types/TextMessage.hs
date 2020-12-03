{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.TextMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.TextMessage where

import Network.AWS.AlexaBusiness.Types.Locale
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The text message.
--
--
--
-- /See:/ 'textMessage' smart constructor.
data TextMessage = TextMessage'
  { _tmLocale :: !Locale,
    _tmValue :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TextMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tmLocale' - The locale of the text message. Currently, en-US is supported.
--
-- * 'tmValue' - The value of the text message.
textMessage ::
  -- | 'tmLocale'
  Locale ->
  -- | 'tmValue'
  Text ->
  TextMessage
textMessage pLocale_ pValue_ =
  TextMessage' {_tmLocale = pLocale_, _tmValue = pValue_}

-- | The locale of the text message. Currently, en-US is supported.
tmLocale :: Lens' TextMessage Locale
tmLocale = lens _tmLocale (\s a -> s {_tmLocale = a})

-- | The value of the text message.
tmValue :: Lens' TextMessage Text
tmValue = lens _tmValue (\s a -> s {_tmValue = a})

instance Hashable TextMessage

instance NFData TextMessage

instance ToJSON TextMessage where
  toJSON TextMessage' {..} =
    object
      ( catMaybes
          [Just ("Locale" .= _tmLocale), Just ("Value" .= _tmValue)]
      )
