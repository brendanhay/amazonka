{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.Text
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.Text where

import Network.AWS.AlexaBusiness.Types.Locale
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The text message.
--
--
--
-- /See:/ 'text' smart constructor.
data Text = Text' {_tLocale :: !Locale, _tValue :: !Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Text' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tLocale' - The locale of the text message. Currently, en-US is supported.
--
-- * 'tValue' - The value of the text message.
text ::
  -- | 'tLocale'
  Locale ->
  -- | 'tValue'
  Text ->
  Text
text pLocale_ pValue_ =
  Text' {_tLocale = pLocale_, _tValue = pValue_}

-- | The locale of the text message. Currently, en-US is supported.
tLocale :: Lens' Text Locale
tLocale = lens _tLocale (\s a -> s {_tLocale = a})

-- | The value of the text message.
tValue :: Lens' Text Text
tValue = lens _tValue (\s a -> s {_tValue = a})

instance Hashable Text

instance NFData Text

instance ToJSON Text where
  toJSON Text' {..} =
    object
      ( catMaybes
          [Just ("Locale" .= _tLocale), Just ("Value" .= _tValue)]
      )
