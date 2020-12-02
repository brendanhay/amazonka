{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.Ssml
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.Ssml where

import Network.AWS.AlexaBusiness.Types.Locale
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The SSML message. For more information, see <https://developer.amazon.com/docs/custom-skills/speech-synthesis-markup-language-ssml-reference.html SSML Reference> .
--
--
--
-- /See:/ 'ssml' smart constructor.
data Ssml = Ssml' {_ssmLocale :: !Locale, _ssmValue :: !Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Ssml' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssmLocale' - The locale of the SSML message. Currently, en-US is supported.
--
-- * 'ssmValue' - The value of the SSML message in the correct SSML format. The audio tag is not supported.
ssml ::
  -- | 'ssmLocale'
  Locale ->
  -- | 'ssmValue'
  Text ->
  Ssml
ssml pLocale_ pValue_ =
  Ssml' {_ssmLocale = pLocale_, _ssmValue = pValue_}

-- | The locale of the SSML message. Currently, en-US is supported.
ssmLocale :: Lens' Ssml Locale
ssmLocale = lens _ssmLocale (\s a -> s {_ssmLocale = a})

-- | The value of the SSML message in the correct SSML format. The audio tag is not supported.
ssmValue :: Lens' Ssml Text
ssmValue = lens _ssmValue (\s a -> s {_ssmValue = a})

instance Hashable Ssml

instance NFData Ssml

instance ToJSON Ssml where
  toJSON Ssml' {..} =
    object
      ( catMaybes
          [Just ("Locale" .= _ssmLocale), Just ("Value" .= _ssmValue)]
      )
