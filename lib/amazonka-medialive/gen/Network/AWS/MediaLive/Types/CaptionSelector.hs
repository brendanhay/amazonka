{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.CaptionSelector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.CaptionSelector where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.CaptionSelectorSettings
import Network.AWS.Prelude

-- | Output groups for this Live Event. Output groups contain information about where streams should be distributed.
--
-- /See:/ 'captionSelector' smart constructor.
data CaptionSelector = CaptionSelector'
  { _cLanguageCode ::
      !(Maybe Text),
    _cSelectorSettings :: !(Maybe CaptionSelectorSettings),
    _cName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CaptionSelector' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cLanguageCode' - When specified this field indicates the three letter language code of the caption track to extract from the source.
--
-- * 'cSelectorSettings' - Caption selector settings.
--
-- * 'cName' - Name identifier for a caption selector.  This name is used to associate this caption selector with one or more caption descriptions.  Names must be unique within an event.
captionSelector ::
  -- | 'cName'
  Text ->
  CaptionSelector
captionSelector pName_ =
  CaptionSelector'
    { _cLanguageCode = Nothing,
      _cSelectorSettings = Nothing,
      _cName = pName_
    }

-- | When specified this field indicates the three letter language code of the caption track to extract from the source.
cLanguageCode :: Lens' CaptionSelector (Maybe Text)
cLanguageCode = lens _cLanguageCode (\s a -> s {_cLanguageCode = a})

-- | Caption selector settings.
cSelectorSettings :: Lens' CaptionSelector (Maybe CaptionSelectorSettings)
cSelectorSettings = lens _cSelectorSettings (\s a -> s {_cSelectorSettings = a})

-- | Name identifier for a caption selector.  This name is used to associate this caption selector with one or more caption descriptions.  Names must be unique within an event.
cName :: Lens' CaptionSelector Text
cName = lens _cName (\s a -> s {_cName = a})

instance FromJSON CaptionSelector where
  parseJSON =
    withObject
      "CaptionSelector"
      ( \x ->
          CaptionSelector'
            <$> (x .:? "languageCode")
            <*> (x .:? "selectorSettings")
            <*> (x .: "name")
      )

instance Hashable CaptionSelector

instance NFData CaptionSelector

instance ToJSON CaptionSelector where
  toJSON CaptionSelector' {..} =
    object
      ( catMaybes
          [ ("languageCode" .=) <$> _cLanguageCode,
            ("selectorSettings" .=) <$> _cSelectorSettings,
            Just ("name" .= _cName)
          ]
      )
