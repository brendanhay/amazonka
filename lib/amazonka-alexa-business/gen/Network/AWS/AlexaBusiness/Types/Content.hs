{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.Content
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.Content where

import Network.AWS.AlexaBusiness.Types.Audio
import Network.AWS.AlexaBusiness.Types.Ssml
import Network.AWS.AlexaBusiness.Types.TextMessage
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The content definition. This can contain only one text, SSML, or audio list object.
--
--
--
-- /See:/ 'content' smart constructor.
data Content = Content'
  { _cAudioList :: !(Maybe [Audio]),
    _cTextList :: !(Maybe [TextMessage]),
    _cSsmlList :: !(Maybe [Ssml])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Content' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cAudioList' - The list of audio messages.
--
-- * 'cTextList' - The list of text messages.
--
-- * 'cSsmlList' - The list of SSML messages.
content ::
  Content
content =
  Content'
    { _cAudioList = Nothing,
      _cTextList = Nothing,
      _cSsmlList = Nothing
    }

-- | The list of audio messages.
cAudioList :: Lens' Content [Audio]
cAudioList = lens _cAudioList (\s a -> s {_cAudioList = a}) . _Default . _Coerce

-- | The list of text messages.
cTextList :: Lens' Content [TextMessage]
cTextList = lens _cTextList (\s a -> s {_cTextList = a}) . _Default . _Coerce

-- | The list of SSML messages.
cSsmlList :: Lens' Content [Ssml]
cSsmlList = lens _cSsmlList (\s a -> s {_cSsmlList = a}) . _Default . _Coerce

instance Hashable Content

instance NFData Content

instance ToJSON Content where
  toJSON Content' {..} =
    object
      ( catMaybes
          [ ("AudioList" .=) <$> _cAudioList,
            ("TextList" .=) <$> _cTextList,
            ("SsmlList" .=) <$> _cSsmlList
          ]
      )
