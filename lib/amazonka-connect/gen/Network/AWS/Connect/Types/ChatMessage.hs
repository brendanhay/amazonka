{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.ChatMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.ChatMessage where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A chat message.
--
--
--
-- /See:/ 'chatMessage' smart constructor.
data ChatMessage = ChatMessage'
  { _cmContentType :: !Text,
    _cmContent :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ChatMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmContentType' - The type of the content. Supported types are text/plain.
--
-- * 'cmContent' - The content of the chat message.
chatMessage ::
  -- | 'cmContentType'
  Text ->
  -- | 'cmContent'
  Text ->
  ChatMessage
chatMessage pContentType_ pContent_ =
  ChatMessage'
    { _cmContentType = pContentType_,
      _cmContent = pContent_
    }

-- | The type of the content. Supported types are text/plain.
cmContentType :: Lens' ChatMessage Text
cmContentType = lens _cmContentType (\s a -> s {_cmContentType = a})

-- | The content of the chat message.
cmContent :: Lens' ChatMessage Text
cmContent = lens _cmContent (\s a -> s {_cmContent = a})

instance Hashable ChatMessage

instance NFData ChatMessage

instance ToJSON ChatMessage where
  toJSON ChatMessage' {..} =
    object
      ( catMaybes
          [ Just ("ContentType" .= _cmContentType),
            Just ("Content" .= _cmContent)
          ]
      )
