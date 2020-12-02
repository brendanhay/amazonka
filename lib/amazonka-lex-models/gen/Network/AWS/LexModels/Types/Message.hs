{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.Message
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.Message where

import Network.AWS.Lens
import Network.AWS.LexModels.Types.ContentType
import Network.AWS.Prelude

-- | The message object that provides the message text and its type.
--
--
--
-- /See:/ 'message' smart constructor.
data Message = Message'
  { _mGroupNumber :: !(Maybe Nat),
    _mContentType :: !ContentType,
    _mContent :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Message' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mGroupNumber' - Identifies the message group that the message belongs to. When a group is assigned to a message, Amazon Lex returns one message from each group in the response.
--
-- * 'mContentType' - The content type of the message string.
--
-- * 'mContent' - The text of the message.
message ::
  -- | 'mContentType'
  ContentType ->
  -- | 'mContent'
  Text ->
  Message
message pContentType_ pContent_ =
  Message'
    { _mGroupNumber = Nothing,
      _mContentType = pContentType_,
      _mContent = pContent_
    }

-- | Identifies the message group that the message belongs to. When a group is assigned to a message, Amazon Lex returns one message from each group in the response.
mGroupNumber :: Lens' Message (Maybe Natural)
mGroupNumber = lens _mGroupNumber (\s a -> s {_mGroupNumber = a}) . mapping _Nat

-- | The content type of the message string.
mContentType :: Lens' Message ContentType
mContentType = lens _mContentType (\s a -> s {_mContentType = a})

-- | The text of the message.
mContent :: Lens' Message Text
mContent = lens _mContent (\s a -> s {_mContent = a})

instance FromJSON Message where
  parseJSON =
    withObject
      "Message"
      ( \x ->
          Message'
            <$> (x .:? "groupNumber")
            <*> (x .: "contentType")
            <*> (x .: "content")
      )

instance Hashable Message

instance NFData Message

instance ToJSON Message where
  toJSON Message' {..} =
    object
      ( catMaybes
          [ ("groupNumber" .=) <$> _mGroupNumber,
            Just ("contentType" .= _mContentType),
            Just ("content" .= _mContent)
          ]
      )
