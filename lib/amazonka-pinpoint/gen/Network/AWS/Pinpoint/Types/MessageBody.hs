{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.MessageBody
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.MessageBody where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information about an API request or response.
--
--
--
-- /See:/ 'messageBody' smart constructor.
data MessageBody = MessageBody'
  { _mbRequestId :: !(Maybe Text),
    _mbMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MessageBody' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mbRequestId' - The unique identifier for the request or response.
--
-- * 'mbMessage' - The message that's returned from the API.
messageBody ::
  MessageBody
messageBody =
  MessageBody' {_mbRequestId = Nothing, _mbMessage = Nothing}

-- | The unique identifier for the request or response.
mbRequestId :: Lens' MessageBody (Maybe Text)
mbRequestId = lens _mbRequestId (\s a -> s {_mbRequestId = a})

-- | The message that's returned from the API.
mbMessage :: Lens' MessageBody (Maybe Text)
mbMessage = lens _mbMessage (\s a -> s {_mbMessage = a})

instance FromJSON MessageBody where
  parseJSON =
    withObject
      "MessageBody"
      (\x -> MessageBody' <$> (x .:? "RequestID") <*> (x .:? "Message"))

instance Hashable MessageBody

instance NFData MessageBody
