{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.MessageData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.MessageData where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A message returned by the @GetMetricData@ API, including a code and a description.
--
--
--
-- /See:/ 'messageData' smart constructor.
data MessageData = MessageData'
  { _mValue :: !(Maybe Text),
    _mCode :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MessageData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mValue' - The message text.
--
-- * 'mCode' - The error code or status code associated with the message.
messageData ::
  MessageData
messageData = MessageData' {_mValue = Nothing, _mCode = Nothing}

-- | The message text.
mValue :: Lens' MessageData (Maybe Text)
mValue = lens _mValue (\s a -> s {_mValue = a})

-- | The error code or status code associated with the message.
mCode :: Lens' MessageData (Maybe Text)
mCode = lens _mCode (\s a -> s {_mCode = a})

instance FromXML MessageData where
  parseXML x = MessageData' <$> (x .@? "Value") <*> (x .@? "Code")

instance Hashable MessageData

instance NFData MessageData
