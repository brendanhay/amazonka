{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.ChannelActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.ChannelActivity where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The activity that determines the source of the messages to be processed.
--
--
--
-- /See:/ 'channelActivity' smart constructor.
data ChannelActivity = ChannelActivity'
  { _caNext :: !(Maybe Text),
    _caName :: !Text,
    _caChannelName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ChannelActivity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'caNext' - The next activity in the pipeline.
--
-- * 'caName' - The name of the channel activity.
--
-- * 'caChannelName' - The name of the channel from which the messages are processed.
channelActivity ::
  -- | 'caName'
  Text ->
  -- | 'caChannelName'
  Text ->
  ChannelActivity
channelActivity pName_ pChannelName_ =
  ChannelActivity'
    { _caNext = Nothing,
      _caName = pName_,
      _caChannelName = pChannelName_
    }

-- | The next activity in the pipeline.
caNext :: Lens' ChannelActivity (Maybe Text)
caNext = lens _caNext (\s a -> s {_caNext = a})

-- | The name of the channel activity.
caName :: Lens' ChannelActivity Text
caName = lens _caName (\s a -> s {_caName = a})

-- | The name of the channel from which the messages are processed.
caChannelName :: Lens' ChannelActivity Text
caChannelName = lens _caChannelName (\s a -> s {_caChannelName = a})

instance FromJSON ChannelActivity where
  parseJSON =
    withObject
      "ChannelActivity"
      ( \x ->
          ChannelActivity'
            <$> (x .:? "next") <*> (x .: "name") <*> (x .: "channelName")
      )

instance Hashable ChannelActivity

instance NFData ChannelActivity

instance ToJSON ChannelActivity where
  toJSON ChannelActivity' {..} =
    object
      ( catMaybes
          [ ("next" .=) <$> _caNext,
            Just ("name" .= _caName),
            Just ("channelName" .= _caChannelName)
          ]
      )
