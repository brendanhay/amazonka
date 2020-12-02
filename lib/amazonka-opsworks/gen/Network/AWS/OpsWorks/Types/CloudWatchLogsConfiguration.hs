{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.CloudWatchLogsConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.CloudWatchLogsConfiguration where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types.CloudWatchLogsLogStream
import Network.AWS.Prelude

-- | Describes the Amazon CloudWatch logs configuration for a layer.
--
--
--
-- /See:/ 'cloudWatchLogsConfiguration' smart constructor.
data CloudWatchLogsConfiguration = CloudWatchLogsConfiguration'
  { _cwlcEnabled ::
      !(Maybe Bool),
    _cwlcLogStreams ::
      !(Maybe [CloudWatchLogsLogStream])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CloudWatchLogsConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cwlcEnabled' - Whether CloudWatch Logs is enabled for a layer.
--
-- * 'cwlcLogStreams' - A list of configuration options for CloudWatch Logs.
cloudWatchLogsConfiguration ::
  CloudWatchLogsConfiguration
cloudWatchLogsConfiguration =
  CloudWatchLogsConfiguration'
    { _cwlcEnabled = Nothing,
      _cwlcLogStreams = Nothing
    }

-- | Whether CloudWatch Logs is enabled for a layer.
cwlcEnabled :: Lens' CloudWatchLogsConfiguration (Maybe Bool)
cwlcEnabled = lens _cwlcEnabled (\s a -> s {_cwlcEnabled = a})

-- | A list of configuration options for CloudWatch Logs.
cwlcLogStreams :: Lens' CloudWatchLogsConfiguration [CloudWatchLogsLogStream]
cwlcLogStreams = lens _cwlcLogStreams (\s a -> s {_cwlcLogStreams = a}) . _Default . _Coerce

instance FromJSON CloudWatchLogsConfiguration where
  parseJSON =
    withObject
      "CloudWatchLogsConfiguration"
      ( \x ->
          CloudWatchLogsConfiguration'
            <$> (x .:? "Enabled") <*> (x .:? "LogStreams" .!= mempty)
      )

instance Hashable CloudWatchLogsConfiguration

instance NFData CloudWatchLogsConfiguration

instance ToJSON CloudWatchLogsConfiguration where
  toJSON CloudWatchLogsConfiguration' {..} =
    object
      ( catMaybes
          [ ("Enabled" .=) <$> _cwlcEnabled,
            ("LogStreams" .=) <$> _cwlcLogStreams
          ]
      )
