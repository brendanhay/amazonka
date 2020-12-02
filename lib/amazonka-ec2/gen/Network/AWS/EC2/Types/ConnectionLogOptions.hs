{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ConnectionLogOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ConnectionLogOptions where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the client connection logging options for the Client VPN endpoint.
--
--
--
-- /See:/ 'connectionLogOptions' smart constructor.
data ConnectionLogOptions = ConnectionLogOptions'
  { _cloEnabled ::
      !(Maybe Bool),
    _cloCloudwatchLogStream :: !(Maybe Text),
    _cloCloudwatchLogGroup :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ConnectionLogOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cloEnabled' - Indicates whether connection logging is enabled.
--
-- * 'cloCloudwatchLogStream' - The name of the CloudWatch Logs log stream to which the connection data is published.
--
-- * 'cloCloudwatchLogGroup' - The name of the CloudWatch Logs log group. Required if connection logging is enabled.
connectionLogOptions ::
  ConnectionLogOptions
connectionLogOptions =
  ConnectionLogOptions'
    { _cloEnabled = Nothing,
      _cloCloudwatchLogStream = Nothing,
      _cloCloudwatchLogGroup = Nothing
    }

-- | Indicates whether connection logging is enabled.
cloEnabled :: Lens' ConnectionLogOptions (Maybe Bool)
cloEnabled = lens _cloEnabled (\s a -> s {_cloEnabled = a})

-- | The name of the CloudWatch Logs log stream to which the connection data is published.
cloCloudwatchLogStream :: Lens' ConnectionLogOptions (Maybe Text)
cloCloudwatchLogStream = lens _cloCloudwatchLogStream (\s a -> s {_cloCloudwatchLogStream = a})

-- | The name of the CloudWatch Logs log group. Required if connection logging is enabled.
cloCloudwatchLogGroup :: Lens' ConnectionLogOptions (Maybe Text)
cloCloudwatchLogGroup = lens _cloCloudwatchLogGroup (\s a -> s {_cloCloudwatchLogGroup = a})

instance Hashable ConnectionLogOptions

instance NFData ConnectionLogOptions

instance ToQuery ConnectionLogOptions where
  toQuery ConnectionLogOptions' {..} =
    mconcat
      [ "Enabled" =: _cloEnabled,
        "CloudwatchLogStream" =: _cloCloudwatchLogStream,
        "CloudwatchLogGroup" =: _cloCloudwatchLogGroup
      ]
