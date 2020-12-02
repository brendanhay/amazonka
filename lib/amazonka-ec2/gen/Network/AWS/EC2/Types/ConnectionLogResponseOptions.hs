{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ConnectionLogResponseOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ConnectionLogResponseOptions where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the client connection logging options for a Client VPN endpoint.
--
--
--
-- /See:/ 'connectionLogResponseOptions' smart constructor.
data ConnectionLogResponseOptions = ConnectionLogResponseOptions'
  { _clroEnabled ::
      !(Maybe Bool),
    _clroCloudwatchLogStream ::
      !(Maybe Text),
    _clroCloudwatchLogGroup ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ConnectionLogResponseOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clroEnabled' - Indicates whether client connection logging is enabled for the Client VPN endpoint.
--
-- * 'clroCloudwatchLogStream' - The name of the Amazon CloudWatch Logs log stream to which connection logging data is published.
--
-- * 'clroCloudwatchLogGroup' - The name of the Amazon CloudWatch Logs log group to which connection logging data is published.
connectionLogResponseOptions ::
  ConnectionLogResponseOptions
connectionLogResponseOptions =
  ConnectionLogResponseOptions'
    { _clroEnabled = Nothing,
      _clroCloudwatchLogStream = Nothing,
      _clroCloudwatchLogGroup = Nothing
    }

-- | Indicates whether client connection logging is enabled for the Client VPN endpoint.
clroEnabled :: Lens' ConnectionLogResponseOptions (Maybe Bool)
clroEnabled = lens _clroEnabled (\s a -> s {_clroEnabled = a})

-- | The name of the Amazon CloudWatch Logs log stream to which connection logging data is published.
clroCloudwatchLogStream :: Lens' ConnectionLogResponseOptions (Maybe Text)
clroCloudwatchLogStream = lens _clroCloudwatchLogStream (\s a -> s {_clroCloudwatchLogStream = a})

-- | The name of the Amazon CloudWatch Logs log group to which connection logging data is published.
clroCloudwatchLogGroup :: Lens' ConnectionLogResponseOptions (Maybe Text)
clroCloudwatchLogGroup = lens _clroCloudwatchLogGroup (\s a -> s {_clroCloudwatchLogGroup = a})

instance FromXML ConnectionLogResponseOptions where
  parseXML x =
    ConnectionLogResponseOptions'
      <$> (x .@? "Enabled")
      <*> (x .@? "CloudwatchLogStream")
      <*> (x .@? "CloudwatchLogGroup")

instance Hashable ConnectionLogResponseOptions

instance NFData ConnectionLogResponseOptions
