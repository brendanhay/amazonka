{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.DataSourceConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.DataSourceConfigurations where

import Network.AWS.GuardDuty.Types.S3LogsConfiguration
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about which data sources are enabled.
--
--
--
-- /See:/ 'dataSourceConfigurations' smart constructor.
newtype DataSourceConfigurations = DataSourceConfigurations'
  { _dscS3Logs ::
      Maybe S3LogsConfiguration
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DataSourceConfigurations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dscS3Logs' - Describes whether S3 data event logs are enabled as a data source.
dataSourceConfigurations ::
  DataSourceConfigurations
dataSourceConfigurations =
  DataSourceConfigurations' {_dscS3Logs = Nothing}

-- | Describes whether S3 data event logs are enabled as a data source.
dscS3Logs :: Lens' DataSourceConfigurations (Maybe S3LogsConfiguration)
dscS3Logs = lens _dscS3Logs (\s a -> s {_dscS3Logs = a})

instance Hashable DataSourceConfigurations

instance NFData DataSourceConfigurations

instance ToJSON DataSourceConfigurations where
  toJSON DataSourceConfigurations' {..} =
    object (catMaybes [("s3Logs" .=) <$> _dscS3Logs])
