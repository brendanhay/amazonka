{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.CloudwatchLogsExportConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.CloudwatchLogsExportConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The configuration setting for the log types to be enabled for export to CloudWatch Logs for a specific DB instance or DB cluster.
--
--
-- The @EnableLogTypes@ and @DisableLogTypes@ arrays determine which logs will be exported (or not exported) to CloudWatch Logs. The values within these arrays depend on the DB engine being used.
--
-- For more information about exporting CloudWatch Logs for Amazon RDS DB instances, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs > in the /Amazon RDS User Guide/ .
--
-- For more information about exporting CloudWatch Logs for Amazon Aurora DB clusters, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs> in the /Amazon Aurora User Guide/ .
--
--
-- /See:/ 'cloudwatchLogsExportConfiguration' smart constructor.
data CloudwatchLogsExportConfiguration = CloudwatchLogsExportConfiguration'
  { _clecDisableLogTypes ::
      !(Maybe [Text]),
    _clecEnableLogTypes ::
      !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CloudwatchLogsExportConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clecDisableLogTypes' - The list of log types to disable.
--
-- * 'clecEnableLogTypes' - The list of log types to enable.
cloudwatchLogsExportConfiguration ::
  CloudwatchLogsExportConfiguration
cloudwatchLogsExportConfiguration =
  CloudwatchLogsExportConfiguration'
    { _clecDisableLogTypes =
        Nothing,
      _clecEnableLogTypes = Nothing
    }

-- | The list of log types to disable.
clecDisableLogTypes :: Lens' CloudwatchLogsExportConfiguration [Text]
clecDisableLogTypes = lens _clecDisableLogTypes (\s a -> s {_clecDisableLogTypes = a}) . _Default . _Coerce

-- | The list of log types to enable.
clecEnableLogTypes :: Lens' CloudwatchLogsExportConfiguration [Text]
clecEnableLogTypes = lens _clecEnableLogTypes (\s a -> s {_clecEnableLogTypes = a}) . _Default . _Coerce

instance Hashable CloudwatchLogsExportConfiguration

instance NFData CloudwatchLogsExportConfiguration

instance ToQuery CloudwatchLogsExportConfiguration where
  toQuery CloudwatchLogsExportConfiguration' {..} =
    mconcat
      [ "DisableLogTypes"
          =: toQuery (toQueryList "member" <$> _clecDisableLogTypes),
        "EnableLogTypes"
          =: toQuery (toQueryList "member" <$> _clecEnableLogTypes)
      ]
