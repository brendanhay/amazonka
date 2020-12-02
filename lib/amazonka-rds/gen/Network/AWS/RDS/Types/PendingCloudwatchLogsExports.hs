{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.PendingCloudwatchLogsExports
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.PendingCloudwatchLogsExports where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A list of the log types whose configuration is still pending. In other words, these log types are in the process of being activated or deactivated.
--
--
--
-- /See:/ 'pendingCloudwatchLogsExports' smart constructor.
data PendingCloudwatchLogsExports = PendingCloudwatchLogsExports'
  { _pcleLogTypesToEnable ::
      !(Maybe [Text]),
    _pcleLogTypesToDisable ::
      !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PendingCloudwatchLogsExports' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcleLogTypesToEnable' - Log types that are in the process of being deactivated. After they are deactivated, these log types aren't exported to CloudWatch Logs.
--
-- * 'pcleLogTypesToDisable' - Log types that are in the process of being enabled. After they are enabled, these log types are exported to CloudWatch Logs.
pendingCloudwatchLogsExports ::
  PendingCloudwatchLogsExports
pendingCloudwatchLogsExports =
  PendingCloudwatchLogsExports'
    { _pcleLogTypesToEnable = Nothing,
      _pcleLogTypesToDisable = Nothing
    }

-- | Log types that are in the process of being deactivated. After they are deactivated, these log types aren't exported to CloudWatch Logs.
pcleLogTypesToEnable :: Lens' PendingCloudwatchLogsExports [Text]
pcleLogTypesToEnable = lens _pcleLogTypesToEnable (\s a -> s {_pcleLogTypesToEnable = a}) . _Default . _Coerce

-- | Log types that are in the process of being enabled. After they are enabled, these log types are exported to CloudWatch Logs.
pcleLogTypesToDisable :: Lens' PendingCloudwatchLogsExports [Text]
pcleLogTypesToDisable = lens _pcleLogTypesToDisable (\s a -> s {_pcleLogTypesToDisable = a}) . _Default . _Coerce

instance FromXML PendingCloudwatchLogsExports where
  parseXML x =
    PendingCloudwatchLogsExports'
      <$> ( x .@? "LogTypesToEnable" .!@ mempty
              >>= may (parseXMLList "member")
          )
      <*> ( x .@? "LogTypesToDisable" .!@ mempty
              >>= may (parseXMLList "member")
          )

instance Hashable PendingCloudwatchLogsExports

instance NFData PendingCloudwatchLogsExports
