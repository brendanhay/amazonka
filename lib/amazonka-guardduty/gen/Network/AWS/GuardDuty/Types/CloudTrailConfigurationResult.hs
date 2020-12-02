{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.CloudTrailConfigurationResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.CloudTrailConfigurationResult where

import Network.AWS.GuardDuty.Types.DataSourceStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information on the status of CloudTrail as a data source for the detector.
--
--
--
-- /See:/ 'cloudTrailConfigurationResult' smart constructor.
newtype CloudTrailConfigurationResult = CloudTrailConfigurationResult'
  { _ctcrStatus ::
      DataSourceStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CloudTrailConfigurationResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctcrStatus' - Describes whether CloudTrail is enabled as a data source for the detector.
cloudTrailConfigurationResult ::
  -- | 'ctcrStatus'
  DataSourceStatus ->
  CloudTrailConfigurationResult
cloudTrailConfigurationResult pStatus_ =
  CloudTrailConfigurationResult' {_ctcrStatus = pStatus_}

-- | Describes whether CloudTrail is enabled as a data source for the detector.
ctcrStatus :: Lens' CloudTrailConfigurationResult DataSourceStatus
ctcrStatus = lens _ctcrStatus (\s a -> s {_ctcrStatus = a})

instance FromJSON CloudTrailConfigurationResult where
  parseJSON =
    withObject
      "CloudTrailConfigurationResult"
      (\x -> CloudTrailConfigurationResult' <$> (x .: "status"))

instance Hashable CloudTrailConfigurationResult

instance NFData CloudTrailConfigurationResult
