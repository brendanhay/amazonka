{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.S3LogsConfigurationResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.S3LogsConfigurationResult where

import Network.AWS.GuardDuty.Types.DataSourceStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes whether S3 data event logs will be enabled as a data source.
--
--
--
-- /See:/ 's3LogsConfigurationResult' smart constructor.
newtype S3LogsConfigurationResult = S3LogsConfigurationResult'
  { _slcrStatus ::
      DataSourceStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'S3LogsConfigurationResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slcrStatus' - A value that describes whether S3 data event logs are automatically enabled for new members of the organization.
s3LogsConfigurationResult ::
  -- | 'slcrStatus'
  DataSourceStatus ->
  S3LogsConfigurationResult
s3LogsConfigurationResult pStatus_ =
  S3LogsConfigurationResult' {_slcrStatus = pStatus_}

-- | A value that describes whether S3 data event logs are automatically enabled for new members of the organization.
slcrStatus :: Lens' S3LogsConfigurationResult DataSourceStatus
slcrStatus = lens _slcrStatus (\s a -> s {_slcrStatus = a})

instance FromJSON S3LogsConfigurationResult where
  parseJSON =
    withObject
      "S3LogsConfigurationResult"
      (\x -> S3LogsConfigurationResult' <$> (x .: "status"))

instance Hashable S3LogsConfigurationResult

instance NFData S3LogsConfigurationResult
