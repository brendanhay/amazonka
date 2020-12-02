{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.OrganizationS3LogsConfigurationResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.OrganizationS3LogsConfigurationResult where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The current configuration of S3 data event logs as a data source for the organization.
--
--
--
-- /See:/ 'organizationS3LogsConfigurationResult' smart constructor.
newtype OrganizationS3LogsConfigurationResult = OrganizationS3LogsConfigurationResult'
  { _oslcrAutoEnable ::
      Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OrganizationS3LogsConfigurationResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oslcrAutoEnable' - A value that describes whether S3 data event logs are automatically enabled for new members of the organization.
organizationS3LogsConfigurationResult ::
  -- | 'oslcrAutoEnable'
  Bool ->
  OrganizationS3LogsConfigurationResult
organizationS3LogsConfigurationResult pAutoEnable_ =
  OrganizationS3LogsConfigurationResult'
    { _oslcrAutoEnable =
        pAutoEnable_
    }

-- | A value that describes whether S3 data event logs are automatically enabled for new members of the organization.
oslcrAutoEnable :: Lens' OrganizationS3LogsConfigurationResult Bool
oslcrAutoEnable = lens _oslcrAutoEnable (\s a -> s {_oslcrAutoEnable = a})

instance FromJSON OrganizationS3LogsConfigurationResult where
  parseJSON =
    withObject
      "OrganizationS3LogsConfigurationResult"
      ( \x ->
          OrganizationS3LogsConfigurationResult' <$> (x .: "autoEnable")
      )

instance Hashable OrganizationS3LogsConfigurationResult

instance NFData OrganizationS3LogsConfigurationResult
