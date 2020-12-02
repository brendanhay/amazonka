{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.OrganizationDataSourceConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.OrganizationDataSourceConfigurations where

import Network.AWS.GuardDuty.Types.OrganizationS3LogsConfiguration
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object that contains information on which data sources will be configured to be automatically enabled for new members within the organization.
--
--
--
-- /See:/ 'organizationDataSourceConfigurations' smart constructor.
newtype OrganizationDataSourceConfigurations = OrganizationDataSourceConfigurations'
  { _odscS3Logs ::
      Maybe
        OrganizationS3LogsConfiguration
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OrganizationDataSourceConfigurations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'odscS3Logs' - Describes whether S3 data event logs are enabled for new members of the organization.
organizationDataSourceConfigurations ::
  OrganizationDataSourceConfigurations
organizationDataSourceConfigurations =
  OrganizationDataSourceConfigurations' {_odscS3Logs = Nothing}

-- | Describes whether S3 data event logs are enabled for new members of the organization.
odscS3Logs :: Lens' OrganizationDataSourceConfigurations (Maybe OrganizationS3LogsConfiguration)
odscS3Logs = lens _odscS3Logs (\s a -> s {_odscS3Logs = a})

instance Hashable OrganizationDataSourceConfigurations

instance NFData OrganizationDataSourceConfigurations

instance ToJSON OrganizationDataSourceConfigurations where
  toJSON OrganizationDataSourceConfigurations' {..} =
    object (catMaybes [("s3Logs" .=) <$> _odscS3Logs])
