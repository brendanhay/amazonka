{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.OrganizationDataSourceConfigurationsResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.OrganizationDataSourceConfigurationsResult where

import Network.AWS.GuardDuty.Types.OrganizationS3LogsConfigurationResult
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object that contains information on which data sources are automatically enabled for new members within the organization.
--
--
--
-- /See:/ 'organizationDataSourceConfigurationsResult' smart constructor.
newtype OrganizationDataSourceConfigurationsResult = OrganizationDataSourceConfigurationsResult'
  { _odscrS3Logs ::
      OrganizationS3LogsConfigurationResult
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'OrganizationDataSourceConfigurationsResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'odscrS3Logs' - Describes whether S3 data event logs are enabled as a data source.
organizationDataSourceConfigurationsResult ::
  -- | 'odscrS3Logs'
  OrganizationS3LogsConfigurationResult ->
  OrganizationDataSourceConfigurationsResult
organizationDataSourceConfigurationsResult pS3Logs_ =
  OrganizationDataSourceConfigurationsResult'
    { _odscrS3Logs =
        pS3Logs_
    }

-- | Describes whether S3 data event logs are enabled as a data source.
odscrS3Logs :: Lens' OrganizationDataSourceConfigurationsResult OrganizationS3LogsConfigurationResult
odscrS3Logs = lens _odscrS3Logs (\s a -> s {_odscrS3Logs = a})

instance FromJSON OrganizationDataSourceConfigurationsResult where
  parseJSON =
    withObject
      "OrganizationDataSourceConfigurationsResult"
      ( \x ->
          OrganizationDataSourceConfigurationsResult' <$> (x .: "s3Logs")
      )

instance Hashable OrganizationDataSourceConfigurationsResult

instance NFData OrganizationDataSourceConfigurationsResult
