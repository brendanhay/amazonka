{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.MemberDataSourceConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.MemberDataSourceConfiguration where

import Network.AWS.GuardDuty.Types.DataSourceConfigurationsResult
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information on which data sources are enabled for a member account.
--
--
--
-- /See:/ 'memberDataSourceConfiguration' smart constructor.
data MemberDataSourceConfiguration = MemberDataSourceConfiguration'
  { _mdscAccountId ::
      !Text,
    _mdscDataSources ::
      !DataSourceConfigurationsResult
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MemberDataSourceConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdscAccountId' - The account ID for the member account.
--
-- * 'mdscDataSources' - Contains information on the status of data sources for the account.
memberDataSourceConfiguration ::
  -- | 'mdscAccountId'
  Text ->
  -- | 'mdscDataSources'
  DataSourceConfigurationsResult ->
  MemberDataSourceConfiguration
memberDataSourceConfiguration pAccountId_ pDataSources_ =
  MemberDataSourceConfiguration'
    { _mdscAccountId = pAccountId_,
      _mdscDataSources = pDataSources_
    }

-- | The account ID for the member account.
mdscAccountId :: Lens' MemberDataSourceConfiguration Text
mdscAccountId = lens _mdscAccountId (\s a -> s {_mdscAccountId = a})

-- | Contains information on the status of data sources for the account.
mdscDataSources :: Lens' MemberDataSourceConfiguration DataSourceConfigurationsResult
mdscDataSources = lens _mdscDataSources (\s a -> s {_mdscDataSources = a})

instance FromJSON MemberDataSourceConfiguration where
  parseJSON =
    withObject
      "MemberDataSourceConfiguration"
      ( \x ->
          MemberDataSourceConfiguration'
            <$> (x .: "accountId") <*> (x .: "dataSources")
      )

instance Hashable MemberDataSourceConfiguration

instance NFData MemberDataSourceConfiguration
