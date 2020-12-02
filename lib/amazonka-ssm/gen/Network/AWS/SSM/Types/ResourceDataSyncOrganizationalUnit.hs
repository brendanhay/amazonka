{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ResourceDataSyncOrganizationalUnit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ResourceDataSyncOrganizationalUnit where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The AWS Organizations organizational unit data source for the sync.
--
--
--
-- /See:/ 'resourceDataSyncOrganizationalUnit' smart constructor.
newtype ResourceDataSyncOrganizationalUnit = ResourceDataSyncOrganizationalUnit'
  { _rdsouOrganizationalUnitId ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceDataSyncOrganizationalUnit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdsouOrganizationalUnitId' - The AWS Organization unit ID data source for the sync.
resourceDataSyncOrganizationalUnit ::
  ResourceDataSyncOrganizationalUnit
resourceDataSyncOrganizationalUnit =
  ResourceDataSyncOrganizationalUnit'
    { _rdsouOrganizationalUnitId =
        Nothing
    }

-- | The AWS Organization unit ID data source for the sync.
rdsouOrganizationalUnitId :: Lens' ResourceDataSyncOrganizationalUnit (Maybe Text)
rdsouOrganizationalUnitId = lens _rdsouOrganizationalUnitId (\s a -> s {_rdsouOrganizationalUnitId = a})

instance FromJSON ResourceDataSyncOrganizationalUnit where
  parseJSON =
    withObject
      "ResourceDataSyncOrganizationalUnit"
      ( \x ->
          ResourceDataSyncOrganizationalUnit'
            <$> (x .:? "OrganizationalUnitId")
      )

instance Hashable ResourceDataSyncOrganizationalUnit

instance NFData ResourceDataSyncOrganizationalUnit

instance ToJSON ResourceDataSyncOrganizationalUnit where
  toJSON ResourceDataSyncOrganizationalUnit' {..} =
    object
      ( catMaybes
          [("OrganizationalUnitId" .=) <$> _rdsouOrganizationalUnitId]
      )
