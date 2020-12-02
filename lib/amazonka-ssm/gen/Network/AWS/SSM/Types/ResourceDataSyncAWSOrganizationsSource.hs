{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ResourceDataSyncAWSOrganizationsSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ResourceDataSyncAWSOrganizationsSource where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.ResourceDataSyncOrganizationalUnit

-- | Information about the AwsOrganizationsSource resource data sync source. A sync source of this type can synchronize data from AWS Organizations or, if an AWS Organization is not present, from multiple AWS Regions.
--
--
--
-- /See:/ 'resourceDataSyncAWSOrganizationsSource' smart constructor.
data ResourceDataSyncAWSOrganizationsSource = ResourceDataSyncAWSOrganizationsSource'
  { _rdsaosOrganizationalUnits ::
      !( Maybe
           ( List1
               ResourceDataSyncOrganizationalUnit
           )
       ),
    _rdsaosOrganizationSourceType ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceDataSyncAWSOrganizationsSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdsaosOrganizationalUnits' - The AWS Organizations organization units included in the sync.
--
-- * 'rdsaosOrganizationSourceType' - If an AWS Organization is present, this is either @OrganizationalUnits@ or @EntireOrganization@ . For @OrganizationalUnits@ , the data is aggregated from a set of organization units. For @EntireOrganization@ , the data is aggregated from the entire AWS Organization.
resourceDataSyncAWSOrganizationsSource ::
  -- | 'rdsaosOrganizationSourceType'
  Text ->
  ResourceDataSyncAWSOrganizationsSource
resourceDataSyncAWSOrganizationsSource pOrganizationSourceType_ =
  ResourceDataSyncAWSOrganizationsSource'
    { _rdsaosOrganizationalUnits =
        Nothing,
      _rdsaosOrganizationSourceType =
        pOrganizationSourceType_
    }

-- | The AWS Organizations organization units included in the sync.
rdsaosOrganizationalUnits :: Lens' ResourceDataSyncAWSOrganizationsSource (Maybe (NonEmpty ResourceDataSyncOrganizationalUnit))
rdsaosOrganizationalUnits = lens _rdsaosOrganizationalUnits (\s a -> s {_rdsaosOrganizationalUnits = a}) . mapping _List1

-- | If an AWS Organization is present, this is either @OrganizationalUnits@ or @EntireOrganization@ . For @OrganizationalUnits@ , the data is aggregated from a set of organization units. For @EntireOrganization@ , the data is aggregated from the entire AWS Organization.
rdsaosOrganizationSourceType :: Lens' ResourceDataSyncAWSOrganizationsSource Text
rdsaosOrganizationSourceType = lens _rdsaosOrganizationSourceType (\s a -> s {_rdsaosOrganizationSourceType = a})

instance FromJSON ResourceDataSyncAWSOrganizationsSource where
  parseJSON =
    withObject
      "ResourceDataSyncAWSOrganizationsSource"
      ( \x ->
          ResourceDataSyncAWSOrganizationsSource'
            <$> (x .:? "OrganizationalUnits") <*> (x .: "OrganizationSourceType")
      )

instance Hashable ResourceDataSyncAWSOrganizationsSource

instance NFData ResourceDataSyncAWSOrganizationsSource

instance ToJSON ResourceDataSyncAWSOrganizationsSource where
  toJSON ResourceDataSyncAWSOrganizationsSource' {..} =
    object
      ( catMaybes
          [ ("OrganizationalUnits" .=) <$> _rdsaosOrganizationalUnits,
            Just ("OrganizationSourceType" .= _rdsaosOrganizationSourceType)
          ]
      )
