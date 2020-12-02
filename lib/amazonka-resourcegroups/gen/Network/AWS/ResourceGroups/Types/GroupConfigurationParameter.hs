{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.Types.GroupConfigurationParameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroups.Types.GroupConfigurationParameter where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A parameter for a group configuration item.
--
--
--
-- /See:/ 'groupConfigurationParameter' smart constructor.
data GroupConfigurationParameter = GroupConfigurationParameter'
  { _gcpValues ::
      !(Maybe [Text]),
    _gcpName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GroupConfigurationParameter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcpValues' - The values of for this parameter. You can specify the following string value:     * For item type @allowed-resource-types@ : the only supported parameter value is @AWS::EC2::CapacityReservation@ .
--
-- * 'gcpName' - The name of the group configuration parameter. You can specify the following string values:     * For configuration item type @AWS::ResourceGroups::Generic@ :     * @allowed-resource-types@  Specifies the types of resources that you can add to this group by using the 'GroupResources' operation.     * For configuration item type @AWS::EC2::CapacityReservationPool@ :     * None - This configuration item type doesn't support any parameters. For more information about EC2 capacity reservation groups, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/capacity-reservations-using.html#create-cr-group Working with capacity reservation groups> in the /EC2 Users Guide/ .
groupConfigurationParameter ::
  -- | 'gcpName'
  Text ->
  GroupConfigurationParameter
groupConfigurationParameter pName_ =
  GroupConfigurationParameter'
    { _gcpValues = Nothing,
      _gcpName = pName_
    }

-- | The values of for this parameter. You can specify the following string value:     * For item type @allowed-resource-types@ : the only supported parameter value is @AWS::EC2::CapacityReservation@ .
gcpValues :: Lens' GroupConfigurationParameter [Text]
gcpValues = lens _gcpValues (\s a -> s {_gcpValues = a}) . _Default . _Coerce

-- | The name of the group configuration parameter. You can specify the following string values:     * For configuration item type @AWS::ResourceGroups::Generic@ :     * @allowed-resource-types@  Specifies the types of resources that you can add to this group by using the 'GroupResources' operation.     * For configuration item type @AWS::EC2::CapacityReservationPool@ :     * None - This configuration item type doesn't support any parameters. For more information about EC2 capacity reservation groups, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/capacity-reservations-using.html#create-cr-group Working with capacity reservation groups> in the /EC2 Users Guide/ .
gcpName :: Lens' GroupConfigurationParameter Text
gcpName = lens _gcpName (\s a -> s {_gcpName = a})

instance FromJSON GroupConfigurationParameter where
  parseJSON =
    withObject
      "GroupConfigurationParameter"
      ( \x ->
          GroupConfigurationParameter'
            <$> (x .:? "Values" .!= mempty) <*> (x .: "Name")
      )

instance Hashable GroupConfigurationParameter

instance NFData GroupConfigurationParameter

instance ToJSON GroupConfigurationParameter where
  toJSON GroupConfigurationParameter' {..} =
    object
      ( catMaybes
          [("Values" .=) <$> _gcpValues, Just ("Name" .= _gcpName)]
      )
