{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.Types.Group
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroups.Types.Group where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A resource group that contains AWS resources. You can assign resources to the group by associating either of the following elements with the group:
--
--
--     * 'ResourceQuery' - Use a resource query to specify a set of tag keys and values. All resources in the same AWS Region and AWS account that have those keys with the same values are included in the group. You can add a resource query when you create the group.
--
--     * 'GroupConfiguration' - Use a service configuration to associate the group with an AWS service. The configuration specifies which resource types can be included in the group.
--
--
--
--
-- /See:/ 'group'' smart constructor.
data Group = Group'
  { _gDescription :: !(Maybe Text),
    _gGroupARN :: !Text,
    _gName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Group' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gDescription' - The description of the resource group.
--
-- * 'gGroupARN' - The ARN of the resource group.
--
-- * 'gName' - The name of the resource group.
group' ::
  -- | 'gGroupARN'
  Text ->
  -- | 'gName'
  Text ->
  Group
group' pGroupARN_ pName_ =
  Group'
    { _gDescription = Nothing,
      _gGroupARN = pGroupARN_,
      _gName = pName_
    }

-- | The description of the resource group.
gDescription :: Lens' Group (Maybe Text)
gDescription = lens _gDescription (\s a -> s {_gDescription = a})

-- | The ARN of the resource group.
gGroupARN :: Lens' Group Text
gGroupARN = lens _gGroupARN (\s a -> s {_gGroupARN = a})

-- | The name of the resource group.
gName :: Lens' Group Text
gName = lens _gName (\s a -> s {_gName = a})

instance FromJSON Group where
  parseJSON =
    withObject
      "Group"
      ( \x ->
          Group'
            <$> (x .:? "Description") <*> (x .: "GroupArn") <*> (x .: "Name")
      )

instance Hashable Group

instance NFData Group
