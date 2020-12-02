{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotFleetTagSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SpotFleetTagSpecification where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ResourceType
import Network.AWS.EC2.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The tags for a Spot Fleet resource.
--
--
--
-- /See:/ 'spotFleetTagSpecification' smart constructor.
data SpotFleetTagSpecification = SpotFleetTagSpecification'
  { _sftsResourceType ::
      !(Maybe ResourceType),
    _sftsTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SpotFleetTagSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sftsResourceType' - The type of resource. Currently, the only resource type that is supported is @instance@ . To tag the Spot Fleet request on creation, use the @TagSpecifications@ parameter in <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_SpotFleetRequestConfigData.html @SpotFleetRequestConfigData@ > .
--
-- * 'sftsTags' - The tags.
spotFleetTagSpecification ::
  SpotFleetTagSpecification
spotFleetTagSpecification =
  SpotFleetTagSpecification'
    { _sftsResourceType = Nothing,
      _sftsTags = Nothing
    }

-- | The type of resource. Currently, the only resource type that is supported is @instance@ . To tag the Spot Fleet request on creation, use the @TagSpecifications@ parameter in <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_SpotFleetRequestConfigData.html @SpotFleetRequestConfigData@ > .
sftsResourceType :: Lens' SpotFleetTagSpecification (Maybe ResourceType)
sftsResourceType = lens _sftsResourceType (\s a -> s {_sftsResourceType = a})

-- | The tags.
sftsTags :: Lens' SpotFleetTagSpecification [Tag]
sftsTags = lens _sftsTags (\s a -> s {_sftsTags = a}) . _Default . _Coerce

instance FromXML SpotFleetTagSpecification where
  parseXML x =
    SpotFleetTagSpecification'
      <$> (x .@? "resourceType")
      <*> (x .@? "tag" .!@ mempty >>= may (parseXMLList "item"))

instance Hashable SpotFleetTagSpecification

instance NFData SpotFleetTagSpecification

instance ToQuery SpotFleetTagSpecification where
  toQuery SpotFleetTagSpecification' {..} =
    mconcat
      [ "ResourceType" =: _sftsResourceType,
        toQuery (toQueryList "Tag" <$> _sftsTags)
      ]
