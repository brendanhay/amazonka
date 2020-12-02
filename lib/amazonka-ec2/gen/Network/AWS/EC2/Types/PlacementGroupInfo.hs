{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PlacementGroupInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PlacementGroupInfo where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.PlacementGroupStrategy
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the placement group support of the instance type.
--
--
--
-- /See:/ 'placementGroupInfo' smart constructor.
newtype PlacementGroupInfo = PlacementGroupInfo'
  { _pgiSupportedStrategies ::
      Maybe [PlacementGroupStrategy]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PlacementGroupInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pgiSupportedStrategies' - The supported placement group types.
placementGroupInfo ::
  PlacementGroupInfo
placementGroupInfo =
  PlacementGroupInfo' {_pgiSupportedStrategies = Nothing}

-- | The supported placement group types.
pgiSupportedStrategies :: Lens' PlacementGroupInfo [PlacementGroupStrategy]
pgiSupportedStrategies = lens _pgiSupportedStrategies (\s a -> s {_pgiSupportedStrategies = a}) . _Default . _Coerce

instance FromXML PlacementGroupInfo where
  parseXML x =
    PlacementGroupInfo'
      <$> ( x .@? "supportedStrategies" .!@ mempty
              >>= may (parseXMLList "item")
          )

instance Hashable PlacementGroupInfo

instance NFData PlacementGroupInfo
