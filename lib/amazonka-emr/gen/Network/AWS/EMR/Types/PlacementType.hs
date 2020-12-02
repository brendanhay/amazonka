{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.PlacementType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.PlacementType where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The Amazon EC2 Availability Zone configuration of the cluster (job flow).
--
--
--
-- /See:/ 'placementType' smart constructor.
data PlacementType = PlacementType'
  { _ptAvailabilityZones ::
      !(Maybe [Text]),
    _ptAvailabilityZone :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PlacementType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ptAvailabilityZones' - When multiple Availability Zones are specified, Amazon EMR evaluates them and launches instances in the optimal Availability Zone. @AvailabilityZones@ is used for instance fleets, while @AvailabilityZone@ (singular) is used for uniform instance groups.
--
-- * 'ptAvailabilityZone' - The Amazon EC2 Availability Zone for the cluster. @AvailabilityZone@ is used for uniform instance groups, while @AvailabilityZones@ (plural) is used for instance fleets.
placementType ::
  PlacementType
placementType =
  PlacementType'
    { _ptAvailabilityZones = Nothing,
      _ptAvailabilityZone = Nothing
    }

-- | When multiple Availability Zones are specified, Amazon EMR evaluates them and launches instances in the optimal Availability Zone. @AvailabilityZones@ is used for instance fleets, while @AvailabilityZone@ (singular) is used for uniform instance groups.
ptAvailabilityZones :: Lens' PlacementType [Text]
ptAvailabilityZones = lens _ptAvailabilityZones (\s a -> s {_ptAvailabilityZones = a}) . _Default . _Coerce

-- | The Amazon EC2 Availability Zone for the cluster. @AvailabilityZone@ is used for uniform instance groups, while @AvailabilityZones@ (plural) is used for instance fleets.
ptAvailabilityZone :: Lens' PlacementType (Maybe Text)
ptAvailabilityZone = lens _ptAvailabilityZone (\s a -> s {_ptAvailabilityZone = a})

instance Hashable PlacementType

instance NFData PlacementType

instance ToJSON PlacementType where
  toJSON PlacementType' {..} =
    object
      ( catMaybes
          [ ("AvailabilityZones" .=) <$> _ptAvailabilityZones,
            ("AvailabilityZone" .=) <$> _ptAvailabilityZone
          ]
      )
