{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.PlacementGroupConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.PlacementGroupConfig where

import Network.AWS.EMR.Types.InstanceRoleType
import Network.AWS.EMR.Types.PlacementGroupStrategy
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Placement group configuration for an Amazon EMR cluster. The configuration specifies the placement strategy that can be applied to instance roles during cluster creation.
--
--
-- To use this configuration, consider attaching managed policy AmazonElasticMapReducePlacementGroupPolicy to the EMR role.
--
--
-- /See:/ 'placementGroupConfig' smart constructor.
data PlacementGroupConfig = PlacementGroupConfig'
  { _pgcPlacementStrategy ::
      !(Maybe PlacementGroupStrategy),
    _pgcInstanceRole :: !InstanceRoleType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PlacementGroupConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pgcPlacementStrategy' - EC2 Placement Group strategy associated with instance role. Starting with Amazon EMR version 5.23.0, the only supported placement strategy is @SPREAD@ for the @MASTER@ instance role.
--
-- * 'pgcInstanceRole' - Role of the instance in the cluster. Starting with Amazon EMR version 5.23.0, the only supported instance role is @MASTER@ .
placementGroupConfig ::
  -- | 'pgcInstanceRole'
  InstanceRoleType ->
  PlacementGroupConfig
placementGroupConfig pInstanceRole_ =
  PlacementGroupConfig'
    { _pgcPlacementStrategy = Nothing,
      _pgcInstanceRole = pInstanceRole_
    }

-- | EC2 Placement Group strategy associated with instance role. Starting with Amazon EMR version 5.23.0, the only supported placement strategy is @SPREAD@ for the @MASTER@ instance role.
pgcPlacementStrategy :: Lens' PlacementGroupConfig (Maybe PlacementGroupStrategy)
pgcPlacementStrategy = lens _pgcPlacementStrategy (\s a -> s {_pgcPlacementStrategy = a})

-- | Role of the instance in the cluster. Starting with Amazon EMR version 5.23.0, the only supported instance role is @MASTER@ .
pgcInstanceRole :: Lens' PlacementGroupConfig InstanceRoleType
pgcInstanceRole = lens _pgcInstanceRole (\s a -> s {_pgcInstanceRole = a})

instance FromJSON PlacementGroupConfig where
  parseJSON =
    withObject
      "PlacementGroupConfig"
      ( \x ->
          PlacementGroupConfig'
            <$> (x .:? "PlacementStrategy") <*> (x .: "InstanceRole")
      )

instance Hashable PlacementGroupConfig

instance NFData PlacementGroupConfig

instance ToJSON PlacementGroupConfig where
  toJSON PlacementGroupConfig' {..} =
    object
      ( catMaybes
          [ ("PlacementStrategy" .=) <$> _pgcPlacementStrategy,
            Just ("InstanceRole" .= _pgcInstanceRole)
          ]
      )
