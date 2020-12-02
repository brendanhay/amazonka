{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceFleetModifyConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceFleetModifyConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Configuration parameters for an instance fleet modification request.
--
--
--
-- /See:/ 'instanceFleetModifyConfig' smart constructor.
data InstanceFleetModifyConfig = InstanceFleetModifyConfig'
  { _ifmcTargetOnDemandCapacity ::
      !(Maybe Nat),
    _ifmcTargetSpotCapacity :: !(Maybe Nat),
    _ifmcInstanceFleetId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceFleetModifyConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ifmcTargetOnDemandCapacity' - The target capacity of On-Demand units for the instance fleet. For more information see 'InstanceFleetConfig$TargetOnDemandCapacity' .
--
-- * 'ifmcTargetSpotCapacity' - The target capacity of Spot units for the instance fleet. For more information, see 'InstanceFleetConfig$TargetSpotCapacity' .
--
-- * 'ifmcInstanceFleetId' - A unique identifier for the instance fleet.
instanceFleetModifyConfig ::
  -- | 'ifmcInstanceFleetId'
  Text ->
  InstanceFleetModifyConfig
instanceFleetModifyConfig pInstanceFleetId_ =
  InstanceFleetModifyConfig'
    { _ifmcTargetOnDemandCapacity = Nothing,
      _ifmcTargetSpotCapacity = Nothing,
      _ifmcInstanceFleetId = pInstanceFleetId_
    }

-- | The target capacity of On-Demand units for the instance fleet. For more information see 'InstanceFleetConfig$TargetOnDemandCapacity' .
ifmcTargetOnDemandCapacity :: Lens' InstanceFleetModifyConfig (Maybe Natural)
ifmcTargetOnDemandCapacity = lens _ifmcTargetOnDemandCapacity (\s a -> s {_ifmcTargetOnDemandCapacity = a}) . mapping _Nat

-- | The target capacity of Spot units for the instance fleet. For more information, see 'InstanceFleetConfig$TargetSpotCapacity' .
ifmcTargetSpotCapacity :: Lens' InstanceFleetModifyConfig (Maybe Natural)
ifmcTargetSpotCapacity = lens _ifmcTargetSpotCapacity (\s a -> s {_ifmcTargetSpotCapacity = a}) . mapping _Nat

-- | A unique identifier for the instance fleet.
ifmcInstanceFleetId :: Lens' InstanceFleetModifyConfig Text
ifmcInstanceFleetId = lens _ifmcInstanceFleetId (\s a -> s {_ifmcInstanceFleetId = a})

instance Hashable InstanceFleetModifyConfig

instance NFData InstanceFleetModifyConfig

instance ToJSON InstanceFleetModifyConfig where
  toJSON InstanceFleetModifyConfig' {..} =
    object
      ( catMaybes
          [ ("TargetOnDemandCapacity" .=) <$> _ifmcTargetOnDemandCapacity,
            ("TargetSpotCapacity" .=) <$> _ifmcTargetSpotCapacity,
            Just ("InstanceFleetId" .= _ifmcInstanceFleetId)
          ]
      )
