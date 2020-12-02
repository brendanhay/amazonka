{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ActiveInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ActiveInstance where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.InstanceHealthStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a running instance in a Spot Fleet.
--
--
--
-- /See:/ 'activeInstance' smart constructor.
data ActiveInstance = ActiveInstance'
  { _aiInstanceId ::
      !(Maybe Text),
    _aiInstanceHealth :: !(Maybe InstanceHealthStatus),
    _aiInstanceType :: !(Maybe Text),
    _aiSpotInstanceRequestId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ActiveInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aiInstanceId' - The ID of the instance.
--
-- * 'aiInstanceHealth' - The health status of the instance. If the status of either the instance status check or the system status check is @impaired@ , the health status of the instance is @unhealthy@ . Otherwise, the health status is @healthy@ .
--
-- * 'aiInstanceType' - The instance type.
--
-- * 'aiSpotInstanceRequestId' - The ID of the Spot Instance request.
activeInstance ::
  ActiveInstance
activeInstance =
  ActiveInstance'
    { _aiInstanceId = Nothing,
      _aiInstanceHealth = Nothing,
      _aiInstanceType = Nothing,
      _aiSpotInstanceRequestId = Nothing
    }

-- | The ID of the instance.
aiInstanceId :: Lens' ActiveInstance (Maybe Text)
aiInstanceId = lens _aiInstanceId (\s a -> s {_aiInstanceId = a})

-- | The health status of the instance. If the status of either the instance status check or the system status check is @impaired@ , the health status of the instance is @unhealthy@ . Otherwise, the health status is @healthy@ .
aiInstanceHealth :: Lens' ActiveInstance (Maybe InstanceHealthStatus)
aiInstanceHealth = lens _aiInstanceHealth (\s a -> s {_aiInstanceHealth = a})

-- | The instance type.
aiInstanceType :: Lens' ActiveInstance (Maybe Text)
aiInstanceType = lens _aiInstanceType (\s a -> s {_aiInstanceType = a})

-- | The ID of the Spot Instance request.
aiSpotInstanceRequestId :: Lens' ActiveInstance (Maybe Text)
aiSpotInstanceRequestId = lens _aiSpotInstanceRequestId (\s a -> s {_aiSpotInstanceRequestId = a})

instance FromXML ActiveInstance where
  parseXML x =
    ActiveInstance'
      <$> (x .@? "instanceId")
      <*> (x .@? "instanceHealth")
      <*> (x .@? "instanceType")
      <*> (x .@? "spotInstanceRequestId")

instance Hashable ActiveInstance

instance NFData ActiveInstance
