{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.ScalingParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.ScalingParameters where

import Network.AWS.CloudSearch.Types.PartitionInstanceType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The desired instance type and desired number of replicas of each index partition.
--
--
--
-- /See:/ 'scalingParameters' smart constructor.
data ScalingParameters = ScalingParameters'
  { _spDesiredInstanceType ::
      !(Maybe PartitionInstanceType),
    _spDesiredReplicationCount :: !(Maybe Nat),
    _spDesiredPartitionCount :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ScalingParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spDesiredInstanceType' - The instance type that you want to preconfigure for your domain. For example, @search.m1.small@ .
--
-- * 'spDesiredReplicationCount' - The number of replicas you want to preconfigure for each index partition.
--
-- * 'spDesiredPartitionCount' - The number of partitions you want to preconfigure for your domain. Only valid when you select @m2.2xlarge@ as the desired instance type.
scalingParameters ::
  ScalingParameters
scalingParameters =
  ScalingParameters'
    { _spDesiredInstanceType = Nothing,
      _spDesiredReplicationCount = Nothing,
      _spDesiredPartitionCount = Nothing
    }

-- | The instance type that you want to preconfigure for your domain. For example, @search.m1.small@ .
spDesiredInstanceType :: Lens' ScalingParameters (Maybe PartitionInstanceType)
spDesiredInstanceType = lens _spDesiredInstanceType (\s a -> s {_spDesiredInstanceType = a})

-- | The number of replicas you want to preconfigure for each index partition.
spDesiredReplicationCount :: Lens' ScalingParameters (Maybe Natural)
spDesiredReplicationCount = lens _spDesiredReplicationCount (\s a -> s {_spDesiredReplicationCount = a}) . mapping _Nat

-- | The number of partitions you want to preconfigure for your domain. Only valid when you select @m2.2xlarge@ as the desired instance type.
spDesiredPartitionCount :: Lens' ScalingParameters (Maybe Natural)
spDesiredPartitionCount = lens _spDesiredPartitionCount (\s a -> s {_spDesiredPartitionCount = a}) . mapping _Nat

instance FromXML ScalingParameters where
  parseXML x =
    ScalingParameters'
      <$> (x .@? "DesiredInstanceType")
      <*> (x .@? "DesiredReplicationCount")
      <*> (x .@? "DesiredPartitionCount")

instance Hashable ScalingParameters

instance NFData ScalingParameters

instance ToQuery ScalingParameters where
  toQuery ScalingParameters' {..} =
    mconcat
      [ "DesiredInstanceType" =: _spDesiredInstanceType,
        "DesiredReplicationCount" =: _spDesiredReplicationCount,
        "DesiredPartitionCount" =: _spDesiredPartitionCount
      ]
