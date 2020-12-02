{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ElasticGpus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ElasticGpus where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ElasticGpuHealth
import Network.AWS.EC2.Types.ElasticGpuState
import Network.AWS.EC2.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an Elastic Graphics accelerator.
--
--
--
-- /See:/ 'elasticGpus' smart constructor.
data ElasticGpus = ElasticGpus'
  { _egInstanceId :: !(Maybe Text),
    _egElasticGpuType :: !(Maybe Text),
    _egElasticGpuId :: !(Maybe Text),
    _egElasticGpuState :: !(Maybe ElasticGpuState),
    _egElasticGpuHealth :: !(Maybe ElasticGpuHealth),
    _egAvailabilityZone :: !(Maybe Text),
    _egTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ElasticGpus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'egInstanceId' - The ID of the instance to which the Elastic Graphics accelerator is attached.
--
-- * 'egElasticGpuType' - The type of Elastic Graphics accelerator.
--
-- * 'egElasticGpuId' - The ID of the Elastic Graphics accelerator.
--
-- * 'egElasticGpuState' - The state of the Elastic Graphics accelerator.
--
-- * 'egElasticGpuHealth' - The status of the Elastic Graphics accelerator.
--
-- * 'egAvailabilityZone' - The Availability Zone in the which the Elastic Graphics accelerator resides.
--
-- * 'egTags' - The tags assigned to the Elastic Graphics accelerator.
elasticGpus ::
  ElasticGpus
elasticGpus =
  ElasticGpus'
    { _egInstanceId = Nothing,
      _egElasticGpuType = Nothing,
      _egElasticGpuId = Nothing,
      _egElasticGpuState = Nothing,
      _egElasticGpuHealth = Nothing,
      _egAvailabilityZone = Nothing,
      _egTags = Nothing
    }

-- | The ID of the instance to which the Elastic Graphics accelerator is attached.
egInstanceId :: Lens' ElasticGpus (Maybe Text)
egInstanceId = lens _egInstanceId (\s a -> s {_egInstanceId = a})

-- | The type of Elastic Graphics accelerator.
egElasticGpuType :: Lens' ElasticGpus (Maybe Text)
egElasticGpuType = lens _egElasticGpuType (\s a -> s {_egElasticGpuType = a})

-- | The ID of the Elastic Graphics accelerator.
egElasticGpuId :: Lens' ElasticGpus (Maybe Text)
egElasticGpuId = lens _egElasticGpuId (\s a -> s {_egElasticGpuId = a})

-- | The state of the Elastic Graphics accelerator.
egElasticGpuState :: Lens' ElasticGpus (Maybe ElasticGpuState)
egElasticGpuState = lens _egElasticGpuState (\s a -> s {_egElasticGpuState = a})

-- | The status of the Elastic Graphics accelerator.
egElasticGpuHealth :: Lens' ElasticGpus (Maybe ElasticGpuHealth)
egElasticGpuHealth = lens _egElasticGpuHealth (\s a -> s {_egElasticGpuHealth = a})

-- | The Availability Zone in the which the Elastic Graphics accelerator resides.
egAvailabilityZone :: Lens' ElasticGpus (Maybe Text)
egAvailabilityZone = lens _egAvailabilityZone (\s a -> s {_egAvailabilityZone = a})

-- | The tags assigned to the Elastic Graphics accelerator.
egTags :: Lens' ElasticGpus [Tag]
egTags = lens _egTags (\s a -> s {_egTags = a}) . _Default . _Coerce

instance FromXML ElasticGpus where
  parseXML x =
    ElasticGpus'
      <$> (x .@? "instanceId")
      <*> (x .@? "elasticGpuType")
      <*> (x .@? "elasticGpuId")
      <*> (x .@? "elasticGpuState")
      <*> (x .@? "elasticGpuHealth")
      <*> (x .@? "availabilityZone")
      <*> (x .@? "tagSet" .!@ mempty >>= may (parseXMLList "item"))

instance Hashable ElasticGpus

instance NFData ElasticGpus
