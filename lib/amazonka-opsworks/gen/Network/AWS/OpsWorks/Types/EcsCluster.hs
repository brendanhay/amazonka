{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.EcsCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.EcsCluster where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a registered Amazon ECS cluster.
--
--
--
-- /See:/ 'ecsCluster' smart constructor.
data EcsCluster = EcsCluster'
  { _ecEcsClusterARN :: !(Maybe Text),
    _ecEcsClusterName :: !(Maybe Text),
    _ecRegisteredAt :: !(Maybe Text),
    _ecStackId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EcsCluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ecEcsClusterARN' - The cluster's ARN.
--
-- * 'ecEcsClusterName' - The cluster name.
--
-- * 'ecRegisteredAt' - The time and date that the cluster was registered with the stack.
--
-- * 'ecStackId' - The stack ID.
ecsCluster ::
  EcsCluster
ecsCluster =
  EcsCluster'
    { _ecEcsClusterARN = Nothing,
      _ecEcsClusterName = Nothing,
      _ecRegisteredAt = Nothing,
      _ecStackId = Nothing
    }

-- | The cluster's ARN.
ecEcsClusterARN :: Lens' EcsCluster (Maybe Text)
ecEcsClusterARN = lens _ecEcsClusterARN (\s a -> s {_ecEcsClusterARN = a})

-- | The cluster name.
ecEcsClusterName :: Lens' EcsCluster (Maybe Text)
ecEcsClusterName = lens _ecEcsClusterName (\s a -> s {_ecEcsClusterName = a})

-- | The time and date that the cluster was registered with the stack.
ecRegisteredAt :: Lens' EcsCluster (Maybe Text)
ecRegisteredAt = lens _ecRegisteredAt (\s a -> s {_ecRegisteredAt = a})

-- | The stack ID.
ecStackId :: Lens' EcsCluster (Maybe Text)
ecStackId = lens _ecStackId (\s a -> s {_ecStackId = a})

instance FromJSON EcsCluster where
  parseJSON =
    withObject
      "EcsCluster"
      ( \x ->
          EcsCluster'
            <$> (x .:? "EcsClusterArn")
            <*> (x .:? "EcsClusterName")
            <*> (x .:? "RegisteredAt")
            <*> (x .:? "StackId")
      )

instance Hashable EcsCluster

instance NFData EcsCluster
