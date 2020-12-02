{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ClusterSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.ClusterSummary where

import Network.AWS.EMR.Types.ClusterStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The summary description of the cluster.
--
--
--
-- /See:/ 'clusterSummary' smart constructor.
data ClusterSummary = ClusterSummary'
  { _cStatus ::
      !(Maybe ClusterStatus),
    _cClusterARN :: !(Maybe Text),
    _cOutpostARN :: !(Maybe Text),
    _cNormalizedInstanceHours :: !(Maybe Int),
    _cName :: !(Maybe Text),
    _cId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ClusterSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cStatus' - The details about the current status of the cluster.
--
-- * 'cClusterARN' - The Amazon Resource Name of the cluster.
--
-- * 'cOutpostARN' - The Amazon Resource Name (ARN) of the Outpost where the cluster is launched.
--
-- * 'cNormalizedInstanceHours' - An approximation of the cost of the cluster, represented in m1.small/hours. This value is incremented one time for every hour an m1.small instance runs. Larger instances are weighted more, so an EC2 instance that is roughly four times more expensive would result in the normalized instance hours being incremented by four. This result is only an approximation and does not reflect the actual billing rate.
--
-- * 'cName' - The name of the cluster.
--
-- * 'cId' - The unique identifier for the cluster.
clusterSummary ::
  ClusterSummary
clusterSummary =
  ClusterSummary'
    { _cStatus = Nothing,
      _cClusterARN = Nothing,
      _cOutpostARN = Nothing,
      _cNormalizedInstanceHours = Nothing,
      _cName = Nothing,
      _cId = Nothing
    }

-- | The details about the current status of the cluster.
cStatus :: Lens' ClusterSummary (Maybe ClusterStatus)
cStatus = lens _cStatus (\s a -> s {_cStatus = a})

-- | The Amazon Resource Name of the cluster.
cClusterARN :: Lens' ClusterSummary (Maybe Text)
cClusterARN = lens _cClusterARN (\s a -> s {_cClusterARN = a})

-- | The Amazon Resource Name (ARN) of the Outpost where the cluster is launched.
cOutpostARN :: Lens' ClusterSummary (Maybe Text)
cOutpostARN = lens _cOutpostARN (\s a -> s {_cOutpostARN = a})

-- | An approximation of the cost of the cluster, represented in m1.small/hours. This value is incremented one time for every hour an m1.small instance runs. Larger instances are weighted more, so an EC2 instance that is roughly four times more expensive would result in the normalized instance hours being incremented by four. This result is only an approximation and does not reflect the actual billing rate.
cNormalizedInstanceHours :: Lens' ClusterSummary (Maybe Int)
cNormalizedInstanceHours = lens _cNormalizedInstanceHours (\s a -> s {_cNormalizedInstanceHours = a})

-- | The name of the cluster.
cName :: Lens' ClusterSummary (Maybe Text)
cName = lens _cName (\s a -> s {_cName = a})

-- | The unique identifier for the cluster.
cId :: Lens' ClusterSummary (Maybe Text)
cId = lens _cId (\s a -> s {_cId = a})

instance FromJSON ClusterSummary where
  parseJSON =
    withObject
      "ClusterSummary"
      ( \x ->
          ClusterSummary'
            <$> (x .:? "Status")
            <*> (x .:? "ClusterArn")
            <*> (x .:? "OutpostArn")
            <*> (x .:? "NormalizedInstanceHours")
            <*> (x .:? "Name")
            <*> (x .:? "Id")
      )

instance Hashable ClusterSummary

instance NFData ClusterSummary
