{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.ClusterListEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.ClusterListEntry where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Snowball.Types.ClusterState

-- | Contains a cluster's state, a cluster's ID, and other important information.
--
--
--
-- /See:/ 'clusterListEntry' smart constructor.
data ClusterListEntry = ClusterListEntry'
  { _cleClusterState ::
      !(Maybe ClusterState),
    _cleClusterId :: !(Maybe Text),
    _cleCreationDate :: !(Maybe POSIX),
    _cleDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ClusterListEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cleClusterState' - The current state of this cluster. For information about the state of a specific node, see 'JobListEntry$JobState' .
--
-- * 'cleClusterId' - The 39-character ID for the cluster that you want to list, for example @CID123e4567-e89b-12d3-a456-426655440000@ .
--
-- * 'cleCreationDate' - The creation date for this cluster.
--
-- * 'cleDescription' - Defines an optional description of the cluster, for example @Environmental Data Cluster-01@ .
clusterListEntry ::
  ClusterListEntry
clusterListEntry =
  ClusterListEntry'
    { _cleClusterState = Nothing,
      _cleClusterId = Nothing,
      _cleCreationDate = Nothing,
      _cleDescription = Nothing
    }

-- | The current state of this cluster. For information about the state of a specific node, see 'JobListEntry$JobState' .
cleClusterState :: Lens' ClusterListEntry (Maybe ClusterState)
cleClusterState = lens _cleClusterState (\s a -> s {_cleClusterState = a})

-- | The 39-character ID for the cluster that you want to list, for example @CID123e4567-e89b-12d3-a456-426655440000@ .
cleClusterId :: Lens' ClusterListEntry (Maybe Text)
cleClusterId = lens _cleClusterId (\s a -> s {_cleClusterId = a})

-- | The creation date for this cluster.
cleCreationDate :: Lens' ClusterListEntry (Maybe UTCTime)
cleCreationDate = lens _cleCreationDate (\s a -> s {_cleCreationDate = a}) . mapping _Time

-- | Defines an optional description of the cluster, for example @Environmental Data Cluster-01@ .
cleDescription :: Lens' ClusterListEntry (Maybe Text)
cleDescription = lens _cleDescription (\s a -> s {_cleDescription = a})

instance FromJSON ClusterListEntry where
  parseJSON =
    withObject
      "ClusterListEntry"
      ( \x ->
          ClusterListEntry'
            <$> (x .:? "ClusterState")
            <*> (x .:? "ClusterId")
            <*> (x .:? "CreationDate")
            <*> (x .:? "Description")
      )

instance Hashable ClusterListEntry

instance NFData ClusterListEntry
