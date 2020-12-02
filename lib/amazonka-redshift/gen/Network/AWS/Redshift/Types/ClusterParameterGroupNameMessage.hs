{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ClusterParameterGroupNameMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ClusterParameterGroupNameMessage where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal

-- |
--
--
--
-- /See:/ 'clusterParameterGroupNameMessage' smart constructor.
data ClusterParameterGroupNameMessage = ClusterParameterGroupNameMessage'
  { _cpgnmParameterGroupStatus ::
      !(Maybe Text),
    _cpgnmParameterGroupName ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ClusterParameterGroupNameMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpgnmParameterGroupStatus' - The status of the parameter group. For example, if you made a change to a parameter group name-value pair, then the change could be pending a reboot of an associated cluster.
--
-- * 'cpgnmParameterGroupName' - The name of the cluster parameter group.
clusterParameterGroupNameMessage ::
  ClusterParameterGroupNameMessage
clusterParameterGroupNameMessage =
  ClusterParameterGroupNameMessage'
    { _cpgnmParameterGroupStatus =
        Nothing,
      _cpgnmParameterGroupName = Nothing
    }

-- | The status of the parameter group. For example, if you made a change to a parameter group name-value pair, then the change could be pending a reboot of an associated cluster.
cpgnmParameterGroupStatus :: Lens' ClusterParameterGroupNameMessage (Maybe Text)
cpgnmParameterGroupStatus = lens _cpgnmParameterGroupStatus (\s a -> s {_cpgnmParameterGroupStatus = a})

-- | The name of the cluster parameter group.
cpgnmParameterGroupName :: Lens' ClusterParameterGroupNameMessage (Maybe Text)
cpgnmParameterGroupName = lens _cpgnmParameterGroupName (\s a -> s {_cpgnmParameterGroupName = a})

instance FromXML ClusterParameterGroupNameMessage where
  parseXML x =
    ClusterParameterGroupNameMessage'
      <$> (x .@? "ParameterGroupStatus") <*> (x .@? "ParameterGroupName")

instance Hashable ClusterParameterGroupNameMessage

instance NFData ClusterParameterGroupNameMessage
