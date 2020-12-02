{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ClusterNode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ClusterNode where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal

-- | The identifier of a node in a cluster.
--
--
--
-- /See:/ 'clusterNode' smart constructor.
data ClusterNode = ClusterNode'
  { _cnNodeRole :: !(Maybe Text),
    _cnPrivateIPAddress :: !(Maybe Text),
    _cnPublicIPAddress :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ClusterNode' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cnNodeRole' - Whether the node is a leader node or a compute node.
--
-- * 'cnPrivateIPAddress' - The private IP address of a node within a cluster.
--
-- * 'cnPublicIPAddress' - The public IP address of a node within a cluster.
clusterNode ::
  ClusterNode
clusterNode =
  ClusterNode'
    { _cnNodeRole = Nothing,
      _cnPrivateIPAddress = Nothing,
      _cnPublicIPAddress = Nothing
    }

-- | Whether the node is a leader node or a compute node.
cnNodeRole :: Lens' ClusterNode (Maybe Text)
cnNodeRole = lens _cnNodeRole (\s a -> s {_cnNodeRole = a})

-- | The private IP address of a node within a cluster.
cnPrivateIPAddress :: Lens' ClusterNode (Maybe Text)
cnPrivateIPAddress = lens _cnPrivateIPAddress (\s a -> s {_cnPrivateIPAddress = a})

-- | The public IP address of a node within a cluster.
cnPublicIPAddress :: Lens' ClusterNode (Maybe Text)
cnPublicIPAddress = lens _cnPublicIPAddress (\s a -> s {_cnPublicIPAddress = a})

instance FromXML ClusterNode where
  parseXML x =
    ClusterNode'
      <$> (x .@? "NodeRole")
      <*> (x .@? "PrivateIPAddress")
      <*> (x .@? "PublicIPAddress")

instance Hashable ClusterNode

instance NFData ClusterNode
