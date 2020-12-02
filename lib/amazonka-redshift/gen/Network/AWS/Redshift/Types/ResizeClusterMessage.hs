{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ResizeClusterMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ResizeClusterMessage where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal

-- | Describes a resize cluster operation. For example, a scheduled action to run the @ResizeCluster@ API operation.
--
--
--
-- /See:/ 'resizeClusterMessage' smart constructor.
data ResizeClusterMessage = ResizeClusterMessage'
  { _rcmNumberOfNodes ::
      !(Maybe Int),
    _rcmClassic :: !(Maybe Bool),
    _rcmClusterType :: !(Maybe Text),
    _rcmNodeType :: !(Maybe Text),
    _rcmClusterIdentifier :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResizeClusterMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcmNumberOfNodes' - The new number of nodes for the cluster. If not specified, the cluster's current number of nodes is used.
--
-- * 'rcmClassic' - A boolean value indicating whether the resize operation is using the classic resize process. If you don't provide this parameter or set the value to @false@ , the resize type is elastic.
--
-- * 'rcmClusterType' - The new cluster type for the specified cluster.
--
-- * 'rcmNodeType' - The new node type for the nodes you are adding. If not specified, the cluster's current node type is used.
--
-- * 'rcmClusterIdentifier' - The unique identifier for the cluster to resize.
resizeClusterMessage ::
  -- | 'rcmClusterIdentifier'
  Text ->
  ResizeClusterMessage
resizeClusterMessage pClusterIdentifier_ =
  ResizeClusterMessage'
    { _rcmNumberOfNodes = Nothing,
      _rcmClassic = Nothing,
      _rcmClusterType = Nothing,
      _rcmNodeType = Nothing,
      _rcmClusterIdentifier = pClusterIdentifier_
    }

-- | The new number of nodes for the cluster. If not specified, the cluster's current number of nodes is used.
rcmNumberOfNodes :: Lens' ResizeClusterMessage (Maybe Int)
rcmNumberOfNodes = lens _rcmNumberOfNodes (\s a -> s {_rcmNumberOfNodes = a})

-- | A boolean value indicating whether the resize operation is using the classic resize process. If you don't provide this parameter or set the value to @false@ , the resize type is elastic.
rcmClassic :: Lens' ResizeClusterMessage (Maybe Bool)
rcmClassic = lens _rcmClassic (\s a -> s {_rcmClassic = a})

-- | The new cluster type for the specified cluster.
rcmClusterType :: Lens' ResizeClusterMessage (Maybe Text)
rcmClusterType = lens _rcmClusterType (\s a -> s {_rcmClusterType = a})

-- | The new node type for the nodes you are adding. If not specified, the cluster's current node type is used.
rcmNodeType :: Lens' ResizeClusterMessage (Maybe Text)
rcmNodeType = lens _rcmNodeType (\s a -> s {_rcmNodeType = a})

-- | The unique identifier for the cluster to resize.
rcmClusterIdentifier :: Lens' ResizeClusterMessage Text
rcmClusterIdentifier = lens _rcmClusterIdentifier (\s a -> s {_rcmClusterIdentifier = a})

instance FromXML ResizeClusterMessage where
  parseXML x =
    ResizeClusterMessage'
      <$> (x .@? "NumberOfNodes")
      <*> (x .@? "Classic")
      <*> (x .@? "ClusterType")
      <*> (x .@? "NodeType")
      <*> (x .@ "ClusterIdentifier")

instance Hashable ResizeClusterMessage

instance NFData ResizeClusterMessage

instance ToQuery ResizeClusterMessage where
  toQuery ResizeClusterMessage' {..} =
    mconcat
      [ "NumberOfNodes" =: _rcmNumberOfNodes,
        "Classic" =: _rcmClassic,
        "ClusterType" =: _rcmClusterType,
        "NodeType" =: _rcmNodeType,
        "ClusterIdentifier" =: _rcmClusterIdentifier
      ]
