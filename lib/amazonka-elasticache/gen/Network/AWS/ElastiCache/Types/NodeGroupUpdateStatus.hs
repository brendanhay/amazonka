{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.NodeGroupUpdateStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.NodeGroupUpdateStatus where

import Network.AWS.ElastiCache.Types.NodeGroupMemberUpdateStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The status of the service update on the node group
--
--
--
-- /See:/ 'nodeGroupUpdateStatus' smart constructor.
data NodeGroupUpdateStatus = NodeGroupUpdateStatus'
  { _ngusNodeGroupMemberUpdateStatus ::
      !(Maybe [NodeGroupMemberUpdateStatus]),
    _ngusNodeGroupId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NodeGroupUpdateStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ngusNodeGroupMemberUpdateStatus' - The status of the service update on the node group member
--
-- * 'ngusNodeGroupId' - The ID of the node group
nodeGroupUpdateStatus ::
  NodeGroupUpdateStatus
nodeGroupUpdateStatus =
  NodeGroupUpdateStatus'
    { _ngusNodeGroupMemberUpdateStatus =
        Nothing,
      _ngusNodeGroupId = Nothing
    }

-- | The status of the service update on the node group member
ngusNodeGroupMemberUpdateStatus :: Lens' NodeGroupUpdateStatus [NodeGroupMemberUpdateStatus]
ngusNodeGroupMemberUpdateStatus = lens _ngusNodeGroupMemberUpdateStatus (\s a -> s {_ngusNodeGroupMemberUpdateStatus = a}) . _Default . _Coerce

-- | The ID of the node group
ngusNodeGroupId :: Lens' NodeGroupUpdateStatus (Maybe Text)
ngusNodeGroupId = lens _ngusNodeGroupId (\s a -> s {_ngusNodeGroupId = a})

instance FromXML NodeGroupUpdateStatus where
  parseXML x =
    NodeGroupUpdateStatus'
      <$> ( x .@? "NodeGroupMemberUpdateStatus" .!@ mempty
              >>= may (parseXMLList "NodeGroupMemberUpdateStatus")
          )
      <*> (x .@? "NodeGroupId")

instance Hashable NodeGroupUpdateStatus

instance NFData NodeGroupUpdateStatus
