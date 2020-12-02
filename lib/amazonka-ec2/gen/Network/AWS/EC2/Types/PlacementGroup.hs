{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PlacementGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PlacementGroup where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.PlacementGroupState
import Network.AWS.EC2.Types.PlacementStrategy
import Network.AWS.EC2.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a placement group.
--
--
--
-- /See:/ 'placementGroup' smart constructor.
data PlacementGroup = PlacementGroup'
  { _pgState ::
      !(Maybe PlacementGroupState),
    _pgStrategy :: !(Maybe PlacementStrategy),
    _pgGroupId :: !(Maybe Text),
    _pgGroupName :: !(Maybe Text),
    _pgPartitionCount :: !(Maybe Int),
    _pgTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PlacementGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pgState' - The state of the placement group.
--
-- * 'pgStrategy' - The placement strategy.
--
-- * 'pgGroupId' - The ID of the placement group.
--
-- * 'pgGroupName' - The name of the placement group.
--
-- * 'pgPartitionCount' - The number of partitions. Valid only if __strategy__ is set to @partition@ .
--
-- * 'pgTags' - Any tags applied to the placement group.
placementGroup ::
  PlacementGroup
placementGroup =
  PlacementGroup'
    { _pgState = Nothing,
      _pgStrategy = Nothing,
      _pgGroupId = Nothing,
      _pgGroupName = Nothing,
      _pgPartitionCount = Nothing,
      _pgTags = Nothing
    }

-- | The state of the placement group.
pgState :: Lens' PlacementGroup (Maybe PlacementGroupState)
pgState = lens _pgState (\s a -> s {_pgState = a})

-- | The placement strategy.
pgStrategy :: Lens' PlacementGroup (Maybe PlacementStrategy)
pgStrategy = lens _pgStrategy (\s a -> s {_pgStrategy = a})

-- | The ID of the placement group.
pgGroupId :: Lens' PlacementGroup (Maybe Text)
pgGroupId = lens _pgGroupId (\s a -> s {_pgGroupId = a})

-- | The name of the placement group.
pgGroupName :: Lens' PlacementGroup (Maybe Text)
pgGroupName = lens _pgGroupName (\s a -> s {_pgGroupName = a})

-- | The number of partitions. Valid only if __strategy__ is set to @partition@ .
pgPartitionCount :: Lens' PlacementGroup (Maybe Int)
pgPartitionCount = lens _pgPartitionCount (\s a -> s {_pgPartitionCount = a})

-- | Any tags applied to the placement group.
pgTags :: Lens' PlacementGroup [Tag]
pgTags = lens _pgTags (\s a -> s {_pgTags = a}) . _Default . _Coerce

instance FromXML PlacementGroup where
  parseXML x =
    PlacementGroup'
      <$> (x .@? "state")
      <*> (x .@? "strategy")
      <*> (x .@? "groupId")
      <*> (x .@? "groupName")
      <*> (x .@? "partitionCount")
      <*> (x .@? "tagSet" .!@ mempty >>= may (parseXMLList "item"))

instance Hashable PlacementGroup

instance NFData PlacementGroup
