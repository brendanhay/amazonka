{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.AddThingToThingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a thing to a thing group.
module Network.AWS.IoT.AddThingToThingGroup
  ( -- * Creating a Request
    addThingToThingGroup,
    AddThingToThingGroup,

    -- * Request Lenses
    atttgThingGroupARN,
    atttgThingARN,
    atttgThingGroupName,
    atttgOverrideDynamicGroups,
    atttgThingName,

    -- * Destructuring the Response
    addThingToThingGroupResponse,
    AddThingToThingGroupResponse,

    -- * Response Lenses
    atttgrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'addThingToThingGroup' smart constructor.
data AddThingToThingGroup = AddThingToThingGroup'
  { _atttgThingGroupARN ::
      !(Maybe Text),
    _atttgThingARN :: !(Maybe Text),
    _atttgThingGroupName :: !(Maybe Text),
    _atttgOverrideDynamicGroups :: !(Maybe Bool),
    _atttgThingName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AddThingToThingGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atttgThingGroupARN' - The ARN of the group to which you are adding a thing.
--
-- * 'atttgThingARN' - The ARN of the thing to add to a group.
--
-- * 'atttgThingGroupName' - The name of the group to which you are adding a thing.
--
-- * 'atttgOverrideDynamicGroups' - Override dynamic thing groups with static thing groups when 10-group limit is reached. If a thing belongs to 10 thing groups, and one or more of those groups are dynamic thing groups, adding a thing to a static group removes the thing from the last dynamic group.
--
-- * 'atttgThingName' - The name of the thing to add to a group.
addThingToThingGroup ::
  AddThingToThingGroup
addThingToThingGroup =
  AddThingToThingGroup'
    { _atttgThingGroupARN = Nothing,
      _atttgThingARN = Nothing,
      _atttgThingGroupName = Nothing,
      _atttgOverrideDynamicGroups = Nothing,
      _atttgThingName = Nothing
    }

-- | The ARN of the group to which you are adding a thing.
atttgThingGroupARN :: Lens' AddThingToThingGroup (Maybe Text)
atttgThingGroupARN = lens _atttgThingGroupARN (\s a -> s {_atttgThingGroupARN = a})

-- | The ARN of the thing to add to a group.
atttgThingARN :: Lens' AddThingToThingGroup (Maybe Text)
atttgThingARN = lens _atttgThingARN (\s a -> s {_atttgThingARN = a})

-- | The name of the group to which you are adding a thing.
atttgThingGroupName :: Lens' AddThingToThingGroup (Maybe Text)
atttgThingGroupName = lens _atttgThingGroupName (\s a -> s {_atttgThingGroupName = a})

-- | Override dynamic thing groups with static thing groups when 10-group limit is reached. If a thing belongs to 10 thing groups, and one or more of those groups are dynamic thing groups, adding a thing to a static group removes the thing from the last dynamic group.
atttgOverrideDynamicGroups :: Lens' AddThingToThingGroup (Maybe Bool)
atttgOverrideDynamicGroups = lens _atttgOverrideDynamicGroups (\s a -> s {_atttgOverrideDynamicGroups = a})

-- | The name of the thing to add to a group.
atttgThingName :: Lens' AddThingToThingGroup (Maybe Text)
atttgThingName = lens _atttgThingName (\s a -> s {_atttgThingName = a})

instance AWSRequest AddThingToThingGroup where
  type Rs AddThingToThingGroup = AddThingToThingGroupResponse
  request = putJSON ioT
  response =
    receiveEmpty
      (\s h x -> AddThingToThingGroupResponse' <$> (pure (fromEnum s)))

instance Hashable AddThingToThingGroup

instance NFData AddThingToThingGroup

instance ToHeaders AddThingToThingGroup where
  toHeaders = const mempty

instance ToJSON AddThingToThingGroup where
  toJSON AddThingToThingGroup' {..} =
    object
      ( catMaybes
          [ ("thingGroupArn" .=) <$> _atttgThingGroupARN,
            ("thingArn" .=) <$> _atttgThingARN,
            ("thingGroupName" .=) <$> _atttgThingGroupName,
            ("overrideDynamicGroups" .=) <$> _atttgOverrideDynamicGroups,
            ("thingName" .=) <$> _atttgThingName
          ]
      )

instance ToPath AddThingToThingGroup where
  toPath = const "/thing-groups/addThingToThingGroup"

instance ToQuery AddThingToThingGroup where
  toQuery = const mempty

-- | /See:/ 'addThingToThingGroupResponse' smart constructor.
newtype AddThingToThingGroupResponse = AddThingToThingGroupResponse'
  { _atttgrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AddThingToThingGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atttgrsResponseStatus' - -- | The response status code.
addThingToThingGroupResponse ::
  -- | 'atttgrsResponseStatus'
  Int ->
  AddThingToThingGroupResponse
addThingToThingGroupResponse pResponseStatus_ =
  AddThingToThingGroupResponse'
    { _atttgrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
atttgrsResponseStatus :: Lens' AddThingToThingGroupResponse Int
atttgrsResponseStatus = lens _atttgrsResponseStatus (\s a -> s {_atttgrsResponseStatus = a})

instance NFData AddThingToThingGroupResponse
