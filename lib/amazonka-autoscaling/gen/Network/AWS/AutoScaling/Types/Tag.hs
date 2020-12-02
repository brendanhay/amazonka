{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.Tag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.Tag where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a tag for an Auto Scaling group.
--
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
  { _tagKey :: !Text,
    _tagResourceId :: !Text,
    _tagResourceType :: !Text,
    _tagPropagateAtLaunch :: !Bool,
    _tagValue :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagKey' - The tag key.
--
-- * 'tagResourceId' - The name of the group.
--
-- * 'tagResourceType' - The type of resource. The only supported value is @auto-scaling-group@ .
--
-- * 'tagPropagateAtLaunch' - Determines whether the tag is added to new instances as they are launched in the group.
--
-- * 'tagValue' - The tag value.
tag ::
  -- | 'tagKey'
  Text ->
  -- | 'tagResourceId'
  Text ->
  -- | 'tagResourceType'
  Text ->
  -- | 'tagPropagateAtLaunch'
  Bool ->
  -- | 'tagValue'
  Text ->
  Tag
tag pKey_ pResourceId_ pResourceType_ pPropagateAtLaunch_ pValue_ =
  Tag'
    { _tagKey = pKey_,
      _tagResourceId = pResourceId_,
      _tagResourceType = pResourceType_,
      _tagPropagateAtLaunch = pPropagateAtLaunch_,
      _tagValue = pValue_
    }

-- | The tag key.
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\s a -> s {_tagKey = a})

-- | The name of the group.
tagResourceId :: Lens' Tag Text
tagResourceId = lens _tagResourceId (\s a -> s {_tagResourceId = a})

-- | The type of resource. The only supported value is @auto-scaling-group@ .
tagResourceType :: Lens' Tag Text
tagResourceType = lens _tagResourceType (\s a -> s {_tagResourceType = a})

-- | Determines whether the tag is added to new instances as they are launched in the group.
tagPropagateAtLaunch :: Lens' Tag Bool
tagPropagateAtLaunch = lens _tagPropagateAtLaunch (\s a -> s {_tagPropagateAtLaunch = a})

-- | The tag value.
tagValue :: Lens' Tag Text
tagValue = lens _tagValue (\s a -> s {_tagValue = a})

instance Hashable Tag

instance NFData Tag

instance ToQuery Tag where
  toQuery Tag' {..} =
    mconcat
      [ "Key" =: _tagKey,
        "ResourceId" =: _tagResourceId,
        "ResourceType" =: _tagResourceType,
        "PropagateAtLaunch" =: _tagPropagateAtLaunch,
        "Value" =: _tagValue
      ]
