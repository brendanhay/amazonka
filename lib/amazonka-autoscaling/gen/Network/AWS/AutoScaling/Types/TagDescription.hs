{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.TagDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.TagDescription where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a tag for an Auto Scaling group.
--
--
--
-- /See:/ 'tagDescription' smart constructor.
data TagDescription = TagDescription'
  { _tdResourceId :: !Text,
    _tdResourceType :: !Text,
    _tdKey :: !Text,
    _tdPropagateAtLaunch :: !Bool,
    _tdValue :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TagDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdResourceId' - The name of the group.
--
-- * 'tdResourceType' - The type of resource. The only supported value is @auto-scaling-group@ .
--
-- * 'tdKey' - The tag key.
--
-- * 'tdPropagateAtLaunch' - Determines whether the tag is added to new instances as they are launched in the group.
--
-- * 'tdValue' - The tag value.
tagDescription ::
  -- | 'tdResourceId'
  Text ->
  -- | 'tdResourceType'
  Text ->
  -- | 'tdKey'
  Text ->
  -- | 'tdPropagateAtLaunch'
  Bool ->
  -- | 'tdValue'
  Text ->
  TagDescription
tagDescription
  pResourceId_
  pResourceType_
  pKey_
  pPropagateAtLaunch_
  pValue_ =
    TagDescription'
      { _tdResourceId = pResourceId_,
        _tdResourceType = pResourceType_,
        _tdKey = pKey_,
        _tdPropagateAtLaunch = pPropagateAtLaunch_,
        _tdValue = pValue_
      }

-- | The name of the group.
tdResourceId :: Lens' TagDescription Text
tdResourceId = lens _tdResourceId (\s a -> s {_tdResourceId = a})

-- | The type of resource. The only supported value is @auto-scaling-group@ .
tdResourceType :: Lens' TagDescription Text
tdResourceType = lens _tdResourceType (\s a -> s {_tdResourceType = a})

-- | The tag key.
tdKey :: Lens' TagDescription Text
tdKey = lens _tdKey (\s a -> s {_tdKey = a})

-- | Determines whether the tag is added to new instances as they are launched in the group.
tdPropagateAtLaunch :: Lens' TagDescription Bool
tdPropagateAtLaunch = lens _tdPropagateAtLaunch (\s a -> s {_tdPropagateAtLaunch = a})

-- | The tag value.
tdValue :: Lens' TagDescription Text
tdValue = lens _tdValue (\s a -> s {_tdValue = a})

instance FromXML TagDescription where
  parseXML x =
    TagDescription'
      <$> (x .@ "ResourceId")
      <*> (x .@ "ResourceType")
      <*> (x .@ "Key")
      <*> (x .@ "PropagateAtLaunch")
      <*> (x .@ "Value")

instance Hashable TagDescription

instance NFData TagDescription
