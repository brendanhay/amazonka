{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TagDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TagDescription where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ResourceType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a tag.
--
--
--
-- /See:/ 'tagDescription' smart constructor.
data TagDescription = TagDescription'
  { _tdResourceId :: !Text,
    _tdResourceType :: !ResourceType,
    _tdKey :: !Text,
    _tdValue :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TagDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdResourceId' - The ID of the resource.
--
-- * 'tdResourceType' - The resource type.
--
-- * 'tdKey' - The tag key.
--
-- * 'tdValue' - The tag value.
tagDescription ::
  -- | 'tdResourceId'
  Text ->
  -- | 'tdResourceType'
  ResourceType ->
  -- | 'tdKey'
  Text ->
  -- | 'tdValue'
  Text ->
  TagDescription
tagDescription pResourceId_ pResourceType_ pKey_ pValue_ =
  TagDescription'
    { _tdResourceId = pResourceId_,
      _tdResourceType = pResourceType_,
      _tdKey = pKey_,
      _tdValue = pValue_
    }

-- | The ID of the resource.
tdResourceId :: Lens' TagDescription Text
tdResourceId = lens _tdResourceId (\s a -> s {_tdResourceId = a})

-- | The resource type.
tdResourceType :: Lens' TagDescription ResourceType
tdResourceType = lens _tdResourceType (\s a -> s {_tdResourceType = a})

-- | The tag key.
tdKey :: Lens' TagDescription Text
tdKey = lens _tdKey (\s a -> s {_tdKey = a})

-- | The tag value.
tdValue :: Lens' TagDescription Text
tdValue = lens _tdValue (\s a -> s {_tdValue = a})

instance FromXML TagDescription where
  parseXML x =
    TagDescription'
      <$> (x .@ "resourceId")
      <*> (x .@ "resourceType")
      <*> (x .@ "key")
      <*> (x .@ "value")

instance Hashable TagDescription

instance NFData TagDescription
