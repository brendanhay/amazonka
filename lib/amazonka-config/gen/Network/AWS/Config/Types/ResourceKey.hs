{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ResourceKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ResourceKey where

import Network.AWS.Config.Types.ResourceType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The details that identify a resource within AWS Config, including the resource type and resource ID.
--
--
--
-- /See:/ 'resourceKey' smart constructor.
data ResourceKey = ResourceKey'
  { _rkResourceType :: !ResourceType,
    _rkResourceId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rkResourceType' - The resource type.
--
-- * 'rkResourceId' - The ID of the resource (for example., sg-xxxxxx).
resourceKey ::
  -- | 'rkResourceType'
  ResourceType ->
  -- | 'rkResourceId'
  Text ->
  ResourceKey
resourceKey pResourceType_ pResourceId_ =
  ResourceKey'
    { _rkResourceType = pResourceType_,
      _rkResourceId = pResourceId_
    }

-- | The resource type.
rkResourceType :: Lens' ResourceKey ResourceType
rkResourceType = lens _rkResourceType (\s a -> s {_rkResourceType = a})

-- | The ID of the resource (for example., sg-xxxxxx).
rkResourceId :: Lens' ResourceKey Text
rkResourceId = lens _rkResourceId (\s a -> s {_rkResourceId = a})

instance FromJSON ResourceKey where
  parseJSON =
    withObject
      "ResourceKey"
      ( \x ->
          ResourceKey' <$> (x .: "resourceType") <*> (x .: "resourceId")
      )

instance Hashable ResourceKey

instance NFData ResourceKey

instance ToJSON ResourceKey where
  toJSON ResourceKey' {..} =
    object
      ( catMaybes
          [ Just ("resourceType" .= _rkResourceType),
            Just ("resourceId" .= _rkResourceId)
          ]
      )
