{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ResourceIdentifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ResourceIdentifier where

import Network.AWS.Config.Types.ResourceType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The details that identify a resource that is discovered by AWS Config, including the resource type, ID, and (if available) the custom resource name.
--
--
--
-- /See:/ 'resourceIdentifier' smart constructor.
data ResourceIdentifier = ResourceIdentifier'
  { _riResourceId ::
      !(Maybe Text),
    _riResourceType :: !(Maybe ResourceType),
    _riResourceName :: !(Maybe Text),
    _riResourceDeletionTime :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceIdentifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'riResourceId' - The ID of the resource (for example, @sg-xxxxxx@ ).
--
-- * 'riResourceType' - The type of resource.
--
-- * 'riResourceName' - The custom name of the resource (if available).
--
-- * 'riResourceDeletionTime' - The time that the resource was deleted.
resourceIdentifier ::
  ResourceIdentifier
resourceIdentifier =
  ResourceIdentifier'
    { _riResourceId = Nothing,
      _riResourceType = Nothing,
      _riResourceName = Nothing,
      _riResourceDeletionTime = Nothing
    }

-- | The ID of the resource (for example, @sg-xxxxxx@ ).
riResourceId :: Lens' ResourceIdentifier (Maybe Text)
riResourceId = lens _riResourceId (\s a -> s {_riResourceId = a})

-- | The type of resource.
riResourceType :: Lens' ResourceIdentifier (Maybe ResourceType)
riResourceType = lens _riResourceType (\s a -> s {_riResourceType = a})

-- | The custom name of the resource (if available).
riResourceName :: Lens' ResourceIdentifier (Maybe Text)
riResourceName = lens _riResourceName (\s a -> s {_riResourceName = a})

-- | The time that the resource was deleted.
riResourceDeletionTime :: Lens' ResourceIdentifier (Maybe UTCTime)
riResourceDeletionTime = lens _riResourceDeletionTime (\s a -> s {_riResourceDeletionTime = a}) . mapping _Time

instance FromJSON ResourceIdentifier where
  parseJSON =
    withObject
      "ResourceIdentifier"
      ( \x ->
          ResourceIdentifier'
            <$> (x .:? "resourceId")
            <*> (x .:? "resourceType")
            <*> (x .:? "resourceName")
            <*> (x .:? "resourceDeletionTime")
      )

instance Hashable ResourceIdentifier

instance NFData ResourceIdentifier
