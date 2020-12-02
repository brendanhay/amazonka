{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.Relationship
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.Relationship where

import Network.AWS.Config.Types.ResourceType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The relationship of the related resource to the main resource.
--
--
--
-- /See:/ 'relationship' smart constructor.
data Relationship = Relationship'
  { _rResourceId :: !(Maybe Text),
    _rResourceType :: !(Maybe ResourceType),
    _rResourceName :: !(Maybe Text),
    _rRelationshipName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Relationship' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rResourceId' - The ID of the related resource (for example, @sg-xxxxxx@ ).
--
-- * 'rResourceType' - The resource type of the related resource.
--
-- * 'rResourceName' - The custom name of the related resource, if available.
--
-- * 'rRelationshipName' - The type of relationship with the related resource.
relationship ::
  Relationship
relationship =
  Relationship'
    { _rResourceId = Nothing,
      _rResourceType = Nothing,
      _rResourceName = Nothing,
      _rRelationshipName = Nothing
    }

-- | The ID of the related resource (for example, @sg-xxxxxx@ ).
rResourceId :: Lens' Relationship (Maybe Text)
rResourceId = lens _rResourceId (\s a -> s {_rResourceId = a})

-- | The resource type of the related resource.
rResourceType :: Lens' Relationship (Maybe ResourceType)
rResourceType = lens _rResourceType (\s a -> s {_rResourceType = a})

-- | The custom name of the related resource, if available.
rResourceName :: Lens' Relationship (Maybe Text)
rResourceName = lens _rResourceName (\s a -> s {_rResourceName = a})

-- | The type of relationship with the related resource.
rRelationshipName :: Lens' Relationship (Maybe Text)
rRelationshipName = lens _rRelationshipName (\s a -> s {_rRelationshipName = a})

instance FromJSON Relationship where
  parseJSON =
    withObject
      "Relationship"
      ( \x ->
          Relationship'
            <$> (x .:? "resourceId")
            <*> (x .:? "resourceType")
            <*> (x .:? "resourceName")
            <*> (x .:? "relationshipName")
      )

instance Hashable Relationship

instance NFData Relationship
