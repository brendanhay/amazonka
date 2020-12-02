{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.ResourceDefinitionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.ResourceDefinitionVersion where

import Network.AWS.Greengrass.Types.Resource
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a resource definition version.
--
-- /See:/ 'resourceDefinitionVersion' smart constructor.
newtype ResourceDefinitionVersion = ResourceDefinitionVersion'
  { _rdvResources ::
      Maybe [Resource]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceDefinitionVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdvResources' - A list of resources.
resourceDefinitionVersion ::
  ResourceDefinitionVersion
resourceDefinitionVersion =
  ResourceDefinitionVersion' {_rdvResources = Nothing}

-- | A list of resources.
rdvResources :: Lens' ResourceDefinitionVersion [Resource]
rdvResources = lens _rdvResources (\s a -> s {_rdvResources = a}) . _Default . _Coerce

instance FromJSON ResourceDefinitionVersion where
  parseJSON =
    withObject
      "ResourceDefinitionVersion"
      ( \x ->
          ResourceDefinitionVersion' <$> (x .:? "Resources" .!= mempty)
      )

instance Hashable ResourceDefinitionVersion

instance NFData ResourceDefinitionVersion

instance ToJSON ResourceDefinitionVersion where
  toJSON ResourceDefinitionVersion' {..} =
    object (catMaybes [("Resources" .=) <$> _rdvResources])
