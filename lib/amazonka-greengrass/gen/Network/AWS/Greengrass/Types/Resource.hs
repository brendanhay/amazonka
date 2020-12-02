{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.Resource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.Resource where

import Network.AWS.Greengrass.Types.ResourceDataContainer
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a resource.
--
-- /See:/ 'resource' smart constructor.
data Resource = Resource'
  { _rResourceDataContainer ::
      !ResourceDataContainer,
    _rId :: !Text,
    _rName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Resource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rResourceDataContainer' - A container of data for all resource types.
--
-- * 'rId' - The resource ID, used to refer to a resource in the Lambda function configuration. Max length is 128 characters with pattern ''[a-zA-Z0-9:_-]+''. This must be unique within a Greengrass group.
--
-- * 'rName' - The descriptive resource name, which is displayed on the AWS IoT Greengrass console. Max length 128 characters with pattern ''[a-zA-Z0-9:_-]+''. This must be unique within a Greengrass group.
resource ::
  -- | 'rResourceDataContainer'
  ResourceDataContainer ->
  -- | 'rId'
  Text ->
  -- | 'rName'
  Text ->
  Resource
resource pResourceDataContainer_ pId_ pName_ =
  Resource'
    { _rResourceDataContainer = pResourceDataContainer_,
      _rId = pId_,
      _rName = pName_
    }

-- | A container of data for all resource types.
rResourceDataContainer :: Lens' Resource ResourceDataContainer
rResourceDataContainer = lens _rResourceDataContainer (\s a -> s {_rResourceDataContainer = a})

-- | The resource ID, used to refer to a resource in the Lambda function configuration. Max length is 128 characters with pattern ''[a-zA-Z0-9:_-]+''. This must be unique within a Greengrass group.
rId :: Lens' Resource Text
rId = lens _rId (\s a -> s {_rId = a})

-- | The descriptive resource name, which is displayed on the AWS IoT Greengrass console. Max length 128 characters with pattern ''[a-zA-Z0-9:_-]+''. This must be unique within a Greengrass group.
rName :: Lens' Resource Text
rName = lens _rName (\s a -> s {_rName = a})

instance FromJSON Resource where
  parseJSON =
    withObject
      "Resource"
      ( \x ->
          Resource'
            <$> (x .: "ResourceDataContainer") <*> (x .: "Id") <*> (x .: "Name")
      )

instance Hashable Resource

instance NFData Resource

instance ToJSON Resource where
  toJSON Resource' {..} =
    object
      ( catMaybes
          [ Just ("ResourceDataContainer" .= _rResourceDataContainer),
            Just ("Id" .= _rId),
            Just ("Name" .= _rName)
          ]
      )
