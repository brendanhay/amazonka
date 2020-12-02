{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.ResourceAccessPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.ResourceAccessPolicy where

import Network.AWS.Greengrass.Types.Permission
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A policy used by the function to access a resource.
--
-- /See:/ 'resourceAccessPolicy' smart constructor.
data ResourceAccessPolicy = ResourceAccessPolicy'
  { _rapPermission ::
      !(Maybe Permission),
    _rapResourceId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceAccessPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rapPermission' - The permissions that the Lambda function has to the resource. Can be one of ''rw'' (read/write) or ''ro'' (read-only).
--
-- * 'rapResourceId' - The ID of the resource. (This ID is assigned to the resource when you create the resource definiton.)
resourceAccessPolicy ::
  -- | 'rapResourceId'
  Text ->
  ResourceAccessPolicy
resourceAccessPolicy pResourceId_ =
  ResourceAccessPolicy'
    { _rapPermission = Nothing,
      _rapResourceId = pResourceId_
    }

-- | The permissions that the Lambda function has to the resource. Can be one of ''rw'' (read/write) or ''ro'' (read-only).
rapPermission :: Lens' ResourceAccessPolicy (Maybe Permission)
rapPermission = lens _rapPermission (\s a -> s {_rapPermission = a})

-- | The ID of the resource. (This ID is assigned to the resource when you create the resource definiton.)
rapResourceId :: Lens' ResourceAccessPolicy Text
rapResourceId = lens _rapResourceId (\s a -> s {_rapResourceId = a})

instance FromJSON ResourceAccessPolicy where
  parseJSON =
    withObject
      "ResourceAccessPolicy"
      ( \x ->
          ResourceAccessPolicy'
            <$> (x .:? "Permission") <*> (x .: "ResourceId")
      )

instance Hashable ResourceAccessPolicy

instance NFData ResourceAccessPolicy

instance ToJSON ResourceAccessPolicy where
  toJSON ResourceAccessPolicy' {..} =
    object
      ( catMaybes
          [ ("Permission" .=) <$> _rapPermission,
            Just ("ResourceId" .= _rapResourceId)
          ]
      )
