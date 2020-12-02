{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.ImagePermissions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.ImagePermissions where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the permissions for an image.
--
--
--
-- /See:/ 'imagePermissions' smart constructor.
data ImagePermissions = ImagePermissions'
  { _ipAllowFleet ::
      !(Maybe Bool),
    _ipAllowImageBuilder :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ImagePermissions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ipAllowFleet' - Indicates whether the image can be used for a fleet.
--
-- * 'ipAllowImageBuilder' - Indicates whether the image can be used for an image builder.
imagePermissions ::
  ImagePermissions
imagePermissions =
  ImagePermissions'
    { _ipAllowFleet = Nothing,
      _ipAllowImageBuilder = Nothing
    }

-- | Indicates whether the image can be used for a fleet.
ipAllowFleet :: Lens' ImagePermissions (Maybe Bool)
ipAllowFleet = lens _ipAllowFleet (\s a -> s {_ipAllowFleet = a})

-- | Indicates whether the image can be used for an image builder.
ipAllowImageBuilder :: Lens' ImagePermissions (Maybe Bool)
ipAllowImageBuilder = lens _ipAllowImageBuilder (\s a -> s {_ipAllowImageBuilder = a})

instance FromJSON ImagePermissions where
  parseJSON =
    withObject
      "ImagePermissions"
      ( \x ->
          ImagePermissions'
            <$> (x .:? "allowFleet") <*> (x .:? "allowImageBuilder")
      )

instance Hashable ImagePermissions

instance NFData ImagePermissions

instance ToJSON ImagePermissions where
  toJSON ImagePermissions' {..} =
    object
      ( catMaybes
          [ ("allowFleet" .=) <$> _ipAllowFleet,
            ("allowImageBuilder" .=) <$> _ipAllowImageBuilder
          ]
      )
