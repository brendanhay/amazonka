{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.GroupOwnerSetting
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.GroupOwnerSetting where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Group owner related settings for local resources.
--
-- /See:/ 'groupOwnerSetting' smart constructor.
data GroupOwnerSetting = GroupOwnerSetting'
  { _gosAutoAddGroupOwner ::
      !(Maybe Bool),
    _gosGroupOwner :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GroupOwnerSetting' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gosAutoAddGroupOwner' - If true, AWS IoT Greengrass automatically adds the specified Linux OS group owner of the resource to the Lambda process privileges. Thus the Lambda process will have the file access permissions of the added Linux group.
--
-- * 'gosGroupOwner' - The name of the Linux OS group whose privileges will be added to the Lambda process. This field is optional.
groupOwnerSetting ::
  GroupOwnerSetting
groupOwnerSetting =
  GroupOwnerSetting'
    { _gosAutoAddGroupOwner = Nothing,
      _gosGroupOwner = Nothing
    }

-- | If true, AWS IoT Greengrass automatically adds the specified Linux OS group owner of the resource to the Lambda process privileges. Thus the Lambda process will have the file access permissions of the added Linux group.
gosAutoAddGroupOwner :: Lens' GroupOwnerSetting (Maybe Bool)
gosAutoAddGroupOwner = lens _gosAutoAddGroupOwner (\s a -> s {_gosAutoAddGroupOwner = a})

-- | The name of the Linux OS group whose privileges will be added to the Lambda process. This field is optional.
gosGroupOwner :: Lens' GroupOwnerSetting (Maybe Text)
gosGroupOwner = lens _gosGroupOwner (\s a -> s {_gosGroupOwner = a})

instance FromJSON GroupOwnerSetting where
  parseJSON =
    withObject
      "GroupOwnerSetting"
      ( \x ->
          GroupOwnerSetting'
            <$> (x .:? "AutoAddGroupOwner") <*> (x .:? "GroupOwner")
      )

instance Hashable GroupOwnerSetting

instance NFData GroupOwnerSetting

instance ToJSON GroupOwnerSetting where
  toJSON GroupOwnerSetting' {..} =
    object
      ( catMaybes
          [ ("AutoAddGroupOwner" .=) <$> _gosAutoAddGroupOwner,
            ("GroupOwner" .=) <$> _gosGroupOwner
          ]
      )
