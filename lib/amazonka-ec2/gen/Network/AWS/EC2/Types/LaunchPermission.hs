{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchPermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchPermission where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.PermissionGroup
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a launch permission.
--
--
--
-- /See:/ 'launchPermission' smart constructor.
data LaunchPermission = LaunchPermission'
  { _lGroup ::
      !(Maybe PermissionGroup),
    _lUserId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LaunchPermission' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lGroup' - The name of the group.
--
-- * 'lUserId' - The AWS account ID.
launchPermission ::
  LaunchPermission
launchPermission =
  LaunchPermission' {_lGroup = Nothing, _lUserId = Nothing}

-- | The name of the group.
lGroup :: Lens' LaunchPermission (Maybe PermissionGroup)
lGroup = lens _lGroup (\s a -> s {_lGroup = a})

-- | The AWS account ID.
lUserId :: Lens' LaunchPermission (Maybe Text)
lUserId = lens _lUserId (\s a -> s {_lUserId = a})

instance FromXML LaunchPermission where
  parseXML x =
    LaunchPermission' <$> (x .@? "group") <*> (x .@? "userId")

instance Hashable LaunchPermission

instance NFData LaunchPermission

instance ToQuery LaunchPermission where
  toQuery LaunchPermission' {..} =
    mconcat ["Group" =: _lGroup, "UserId" =: _lUserId]
