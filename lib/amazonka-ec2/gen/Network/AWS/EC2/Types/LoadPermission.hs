{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LoadPermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LoadPermission where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.PermissionGroup
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a load permission.
--
--
--
-- /See:/ 'loadPermission' smart constructor.
data LoadPermission = LoadPermission'
  { _lpGroup ::
      !(Maybe PermissionGroup),
    _lpUserId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LoadPermission' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpGroup' - The name of the group.
--
-- * 'lpUserId' - The AWS account ID.
loadPermission ::
  LoadPermission
loadPermission =
  LoadPermission' {_lpGroup = Nothing, _lpUserId = Nothing}

-- | The name of the group.
lpGroup :: Lens' LoadPermission (Maybe PermissionGroup)
lpGroup = lens _lpGroup (\s a -> s {_lpGroup = a})

-- | The AWS account ID.
lpUserId :: Lens' LoadPermission (Maybe Text)
lpUserId = lens _lpUserId (\s a -> s {_lpUserId = a})

instance FromXML LoadPermission where
  parseXML x =
    LoadPermission' <$> (x .@? "group") <*> (x .@? "userId")

instance Hashable LoadPermission

instance NFData LoadPermission
