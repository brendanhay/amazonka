{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LoadPermissionRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LoadPermissionRequest where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.PermissionGroup
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a load permission.
--
--
--
-- /See:/ 'loadPermissionRequest' smart constructor.
data LoadPermissionRequest = LoadPermissionRequest'
  { _lprGroup ::
      !(Maybe PermissionGroup),
    _lprUserId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LoadPermissionRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lprGroup' - The name of the group.
--
-- * 'lprUserId' - The AWS account ID.
loadPermissionRequest ::
  LoadPermissionRequest
loadPermissionRequest =
  LoadPermissionRequest' {_lprGroup = Nothing, _lprUserId = Nothing}

-- | The name of the group.
lprGroup :: Lens' LoadPermissionRequest (Maybe PermissionGroup)
lprGroup = lens _lprGroup (\s a -> s {_lprGroup = a})

-- | The AWS account ID.
lprUserId :: Lens' LoadPermissionRequest (Maybe Text)
lprUserId = lens _lprUserId (\s a -> s {_lprUserId = a})

instance Hashable LoadPermissionRequest

instance NFData LoadPermissionRequest

instance ToQuery LoadPermissionRequest where
  toQuery LoadPermissionRequest' {..} =
    mconcat ["Group" =: _lprGroup, "UserId" =: _lprUserId]
