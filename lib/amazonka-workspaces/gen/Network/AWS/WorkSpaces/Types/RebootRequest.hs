{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.RebootRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.RebootRequest where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the information used to reboot a WorkSpace.
--
--
--
-- /See:/ 'rebootRequest' smart constructor.
newtype RebootRequest = RebootRequest' {_rWorkspaceId :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RebootRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rWorkspaceId' - The identifier of the WorkSpace.
rebootRequest ::
  -- | 'rWorkspaceId'
  Text ->
  RebootRequest
rebootRequest pWorkspaceId_ =
  RebootRequest' {_rWorkspaceId = pWorkspaceId_}

-- | The identifier of the WorkSpace.
rWorkspaceId :: Lens' RebootRequest Text
rWorkspaceId = lens _rWorkspaceId (\s a -> s {_rWorkspaceId = a})

instance Hashable RebootRequest

instance NFData RebootRequest

instance ToJSON RebootRequest where
  toJSON RebootRequest' {..} =
    object (catMaybes [Just ("WorkspaceId" .= _rWorkspaceId)])
