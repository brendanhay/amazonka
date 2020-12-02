{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.RebuildRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.RebuildRequest where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the information used to rebuild a WorkSpace.
--
--
--
-- /See:/ 'rebuildRequest' smart constructor.
newtype RebuildRequest = RebuildRequest' {_rrWorkspaceId :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RebuildRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrWorkspaceId' - The identifier of the WorkSpace.
rebuildRequest ::
  -- | 'rrWorkspaceId'
  Text ->
  RebuildRequest
rebuildRequest pWorkspaceId_ =
  RebuildRequest' {_rrWorkspaceId = pWorkspaceId_}

-- | The identifier of the WorkSpace.
rrWorkspaceId :: Lens' RebuildRequest Text
rrWorkspaceId = lens _rrWorkspaceId (\s a -> s {_rrWorkspaceId = a})

instance Hashable RebuildRequest

instance NFData RebuildRequest

instance ToJSON RebuildRequest where
  toJSON RebuildRequest' {..} =
    object (catMaybes [Just ("WorkspaceId" .= _rrWorkspaceId)])
