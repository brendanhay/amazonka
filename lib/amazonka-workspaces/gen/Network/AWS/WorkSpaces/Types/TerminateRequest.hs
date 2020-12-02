{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.TerminateRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.TerminateRequest where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the information used to terminate a WorkSpace.
--
--
--
-- /See:/ 'terminateRequest' smart constructor.
newtype TerminateRequest = TerminateRequest'
  { _trWorkspaceId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TerminateRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trWorkspaceId' - The identifier of the WorkSpace.
terminateRequest ::
  -- | 'trWorkspaceId'
  Text ->
  TerminateRequest
terminateRequest pWorkspaceId_ =
  TerminateRequest' {_trWorkspaceId = pWorkspaceId_}

-- | The identifier of the WorkSpace.
trWorkspaceId :: Lens' TerminateRequest Text
trWorkspaceId = lens _trWorkspaceId (\s a -> s {_trWorkspaceId = a})

instance Hashable TerminateRequest

instance NFData TerminateRequest

instance ToJSON TerminateRequest where
  toJSON TerminateRequest' {..} =
    object (catMaybes [Just ("WorkspaceId" .= _trWorkspaceId)])
