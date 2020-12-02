{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.StartRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.StartRequest where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information used to start a WorkSpace.
--
--
--
-- /See:/ 'startRequest' smart constructor.
newtype StartRequest = StartRequest' {_sWorkspaceId :: Maybe Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sWorkspaceId' - The identifier of the WorkSpace.
startRequest ::
  StartRequest
startRequest = StartRequest' {_sWorkspaceId = Nothing}

-- | The identifier of the WorkSpace.
sWorkspaceId :: Lens' StartRequest (Maybe Text)
sWorkspaceId = lens _sWorkspaceId (\s a -> s {_sWorkspaceId = a})

instance Hashable StartRequest

instance NFData StartRequest

instance ToJSON StartRequest where
  toJSON StartRequest' {..} =
    object (catMaybes [("WorkspaceId" .=) <$> _sWorkspaceId])
