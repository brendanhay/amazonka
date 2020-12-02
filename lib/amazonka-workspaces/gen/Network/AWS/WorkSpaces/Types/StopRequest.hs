{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.StopRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.StopRequest where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the information used to stop a WorkSpace.
--
--
--
-- /See:/ 'stopRequest' smart constructor.
newtype StopRequest = StopRequest' {_srWorkspaceId :: Maybe Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StopRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srWorkspaceId' - The identifier of the WorkSpace.
stopRequest ::
  StopRequest
stopRequest = StopRequest' {_srWorkspaceId = Nothing}

-- | The identifier of the WorkSpace.
srWorkspaceId :: Lens' StopRequest (Maybe Text)
srWorkspaceId = lens _srWorkspaceId (\s a -> s {_srWorkspaceId = a})

instance Hashable StopRequest

instance NFData StopRequest

instance ToJSON StopRequest where
  toJSON StopRequest' {..} =
    object (catMaybes [("WorkspaceId" .=) <$> _srWorkspaceId])
