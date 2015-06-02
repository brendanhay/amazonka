{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.WorkSpaces.CreateWorkspaces
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Creates one or more WorkSpaces.
--
-- This operation is asynchronous and returns before the WorkSpaces are
-- created.
--
--
--
-- <http://docs.aws.amazon.com/workspaces/latest/devguide/API_CreateWorkspaces.html>
module Network.AWS.WorkSpaces.CreateWorkspaces
    (
    -- * Request
      CreateWorkspaces
    -- ** Request constructor
    , createWorkspaces
    -- ** Request lenses
    , cwWorkspaces

    -- * Response
    , CreateWorkspacesResponse
    -- ** Response constructor
    , createWorkspacesResponse
    -- ** Response lenses
    , cwrFailedRequests
    , cwrPendingRequests
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.WorkSpaces.Types
import qualified GHC.Exts

newtype CreateWorkspaces = CreateWorkspaces
    { _cwWorkspaces :: List1 "Workspaces" WorkspaceRequest
    } deriving (Eq, Read, Show, Semigroup)

-- | 'CreateWorkspaces' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cwWorkspaces' @::@ 'NonEmpty' 'WorkspaceRequest'
--
createWorkspaces :: NonEmpty WorkspaceRequest -- ^ 'cwWorkspaces'
                 -> CreateWorkspaces
createWorkspaces p1 = CreateWorkspaces
    { _cwWorkspaces = withIso _List1 (const id) p1
    }

-- | An array of structures that specify the WorkSpaces to create.
cwWorkspaces :: Lens' CreateWorkspaces (NonEmpty WorkspaceRequest)
cwWorkspaces = lens _cwWorkspaces (\s a -> s { _cwWorkspaces = a }) . _List1

data CreateWorkspacesResponse = CreateWorkspacesResponse
    { _cwrFailedRequests  :: List "FailedRequests" FailedCreateWorkspaceRequest
    , _cwrPendingRequests :: List "PendingRequests" Workspace
    } deriving (Eq, Read, Show)

-- | 'CreateWorkspacesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cwrFailedRequests' @::@ ['FailedCreateWorkspaceRequest']
--
-- * 'cwrPendingRequests' @::@ ['Workspace']
--
createWorkspacesResponse :: CreateWorkspacesResponse
createWorkspacesResponse = CreateWorkspacesResponse
    { _cwrFailedRequests  = mempty
    , _cwrPendingRequests = mempty
    }

-- | An array of structures that represent the WorkSpaces that could not be
-- created.
cwrFailedRequests :: Lens' CreateWorkspacesResponse [FailedCreateWorkspaceRequest]
cwrFailedRequests =
    lens _cwrFailedRequests (\s a -> s { _cwrFailedRequests = a })
        . _List

-- | An array of structures that represent the WorkSpaces that were created.
--
-- Because this operation is asynchronous, the identifier in 'WorkspaceId' is not
-- immediately available. If you immediately call 'DescribeWorkspaces' with this
-- identifier, no information will be returned.
cwrPendingRequests :: Lens' CreateWorkspacesResponse [Workspace]
cwrPendingRequests =
    lens _cwrPendingRequests (\s a -> s { _cwrPendingRequests = a })
        . _List

instance ToPath CreateWorkspaces where
    toPath = const "/"

instance ToQuery CreateWorkspaces where
    toQuery = const mempty

instance ToHeaders CreateWorkspaces

instance ToJSON CreateWorkspaces where
    toJSON CreateWorkspaces{..} = object
        [ "Workspaces" .= _cwWorkspaces
        ]

instance AWSRequest CreateWorkspaces where
    type Sv CreateWorkspaces = WorkSpaces
    type Rs CreateWorkspaces = CreateWorkspacesResponse

    request  = post "CreateWorkspaces"
    response = jsonResponse

instance FromJSON CreateWorkspacesResponse where
    parseJSON = withObject "CreateWorkspacesResponse" $ \o -> CreateWorkspacesResponse
        <$> o .:? "FailedRequests" .!= mempty
        <*> o .:? "PendingRequests" .!= mempty
