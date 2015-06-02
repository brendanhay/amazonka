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

-- Module      : Network.AWS.WorkSpaces.TerminateWorkspaces
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

-- | Terminates the specified WorkSpaces.
--
-- Terminating a WorkSpace is a permanent action and cannot be undone. The
-- user's data is not maintained and will be destroyed. If you need to archive
-- any user data, contact Amazon Web Services before terminating the WorkSpace.
--
-- You can terminate a WorkSpace that is in any state except 'SUSPENDED'.
--
-- This operation is asynchronous and will return before the WorkSpaces have
-- been completely terminated.
--
--
--
-- <http://docs.aws.amazon.com/workspaces/latest/devguide/API_TerminateWorkspaces.html>
module Network.AWS.WorkSpaces.TerminateWorkspaces
    (
    -- * Request
      TerminateWorkspaces
    -- ** Request constructor
    , terminateWorkspaces
    -- ** Request lenses
    , twTerminateWorkspaceRequests

    -- * Response
    , TerminateWorkspacesResponse
    -- ** Response constructor
    , terminateWorkspacesResponse
    -- ** Response lenses
    , twrFailedRequests
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.WorkSpaces.Types
import qualified GHC.Exts

newtype TerminateWorkspaces = TerminateWorkspaces
    { _twTerminateWorkspaceRequests :: List1 "TerminateWorkspaceRequests" TerminateRequest
    } deriving (Eq, Read, Show, Semigroup)

-- | 'TerminateWorkspaces' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'twTerminateWorkspaceRequests' @::@ 'NonEmpty' 'TerminateRequest'
--
terminateWorkspaces :: NonEmpty TerminateRequest -- ^ 'twTerminateWorkspaceRequests'
                    -> TerminateWorkspaces
terminateWorkspaces p1 = TerminateWorkspaces
    { _twTerminateWorkspaceRequests = withIso _List1 (const id) p1
    }

-- | An array of structures that specify the WorkSpaces to terminate.
twTerminateWorkspaceRequests :: Lens' TerminateWorkspaces (NonEmpty TerminateRequest)
twTerminateWorkspaceRequests =
    lens _twTerminateWorkspaceRequests
        (\s a -> s { _twTerminateWorkspaceRequests = a })
            . _List1

newtype TerminateWorkspacesResponse = TerminateWorkspacesResponse
    { _twrFailedRequests :: List "FailedRequests" FailedWorkspaceChangeRequest
    } deriving (Eq, Read, Show, Monoid, Semigroup)

instance GHC.Exts.IsList TerminateWorkspacesResponse where
    type Item TerminateWorkspacesResponse = FailedWorkspaceChangeRequest

    fromList = TerminateWorkspacesResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _twrFailedRequests

-- | 'TerminateWorkspacesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'twrFailedRequests' @::@ ['FailedWorkspaceChangeRequest']
--
terminateWorkspacesResponse :: TerminateWorkspacesResponse
terminateWorkspacesResponse = TerminateWorkspacesResponse
    { _twrFailedRequests = mempty
    }

-- | An array of structures that represent any WorkSpaces that could not be
-- terminated.
twrFailedRequests :: Lens' TerminateWorkspacesResponse [FailedWorkspaceChangeRequest]
twrFailedRequests =
    lens _twrFailedRequests (\s a -> s { _twrFailedRequests = a })
        . _List

instance ToPath TerminateWorkspaces where
    toPath = const "/"

instance ToQuery TerminateWorkspaces where
    toQuery = const mempty

instance ToHeaders TerminateWorkspaces

instance ToJSON TerminateWorkspaces where
    toJSON TerminateWorkspaces{..} = object
        [ "TerminateWorkspaceRequests" .= _twTerminateWorkspaceRequests
        ]

instance AWSRequest TerminateWorkspaces where
    type Sv TerminateWorkspaces = WorkSpaces
    type Rs TerminateWorkspaces = TerminateWorkspacesResponse

    request  = post "TerminateWorkspaces"
    response = jsonResponse

instance FromJSON TerminateWorkspacesResponse where
    parseJSON = withObject "TerminateWorkspacesResponse" $ \o -> TerminateWorkspacesResponse
        <$> o .:? "FailedRequests" .!= mempty
