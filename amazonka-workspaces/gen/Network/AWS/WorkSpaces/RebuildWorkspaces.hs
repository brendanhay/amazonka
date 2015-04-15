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

-- Module      : Network.AWS.WorkSpaces.RebuildWorkspaces
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

-- | Rebuilds the specified WorkSpaces.
--
-- Rebuilding a WorkSpace is a potentially destructive action that can result
-- in the loss of data. Rebuilding a WorkSpace causes the following to occur:
--
-- The system is restored to the image of the bundle that the WorkSpace is
-- created from. Any applications that have been installed, or system settings
-- that have been made since the WorkSpace was created will be lost. The data
-- drive (D drive) is re-created from the last automatic snapshot taken of the
-- data drive. The current contents of the data drive are overwritten. Automatic
-- snapshots of the data drive are taken every 12 hours, so the snapshot can be
-- as much as 12 hours old.  To be able to rebuild a WorkSpace, the WorkSpace
-- must have a State of 'AVAILABLE' or 'ERROR'.
--
-- This operation is asynchronous and will return before the WorkSpaces have
-- been completely rebuilt.
--
--
--
-- <http://docs.aws.amazon.com/workspaces/latest/devguide/API_RebuildWorkspaces.html>
module Network.AWS.WorkSpaces.RebuildWorkspaces
    (
    -- * Request
      RebuildWorkspaces
    -- ** Request constructor
    , rebuildWorkspaces
    -- ** Request lenses
    , rwRebuildWorkspaceRequests

    -- * Response
    , RebuildWorkspacesResponse
    -- ** Response constructor
    , rebuildWorkspacesResponse
    -- ** Response lenses
    , rwrFailedRequests
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.WorkSpaces.Types
import qualified GHC.Exts

newtype RebuildWorkspaces = RebuildWorkspaces
    { _rwRebuildWorkspaceRequests :: List1 "RebuildWorkspaceRequests" RebuildRequest
    } deriving (Eq, Read, Show, Semigroup)

-- | 'RebuildWorkspaces' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rwRebuildWorkspaceRequests' @::@ 'NonEmpty' 'RebuildRequest'
--
rebuildWorkspaces :: NonEmpty RebuildRequest -- ^ 'rwRebuildWorkspaceRequests'
                  -> RebuildWorkspaces
rebuildWorkspaces p1 = RebuildWorkspaces
    { _rwRebuildWorkspaceRequests = withIso _List1 (const id) p1
    }

-- | An array of structures that specify the WorkSpaces to rebuild.
rwRebuildWorkspaceRequests :: Lens' RebuildWorkspaces (NonEmpty RebuildRequest)
rwRebuildWorkspaceRequests =
    lens _rwRebuildWorkspaceRequests
        (\s a -> s { _rwRebuildWorkspaceRequests = a })
            . _List1

newtype RebuildWorkspacesResponse = RebuildWorkspacesResponse
    { _rwrFailedRequests :: List "FailedRequests" FailedWorkspaceChangeRequest
    } deriving (Eq, Read, Show, Monoid, Semigroup)

instance GHC.Exts.IsList RebuildWorkspacesResponse where
    type Item RebuildWorkspacesResponse = FailedWorkspaceChangeRequest

    fromList = RebuildWorkspacesResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _rwrFailedRequests

-- | 'RebuildWorkspacesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rwrFailedRequests' @::@ ['FailedWorkspaceChangeRequest']
--
rebuildWorkspacesResponse :: RebuildWorkspacesResponse
rebuildWorkspacesResponse = RebuildWorkspacesResponse
    { _rwrFailedRequests = mempty
    }

-- | An array of structures that represent any WorkSpaces that could not be
-- rebuilt.
rwrFailedRequests :: Lens' RebuildWorkspacesResponse [FailedWorkspaceChangeRequest]
rwrFailedRequests =
    lens _rwrFailedRequests (\s a -> s { _rwrFailedRequests = a })
        . _List

instance ToPath RebuildWorkspaces where
    toPath = const "/"

instance ToQuery RebuildWorkspaces where
    toQuery = const mempty

instance ToHeaders RebuildWorkspaces

instance ToJSON RebuildWorkspaces where
    toJSON RebuildWorkspaces{..} = object
        [ "RebuildWorkspaceRequests" .= _rwRebuildWorkspaceRequests
        ]

instance AWSRequest RebuildWorkspaces where
    type Sv RebuildWorkspaces = WorkSpaces
    type Rs RebuildWorkspaces = RebuildWorkspacesResponse

    request  = post "RebuildWorkspaces"
    response = jsonResponse

instance FromJSON RebuildWorkspacesResponse where
    parseJSON = withObject "RebuildWorkspacesResponse" $ \o -> RebuildWorkspacesResponse
        <$> o .:? "FailedRequests" .!= mempty
