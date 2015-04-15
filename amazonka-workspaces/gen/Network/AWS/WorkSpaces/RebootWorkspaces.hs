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

-- Module      : Network.AWS.WorkSpaces.RebootWorkspaces
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

-- | Reboots the specified WorkSpaces.
--
-- To be able to reboot a WorkSpace, the WorkSpace must have a State of 'AVAILABLE', 'IMPAIRED', or 'INOPERABLE'.
--
-- This operation is asynchronous and will return before the WorkSpaces have
-- rebooted.
--
--
--
-- <http://docs.aws.amazon.com/workspaces/latest/devguide/API_RebootWorkspaces.html>
module Network.AWS.WorkSpaces.RebootWorkspaces
    (
    -- * Request
      RebootWorkspaces
    -- ** Request constructor
    , rebootWorkspaces
    -- ** Request lenses
    , rwRebootWorkspaceRequests

    -- * Response
    , RebootWorkspacesResponse
    -- ** Response constructor
    , rebootWorkspacesResponse
    -- ** Response lenses
    , rwr1FailedRequests
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.WorkSpaces.Types
import qualified GHC.Exts

newtype RebootWorkspaces = RebootWorkspaces
    { _rwRebootWorkspaceRequests :: List1 "RebootWorkspaceRequests" RebootRequest
    } deriving (Eq, Read, Show, Semigroup)

-- | 'RebootWorkspaces' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rwRebootWorkspaceRequests' @::@ 'NonEmpty' 'RebootRequest'
--
rebootWorkspaces :: NonEmpty RebootRequest -- ^ 'rwRebootWorkspaceRequests'
                 -> RebootWorkspaces
rebootWorkspaces p1 = RebootWorkspaces
    { _rwRebootWorkspaceRequests = withIso _List1 (const id) p1
    }

-- | An array of structures that specify the WorkSpaces to reboot.
rwRebootWorkspaceRequests :: Lens' RebootWorkspaces (NonEmpty RebootRequest)
rwRebootWorkspaceRequests =
    lens _rwRebootWorkspaceRequests
        (\s a -> s { _rwRebootWorkspaceRequests = a })
            . _List1

newtype RebootWorkspacesResponse = RebootWorkspacesResponse
    { _rwr1FailedRequests :: List "FailedRequests" FailedWorkspaceChangeRequest
    } deriving (Eq, Read, Show, Monoid, Semigroup)

instance GHC.Exts.IsList RebootWorkspacesResponse where
    type Item RebootWorkspacesResponse = FailedWorkspaceChangeRequest

    fromList = RebootWorkspacesResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _rwr1FailedRequests

-- | 'RebootWorkspacesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rwr1FailedRequests' @::@ ['FailedWorkspaceChangeRequest']
--
rebootWorkspacesResponse :: RebootWorkspacesResponse
rebootWorkspacesResponse = RebootWorkspacesResponse
    { _rwr1FailedRequests = mempty
    }

-- | An array of structures that represent any WorkSpaces that could not be
-- rebooted.
rwr1FailedRequests :: Lens' RebootWorkspacesResponse [FailedWorkspaceChangeRequest]
rwr1FailedRequests =
    lens _rwr1FailedRequests (\s a -> s { _rwr1FailedRequests = a })
        . _List

instance ToPath RebootWorkspaces where
    toPath = const "/"

instance ToQuery RebootWorkspaces where
    toQuery = const mempty

instance ToHeaders RebootWorkspaces

instance ToJSON RebootWorkspaces where
    toJSON RebootWorkspaces{..} = object
        [ "RebootWorkspaceRequests" .= _rwRebootWorkspaceRequests
        ]

instance AWSRequest RebootWorkspaces where
    type Sv RebootWorkspaces = WorkSpaces
    type Rs RebootWorkspaces = RebootWorkspacesResponse

    request  = post "RebootWorkspaces"
    response = jsonResponse

instance FromJSON RebootWorkspacesResponse where
    parseJSON = withObject "RebootWorkspacesResponse" $ \o -> RebootWorkspacesResponse
        <$> o .:? "FailedRequests" .!= mempty
