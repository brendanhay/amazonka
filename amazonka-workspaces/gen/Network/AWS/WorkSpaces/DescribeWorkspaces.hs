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

-- Module      : Network.AWS.WorkSpaces.DescribeWorkspaces
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

-- | Obtains information about the specified WorkSpaces.
--
-- Only one of the filter parameters, such as 'BundleId', 'DirectoryId', or 'WorkspaceIds', can be specified at a time.
--
-- This operation supports pagination with the use of the 'NextToken' request and
-- response parameters. If more results are available, the 'NextToken' response
-- member contains a token that you pass in the next call to this operation to
-- retrieve the next set of items.
--
-- <http://docs.aws.amazon.com/workspaces/latest/devguide/API_DescribeWorkspaces.html>
module Network.AWS.WorkSpaces.DescribeWorkspaces
    (
    -- * Request
      DescribeWorkspaces
    -- ** Request constructor
    , describeWorkspaces
    -- ** Request lenses
    , dwBundleId
    , dwDirectoryId
    , dwLimit
    , dwNextToken
    , dwUserName
    , dwWorkspaceIds

    -- * Response
    , DescribeWorkspacesResponse
    -- ** Response constructor
    , describeWorkspacesResponse
    -- ** Response lenses
    , dwrNextToken
    , dwrWorkspaces
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.WorkSpaces.Types
import qualified GHC.Exts

data DescribeWorkspaces = DescribeWorkspaces
    { _dwBundleId     :: Maybe Text
    , _dwDirectoryId  :: Maybe Text
    , _dwLimit        :: Maybe Nat
    , _dwNextToken    :: Maybe Text
    , _dwUserName     :: Maybe Text
    , _dwWorkspaceIds :: List1 "WorkspaceIds" Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DescribeWorkspaces' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dwBundleId' @::@ 'Maybe' 'Text'
--
-- * 'dwDirectoryId' @::@ 'Maybe' 'Text'
--
-- * 'dwLimit' @::@ 'Maybe' 'Natural'
--
-- * 'dwNextToken' @::@ 'Maybe' 'Text'
--
-- * 'dwUserName' @::@ 'Maybe' 'Text'
--
-- * 'dwWorkspaceIds' @::@ 'NonEmpty' 'Text'
--
describeWorkspaces :: NonEmpty Text -- ^ 'dwWorkspaceIds'
                   -> DescribeWorkspaces
describeWorkspaces p1 = DescribeWorkspaces
    { _dwWorkspaceIds = withIso _List1 (const id) p1
    , _dwDirectoryId  = Nothing
    , _dwUserName     = Nothing
    , _dwBundleId     = Nothing
    , _dwLimit        = Nothing
    , _dwNextToken    = Nothing
    }

-- | The identifier of a bundle to obtain the WorkSpaces for. All WorkSpaces that
-- are created from this bundle will be retrieved. This parameter cannot be
-- combined with any other filter parameter.
dwBundleId :: Lens' DescribeWorkspaces (Maybe Text)
dwBundleId = lens _dwBundleId (\s a -> s { _dwBundleId = a })

-- | Specifies the directory identifier to which to limit the WorkSpaces.
-- Optionally, you can specify a specific directory user with the 'UserName'
-- parameter. This parameter cannot be combined with any other filter parameter.
dwDirectoryId :: Lens' DescribeWorkspaces (Maybe Text)
dwDirectoryId = lens _dwDirectoryId (\s a -> s { _dwDirectoryId = a })

-- | The maximum number of items to return.
dwLimit :: Lens' DescribeWorkspaces (Maybe Natural)
dwLimit = lens _dwLimit (\s a -> s { _dwLimit = a }) . mapping _Nat

-- | The 'NextToken' value from a previous call to this operation. Pass null if this
-- is the first call.
dwNextToken :: Lens' DescribeWorkspaces (Maybe Text)
dwNextToken = lens _dwNextToken (\s a -> s { _dwNextToken = a })

-- | Used with the 'DirectoryId' parameter to specify the directory user for which
-- to obtain the WorkSpace.
dwUserName :: Lens' DescribeWorkspaces (Maybe Text)
dwUserName = lens _dwUserName (\s a -> s { _dwUserName = a })

-- | An array of strings that contain the identifiers of the WorkSpaces for which
-- to retrieve information. This parameter cannot be combined with any other
-- filter parameter.
--
-- Because the 'CreateWorkspaces' operation is asynchronous, the identifier
-- returned by 'CreateWorkspaces' is not immediately available. If you immediately
-- call 'DescribeWorkspaces' with this identifier, no information will be returned.
dwWorkspaceIds :: Lens' DescribeWorkspaces (NonEmpty Text)
dwWorkspaceIds = lens _dwWorkspaceIds (\s a -> s { _dwWorkspaceIds = a }) . _List1

data DescribeWorkspacesResponse = DescribeWorkspacesResponse
    { _dwrNextToken  :: Maybe Text
    , _dwrWorkspaces :: List "Workspaces" Workspace
    } deriving (Eq, Read, Show)

-- | 'DescribeWorkspacesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dwrNextToken' @::@ 'Maybe' 'Text'
--
-- * 'dwrWorkspaces' @::@ ['Workspace']
--
describeWorkspacesResponse :: DescribeWorkspacesResponse
describeWorkspacesResponse = DescribeWorkspacesResponse
    { _dwrWorkspaces = mempty
    , _dwrNextToken  = Nothing
    }

-- | If not null, more results are available. Pass this value for the 'NextToken'
-- parameter in a subsequent call to this operation to retrieve the next set of
-- items. This token is valid for one day and must be used within that timeframe.
dwrNextToken :: Lens' DescribeWorkspacesResponse (Maybe Text)
dwrNextToken = lens _dwrNextToken (\s a -> s { _dwrNextToken = a })

-- | An array of structures that contain the information about the WorkSpaces.
--
-- Because the 'CreateWorkspaces' operation is asynchronous, some of this
-- information may be incomplete for a newly-created WorkSpace.
dwrWorkspaces :: Lens' DescribeWorkspacesResponse [Workspace]
dwrWorkspaces = lens _dwrWorkspaces (\s a -> s { _dwrWorkspaces = a }) . _List

instance ToPath DescribeWorkspaces where
    toPath = const "/"

instance ToQuery DescribeWorkspaces where
    toQuery = const mempty

instance ToHeaders DescribeWorkspaces

instance ToJSON DescribeWorkspaces where
    toJSON DescribeWorkspaces{..} = object
        [ "WorkspaceIds" .= _dwWorkspaceIds
        , "DirectoryId"  .= _dwDirectoryId
        , "UserName"     .= _dwUserName
        , "BundleId"     .= _dwBundleId
        , "Limit"        .= _dwLimit
        , "NextToken"    .= _dwNextToken
        ]

instance AWSRequest DescribeWorkspaces where
    type Sv DescribeWorkspaces = WorkSpaces
    type Rs DescribeWorkspaces = DescribeWorkspacesResponse

    request  = post "DescribeWorkspaces"
    response = jsonResponse

instance FromJSON DescribeWorkspacesResponse where
    parseJSON = withObject "DescribeWorkspacesResponse" $ \o -> DescribeWorkspacesResponse
        <$> o .:? "NextToken"
        <*> o .:? "Workspaces" .!= mempty
