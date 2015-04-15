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

-- Module      : Network.AWS.WorkSpaces.DescribeWorkspaceDirectories
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

-- | Retrieves information about the AWS Directory Service directories in the
-- region that are registered with Amazon WorkSpaces and are available to your
-- account.
--
-- This operation supports pagination with the use of the 'NextToken' request and
-- response parameters. If more results are available, the 'NextToken' response
-- member contains a token that you pass in the next call to this operation to
-- retrieve the next set of items.
--
-- <http://docs.aws.amazon.com/workspaces/latest/devguide/API_DescribeWorkspaceDirectories.html>
module Network.AWS.WorkSpaces.DescribeWorkspaceDirectories
    (
    -- * Request
      DescribeWorkspaceDirectories
    -- ** Request constructor
    , describeWorkspaceDirectories
    -- ** Request lenses
    , dwdDirectoryIds
    , dwdNextToken

    -- * Response
    , DescribeWorkspaceDirectoriesResponse
    -- ** Response constructor
    , describeWorkspaceDirectoriesResponse
    -- ** Response lenses
    , dwdrDirectories
    , dwdrNextToken
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.WorkSpaces.Types
import qualified GHC.Exts

data DescribeWorkspaceDirectories = DescribeWorkspaceDirectories
    { _dwdDirectoryIds :: List1 "DirectoryIds" Text
    , _dwdNextToken    :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DescribeWorkspaceDirectories' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dwdDirectoryIds' @::@ 'NonEmpty' 'Text'
--
-- * 'dwdNextToken' @::@ 'Maybe' 'Text'
--
describeWorkspaceDirectories :: NonEmpty Text -- ^ 'dwdDirectoryIds'
                             -> DescribeWorkspaceDirectories
describeWorkspaceDirectories p1 = DescribeWorkspaceDirectories
    { _dwdDirectoryIds = withIso _List1 (const id) p1
    , _dwdNextToken    = Nothing
    }

-- | An array of strings that contains the directory identifiers to retrieve
-- information for. If this member is null, all directories are retrieved.
dwdDirectoryIds :: Lens' DescribeWorkspaceDirectories (NonEmpty Text)
dwdDirectoryIds = lens _dwdDirectoryIds (\s a -> s { _dwdDirectoryIds = a }) . _List1

-- | The 'NextToken' value from a previous call to this operation. Pass null if this
-- is the first call.
dwdNextToken :: Lens' DescribeWorkspaceDirectories (Maybe Text)
dwdNextToken = lens _dwdNextToken (\s a -> s { _dwdNextToken = a })

data DescribeWorkspaceDirectoriesResponse = DescribeWorkspaceDirectoriesResponse
    { _dwdrDirectories :: List "Directories" WorkspaceDirectory
    , _dwdrNextToken   :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'DescribeWorkspaceDirectoriesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dwdrDirectories' @::@ ['WorkspaceDirectory']
--
-- * 'dwdrNextToken' @::@ 'Maybe' 'Text'
--
describeWorkspaceDirectoriesResponse :: DescribeWorkspaceDirectoriesResponse
describeWorkspaceDirectoriesResponse = DescribeWorkspaceDirectoriesResponse
    { _dwdrDirectories = mempty
    , _dwdrNextToken   = Nothing
    }

-- | An array of structures that contain information about the directories.
dwdrDirectories :: Lens' DescribeWorkspaceDirectoriesResponse [WorkspaceDirectory]
dwdrDirectories = lens _dwdrDirectories (\s a -> s { _dwdrDirectories = a }) . _List

-- | If not null, more results are available. Pass this value for the 'NextToken'
-- parameter in a subsequent call to this operation to retrieve the next set of
-- items. This token is valid for one day and must be used within that timeframe.
dwdrNextToken :: Lens' DescribeWorkspaceDirectoriesResponse (Maybe Text)
dwdrNextToken = lens _dwdrNextToken (\s a -> s { _dwdrNextToken = a })

instance ToPath DescribeWorkspaceDirectories where
    toPath = const "/"

instance ToQuery DescribeWorkspaceDirectories where
    toQuery = const mempty

instance ToHeaders DescribeWorkspaceDirectories

instance ToJSON DescribeWorkspaceDirectories where
    toJSON DescribeWorkspaceDirectories{..} = object
        [ "DirectoryIds" .= _dwdDirectoryIds
        , "NextToken"    .= _dwdNextToken
        ]

instance AWSRequest DescribeWorkspaceDirectories where
    type Sv DescribeWorkspaceDirectories = WorkSpaces
    type Rs DescribeWorkspaceDirectories = DescribeWorkspaceDirectoriesResponse

    request  = post "DescribeWorkspaceDirectories"
    response = jsonResponse

instance FromJSON DescribeWorkspaceDirectoriesResponse where
    parseJSON = withObject "DescribeWorkspaceDirectoriesResponse" $ \o -> DescribeWorkspaceDirectoriesResponse
        <$> o .:? "Directories" .!= mempty
        <*> o .:? "NextToken"
