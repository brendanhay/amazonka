{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.DescribeWorkspaceDirectories
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the AWS Directory Service directories in the
-- region that are registered with Amazon WorkSpaces and are available to
-- your account.
--
-- This operation supports pagination with the use of the @NextToken@
-- request and response parameters. If more results are available, the
-- @NextToken@ response member contains a token that you pass in the next
-- call to this operation to retrieve the next set of items.
--
-- <http://docs.aws.amazon.com/workspaces/latest/devguide/API_DescribeWorkspaceDirectories.html>
module Network.AWS.WorkSpaces.DescribeWorkspaceDirectories
    (
    -- * Request
      DescribeWorkspaceDirectories
    -- ** Request constructor
    , describeWorkspaceDirectories
    -- ** Request lenses
    , dwdNextToken
    , dwdDirectoryIds

    -- * Response
    , DescribeWorkspaceDirectoriesResponse
    -- ** Response constructor
    , describeWorkspaceDirectoriesResponse
    -- ** Response lenses
    , dwdrsDirectories
    , dwdrsNextToken
    , dwdrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.WorkSpaces.Types

-- | Contains the inputs for the DescribeWorkspaceDirectories operation.
--
-- /See:/ 'describeWorkspaceDirectories' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dwdNextToken'
--
-- * 'dwdDirectoryIds'
data DescribeWorkspaceDirectories = DescribeWorkspaceDirectories'
    { _dwdNextToken    :: !(Maybe Text)
    , _dwdDirectoryIds :: !(Maybe (List1 Text))
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeWorkspaceDirectories' smart constructor.
describeWorkspaceDirectories :: DescribeWorkspaceDirectories
describeWorkspaceDirectories =
    DescribeWorkspaceDirectories'
    { _dwdNextToken = Nothing
    , _dwdDirectoryIds = Nothing
    }

-- | The @NextToken@ value from a previous call to this operation. Pass null
-- if this is the first call.
dwdNextToken :: Lens' DescribeWorkspaceDirectories (Maybe Text)
dwdNextToken = lens _dwdNextToken (\ s a -> s{_dwdNextToken = a});

-- | An array of strings that contains the directory identifiers to retrieve
-- information for. If this member is null, all directories are retrieved.
dwdDirectoryIds :: Lens' DescribeWorkspaceDirectories (Maybe (NonEmpty Text))
dwdDirectoryIds = lens _dwdDirectoryIds (\ s a -> s{_dwdDirectoryIds = a}) . mapping _List1;

instance AWSRequest DescribeWorkspaceDirectories
         where
        type Sv DescribeWorkspaceDirectories = WorkSpaces
        type Rs DescribeWorkspaceDirectories =
             DescribeWorkspaceDirectoriesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeWorkspaceDirectoriesResponse' <$>
                   (x .?> "Directories" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeWorkspaceDirectories where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("WorkspacesService.DescribeWorkspaceDirectories" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeWorkspaceDirectories where
        toJSON DescribeWorkspaceDirectories'{..}
          = object
              ["NextToken" .= _dwdNextToken,
               "DirectoryIds" .= _dwdDirectoryIds]

instance ToPath DescribeWorkspaceDirectories where
        toPath = const "/"

instance ToQuery DescribeWorkspaceDirectories where
        toQuery = const mempty

-- | Contains the results of the DescribeWorkspaceDirectories operation.
--
-- /See:/ 'describeWorkspaceDirectoriesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dwdrsDirectories'
--
-- * 'dwdrsNextToken'
--
-- * 'dwdrsStatus'
data DescribeWorkspaceDirectoriesResponse = DescribeWorkspaceDirectoriesResponse'
    { _dwdrsDirectories :: !(Maybe [WorkspaceDirectory])
    , _dwdrsNextToken   :: !(Maybe Text)
    , _dwdrsStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeWorkspaceDirectoriesResponse' smart constructor.
describeWorkspaceDirectoriesResponse :: Int -> DescribeWorkspaceDirectoriesResponse
describeWorkspaceDirectoriesResponse pStatus_ =
    DescribeWorkspaceDirectoriesResponse'
    { _dwdrsDirectories = Nothing
    , _dwdrsNextToken = Nothing
    , _dwdrsStatus = pStatus_
    }

-- | An array of structures that contain information about the directories.
dwdrsDirectories :: Lens' DescribeWorkspaceDirectoriesResponse [WorkspaceDirectory]
dwdrsDirectories = lens _dwdrsDirectories (\ s a -> s{_dwdrsDirectories = a}) . _Default;

-- | If not null, more results are available. Pass this value for the
-- @NextToken@ parameter in a subsequent call to this operation to retrieve
-- the next set of items. This token is valid for one day and must be used
-- within that timeframe.
dwdrsNextToken :: Lens' DescribeWorkspaceDirectoriesResponse (Maybe Text)
dwdrsNextToken = lens _dwdrsNextToken (\ s a -> s{_dwdrsNextToken = a});

-- | FIXME: Undocumented member.
dwdrsStatus :: Lens' DescribeWorkspaceDirectoriesResponse Int
dwdrsStatus = lens _dwdrsStatus (\ s a -> s{_dwdrsStatus = a});
