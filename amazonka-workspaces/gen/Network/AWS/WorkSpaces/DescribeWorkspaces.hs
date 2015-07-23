{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.DescribeWorkspaces
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Obtains information about the specified WorkSpaces.
--
-- Only one of the filter parameters, such as @BundleId@, @DirectoryId@, or
-- @WorkspaceIds@, can be specified at a time.
--
-- This operation supports pagination with the use of the @NextToken@
-- request and response parameters. If more results are available, the
-- @NextToken@ response member contains a token that you pass in the next
-- call to this operation to retrieve the next set of items.
--
-- <http://docs.aws.amazon.com/workspaces/latest/devguide/API_DescribeWorkspaces.html>
module Network.AWS.WorkSpaces.DescribeWorkspaces
    (
    -- * Request
      DescribeWorkspaces
    -- ** Request constructor
    , describeWorkspaces
    -- ** Request lenses
    , dwrqDirectoryId
    , dwrqWorkspaceIds
    , dwrqUserName
    , dwrqBundleId
    , dwrqNextToken
    , dwrqLimit

    -- * Response
    , DescribeWorkspacesResponse
    -- ** Response constructor
    , describeWorkspacesResponse
    -- ** Response lenses
    , dwrsNextToken
    , dwrsWorkspaces
    , dwrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.WorkSpaces.Types

-- | Contains the inputs for the DescribeWorkspaces operation.
--
-- /See:/ 'describeWorkspaces' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dwrqDirectoryId'
--
-- * 'dwrqWorkspaceIds'
--
-- * 'dwrqUserName'
--
-- * 'dwrqBundleId'
--
-- * 'dwrqNextToken'
--
-- * 'dwrqLimit'
data DescribeWorkspaces = DescribeWorkspaces'
    { _dwrqDirectoryId  :: !(Maybe Text)
    , _dwrqWorkspaceIds :: !(Maybe (List1 Text))
    , _dwrqUserName     :: !(Maybe Text)
    , _dwrqBundleId     :: !(Maybe Text)
    , _dwrqNextToken    :: !(Maybe Text)
    , _dwrqLimit        :: !(Maybe Nat)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeWorkspaces' smart constructor.
describeWorkspaces :: DescribeWorkspaces
describeWorkspaces =
    DescribeWorkspaces'
    { _dwrqDirectoryId = Nothing
    , _dwrqWorkspaceIds = Nothing
    , _dwrqUserName = Nothing
    , _dwrqBundleId = Nothing
    , _dwrqNextToken = Nothing
    , _dwrqLimit = Nothing
    }

-- | Specifies the directory identifier to which to limit the WorkSpaces.
-- Optionally, you can specify a specific directory user with the
-- @UserName@ parameter. This parameter cannot be combined with any other
-- filter parameter.
dwrqDirectoryId :: Lens' DescribeWorkspaces (Maybe Text)
dwrqDirectoryId = lens _dwrqDirectoryId (\ s a -> s{_dwrqDirectoryId = a});

-- | An array of strings that contain the identifiers of the WorkSpaces for
-- which to retrieve information. This parameter cannot be combined with
-- any other filter parameter.
--
-- Because the CreateWorkspaces operation is asynchronous, the identifier
-- returned by CreateWorkspaces is not immediately available. If you
-- immediately call DescribeWorkspaces with this identifier, no information
-- will be returned.
dwrqWorkspaceIds :: Lens' DescribeWorkspaces (Maybe (NonEmpty Text))
dwrqWorkspaceIds = lens _dwrqWorkspaceIds (\ s a -> s{_dwrqWorkspaceIds = a}) . mapping _List1;

-- | Used with the @DirectoryId@ parameter to specify the directory user for
-- which to obtain the WorkSpace.
dwrqUserName :: Lens' DescribeWorkspaces (Maybe Text)
dwrqUserName = lens _dwrqUserName (\ s a -> s{_dwrqUserName = a});

-- | The identifier of a bundle to obtain the WorkSpaces for. All WorkSpaces
-- that are created from this bundle will be retrieved. This parameter
-- cannot be combined with any other filter parameter.
dwrqBundleId :: Lens' DescribeWorkspaces (Maybe Text)
dwrqBundleId = lens _dwrqBundleId (\ s a -> s{_dwrqBundleId = a});

-- | The @NextToken@ value from a previous call to this operation. Pass null
-- if this is the first call.
dwrqNextToken :: Lens' DescribeWorkspaces (Maybe Text)
dwrqNextToken = lens _dwrqNextToken (\ s a -> s{_dwrqNextToken = a});

-- | The maximum number of items to return.
dwrqLimit :: Lens' DescribeWorkspaces (Maybe Natural)
dwrqLimit = lens _dwrqLimit (\ s a -> s{_dwrqLimit = a}) . mapping _Nat;

instance AWSRequest DescribeWorkspaces where
        type Sv DescribeWorkspaces = WorkSpaces
        type Rs DescribeWorkspaces =
             DescribeWorkspacesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeWorkspacesResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "Workspaces" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeWorkspaces where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("WorkspacesService.DescribeWorkspaces" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeWorkspaces where
        toJSON DescribeWorkspaces'{..}
          = object
              ["DirectoryId" .= _dwrqDirectoryId,
               "WorkspaceIds" .= _dwrqWorkspaceIds,
               "UserName" .= _dwrqUserName,
               "BundleId" .= _dwrqBundleId,
               "NextToken" .= _dwrqNextToken, "Limit" .= _dwrqLimit]

instance ToPath DescribeWorkspaces where
        toPath = const "/"

instance ToQuery DescribeWorkspaces where
        toQuery = const mempty

-- | Contains the results for the DescribeWorkspaces operation.
--
-- /See:/ 'describeWorkspacesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dwrsNextToken'
--
-- * 'dwrsWorkspaces'
--
-- * 'dwrsStatus'
data DescribeWorkspacesResponse = DescribeWorkspacesResponse'
    { _dwrsNextToken  :: !(Maybe Text)
    , _dwrsWorkspaces :: !(Maybe [Workspace])
    , _dwrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeWorkspacesResponse' smart constructor.
describeWorkspacesResponse :: Int -> DescribeWorkspacesResponse
describeWorkspacesResponse pStatus_ =
    DescribeWorkspacesResponse'
    { _dwrsNextToken = Nothing
    , _dwrsWorkspaces = Nothing
    , _dwrsStatus = pStatus_
    }

-- | If not null, more results are available. Pass this value for the
-- @NextToken@ parameter in a subsequent call to this operation to retrieve
-- the next set of items. This token is valid for one day and must be used
-- within that timeframe.
dwrsNextToken :: Lens' DescribeWorkspacesResponse (Maybe Text)
dwrsNextToken = lens _dwrsNextToken (\ s a -> s{_dwrsNextToken = a});

-- | An array of structures that contain the information about the
-- WorkSpaces.
--
-- Because the CreateWorkspaces operation is asynchronous, some of this
-- information may be incomplete for a newly-created WorkSpace.
dwrsWorkspaces :: Lens' DescribeWorkspacesResponse [Workspace]
dwrsWorkspaces = lens _dwrsWorkspaces (\ s a -> s{_dwrsWorkspaces = a}) . _Default;

-- | FIXME: Undocumented member.
dwrsStatus :: Lens' DescribeWorkspacesResponse Int
dwrsStatus = lens _dwrsStatus (\ s a -> s{_dwrsStatus = a});
