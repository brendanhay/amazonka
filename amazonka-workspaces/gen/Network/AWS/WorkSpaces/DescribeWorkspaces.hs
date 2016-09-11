{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.DescribeWorkspaces
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Obtains information about the specified WorkSpaces.
--
-- Only one of the filter parameters, such as 'BundleId', 'DirectoryId', or 'WorkspaceIds', can be specified at a time.
--
-- This operation supports pagination with the use of the 'NextToken' request and response parameters. If more results are available, the 'NextToken' response member contains a token that you pass in the next call to this operation to retrieve the next set of items.
module Network.AWS.WorkSpaces.DescribeWorkspaces
    (
    -- * Creating a Request
      describeWorkspaces
    , DescribeWorkspaces
    -- * Request Lenses
    , dwDirectoryId
    , dwWorkspaceIds
    , dwUserName
    , dwBundleId
    , dwNextToken
    , dwLimit

    -- * Destructuring the Response
    , describeWorkspacesResponse
    , DescribeWorkspacesResponse
    -- * Response Lenses
    , dwrsNextToken
    , dwrsWorkspaces
    , dwrsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.WorkSpaces.Types
import           Network.AWS.WorkSpaces.Types.Product

-- | Contains the inputs for the < DescribeWorkspaces> operation.
--
-- /See:/ 'describeWorkspaces' smart constructor.
data DescribeWorkspaces = DescribeWorkspaces'
    { _dwDirectoryId  :: !(Maybe Text)
    , _dwWorkspaceIds :: !(Maybe (List1 Text))
    , _dwUserName     :: !(Maybe Text)
    , _dwBundleId     :: !(Maybe Text)
    , _dwNextToken    :: !(Maybe Text)
    , _dwLimit        :: !(Maybe Nat)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeWorkspaces' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dwDirectoryId'
--
-- * 'dwWorkspaceIds'
--
-- * 'dwUserName'
--
-- * 'dwBundleId'
--
-- * 'dwNextToken'
--
-- * 'dwLimit'
describeWorkspaces
    :: DescribeWorkspaces
describeWorkspaces =
    DescribeWorkspaces'
    { _dwDirectoryId = Nothing
    , _dwWorkspaceIds = Nothing
    , _dwUserName = Nothing
    , _dwBundleId = Nothing
    , _dwNextToken = Nothing
    , _dwLimit = Nothing
    }

-- | Specifies the directory identifier to which to limit the WorkSpaces. Optionally, you can specify a specific directory user with the 'UserName' parameter. This parameter cannot be combined with any other filter parameter.
dwDirectoryId :: Lens' DescribeWorkspaces (Maybe Text)
dwDirectoryId = lens _dwDirectoryId (\ s a -> s{_dwDirectoryId = a});

-- | An array of strings that contain the identifiers of the WorkSpaces for which to retrieve information. This parameter cannot be combined with any other filter parameter.
--
-- Because the < CreateWorkspaces> operation is asynchronous, the identifier it returns is not immediately available. If you immediately call < DescribeWorkspaces> with this identifier, no information is returned.
dwWorkspaceIds :: Lens' DescribeWorkspaces (Maybe (NonEmpty Text))
dwWorkspaceIds = lens _dwWorkspaceIds (\ s a -> s{_dwWorkspaceIds = a}) . mapping _List1;

-- | Used with the 'DirectoryId' parameter to specify the directory user for whom to obtain the WorkSpace.
dwUserName :: Lens' DescribeWorkspaces (Maybe Text)
dwUserName = lens _dwUserName (\ s a -> s{_dwUserName = a});

-- | The identifier of a bundle to obtain the WorkSpaces for. All WorkSpaces that are created from this bundle will be retrieved. This parameter cannot be combined with any other filter parameter.
dwBundleId :: Lens' DescribeWorkspaces (Maybe Text)
dwBundleId = lens _dwBundleId (\ s a -> s{_dwBundleId = a});

-- | The 'NextToken' value from a previous call to this operation. Pass null if this is the first call.
dwNextToken :: Lens' DescribeWorkspaces (Maybe Text)
dwNextToken = lens _dwNextToken (\ s a -> s{_dwNextToken = a});

-- | The maximum number of items to return.
dwLimit :: Lens' DescribeWorkspaces (Maybe Natural)
dwLimit = lens _dwLimit (\ s a -> s{_dwLimit = a}) . mapping _Nat;

instance AWSRequest DescribeWorkspaces where
        type Rs DescribeWorkspaces =
             DescribeWorkspacesResponse
        request = postJSON workSpaces
        response
          = receiveJSON
              (\ s h x ->
                 DescribeWorkspacesResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "Workspaces" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeWorkspaces

instance NFData DescribeWorkspaces

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
              (catMaybes
                 [("DirectoryId" .=) <$> _dwDirectoryId,
                  ("WorkspaceIds" .=) <$> _dwWorkspaceIds,
                  ("UserName" .=) <$> _dwUserName,
                  ("BundleId" .=) <$> _dwBundleId,
                  ("NextToken" .=) <$> _dwNextToken,
                  ("Limit" .=) <$> _dwLimit])

instance ToPath DescribeWorkspaces where
        toPath = const "/"

instance ToQuery DescribeWorkspaces where
        toQuery = const mempty

-- | Contains the results for the < DescribeWorkspaces> operation.
--
-- /See:/ 'describeWorkspacesResponse' smart constructor.
data DescribeWorkspacesResponse = DescribeWorkspacesResponse'
    { _dwrsNextToken      :: !(Maybe Text)
    , _dwrsWorkspaces     :: !(Maybe [Workspace])
    , _dwrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeWorkspacesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dwrsNextToken'
--
-- * 'dwrsWorkspaces'
--
-- * 'dwrsResponseStatus'
describeWorkspacesResponse
    :: Int -- ^ 'dwrsResponseStatus'
    -> DescribeWorkspacesResponse
describeWorkspacesResponse pResponseStatus_ =
    DescribeWorkspacesResponse'
    { _dwrsNextToken = Nothing
    , _dwrsWorkspaces = Nothing
    , _dwrsResponseStatus = pResponseStatus_
    }

-- | If not null, more results are available. Pass this value for the 'NextToken' parameter in a subsequent call to this operation to retrieve the next set of items. This token is valid for one day and must be used within that time frame.
dwrsNextToken :: Lens' DescribeWorkspacesResponse (Maybe Text)
dwrsNextToken = lens _dwrsNextToken (\ s a -> s{_dwrsNextToken = a});

-- | An array of structures that contain the information about the WorkSpaces.
--
-- Because the < CreateWorkspaces> operation is asynchronous, some of this information may be incomplete for a newly-created WorkSpace.
dwrsWorkspaces :: Lens' DescribeWorkspacesResponse [Workspace]
dwrsWorkspaces = lens _dwrsWorkspaces (\ s a -> s{_dwrsWorkspaces = a}) . _Default . _Coerce;

-- | The response status code.
dwrsResponseStatus :: Lens' DescribeWorkspacesResponse Int
dwrsResponseStatus = lens _dwrsResponseStatus (\ s a -> s{_dwrsResponseStatus = a});

instance NFData DescribeWorkspacesResponse
