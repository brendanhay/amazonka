{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.DescribeWorkspaceBundles
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Obtains information about the WorkSpace bundles that are available to
-- your account in the specified region.
--
-- You can filter the results with either the @BundleIds@ parameter, or the
-- @Owner@ parameter, but not both.
--
-- This operation supports pagination with the use of the @NextToken@
-- request and response parameters. If more results are available, the
-- @NextToken@ response member contains a token that you pass in the next
-- call to this operation to retrieve the next set of items.
--
-- <http://docs.aws.amazon.com/workspaces/latest/devguide/API_DescribeWorkspaceBundles.html>
module Network.AWS.WorkSpaces.DescribeWorkspaceBundles
    (
    -- * Request
      DescribeWorkspaceBundles
    -- ** Request constructor
    , describeWorkspaceBundles
    -- ** Request lenses
    , dwbBundleIds
    , dwbOwner
    , dwbNextToken

    -- * Response
    , DescribeWorkspaceBundlesResponse
    -- ** Response constructor
    , describeWorkspaceBundlesResponse
    -- ** Response lenses
    , dwbrsBundles
    , dwbrsNextToken
    , dwbrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.WorkSpaces.Types

-- | Contains the inputs for the DescribeWorkspaceBundles operation.
--
-- /See:/ 'describeWorkspaceBundles' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dwbBundleIds'
--
-- * 'dwbOwner'
--
-- * 'dwbNextToken'
data DescribeWorkspaceBundles = DescribeWorkspaceBundles'
    { _dwbBundleIds :: !(Maybe (List1 Text))
    , _dwbOwner     :: !(Maybe Text)
    , _dwbNextToken :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeWorkspaceBundles' smart constructor.
describeWorkspaceBundles :: DescribeWorkspaceBundles
describeWorkspaceBundles =
    DescribeWorkspaceBundles'
    { _dwbBundleIds = Nothing
    , _dwbOwner = Nothing
    , _dwbNextToken = Nothing
    }

-- | An array of strings that contains the identifiers of the bundles to
-- retrieve. This parameter cannot be combined with any other filter
-- parameter.
dwbBundleIds :: Lens' DescribeWorkspaceBundles (Maybe (NonEmpty Text))
dwbBundleIds = lens _dwbBundleIds (\ s a -> s{_dwbBundleIds = a}) . mapping _List1;

-- | The owner of the bundles to retrieve. This parameter cannot be combined
-- with any other filter parameter.
--
-- This contains one of the following values:
--
-- -   null - Retrieves the bundles that belong to the account making the
--     call.
-- -   @AMAZON@ - Retrieves the bundles that are provided by AWS.
dwbOwner :: Lens' DescribeWorkspaceBundles (Maybe Text)
dwbOwner = lens _dwbOwner (\ s a -> s{_dwbOwner = a});

-- | The @NextToken@ value from a previous call to this operation. Pass null
-- if this is the first call.
dwbNextToken :: Lens' DescribeWorkspaceBundles (Maybe Text)
dwbNextToken = lens _dwbNextToken (\ s a -> s{_dwbNextToken = a});

instance AWSRequest DescribeWorkspaceBundles where
        type Sv DescribeWorkspaceBundles = WorkSpaces
        type Rs DescribeWorkspaceBundles =
             DescribeWorkspaceBundlesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeWorkspaceBundlesResponse' <$>
                   (x .?> "Bundles" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeWorkspaceBundles where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("WorkspacesService.DescribeWorkspaceBundles" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeWorkspaceBundles where
        toJSON DescribeWorkspaceBundles'{..}
          = object
              ["BundleIds" .= _dwbBundleIds, "Owner" .= _dwbOwner,
               "NextToken" .= _dwbNextToken]

instance ToPath DescribeWorkspaceBundles where
        toPath = const "/"

instance ToQuery DescribeWorkspaceBundles where
        toQuery = const mempty

-- | Contains the results of the DescribeWorkspaceBundles operation.
--
-- /See:/ 'describeWorkspaceBundlesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dwbrsBundles'
--
-- * 'dwbrsNextToken'
--
-- * 'dwbrsStatus'
data DescribeWorkspaceBundlesResponse = DescribeWorkspaceBundlesResponse'
    { _dwbrsBundles   :: !(Maybe [WorkspaceBundle])
    , _dwbrsNextToken :: !(Maybe Text)
    , _dwbrsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeWorkspaceBundlesResponse' smart constructor.
describeWorkspaceBundlesResponse :: Int -> DescribeWorkspaceBundlesResponse
describeWorkspaceBundlesResponse pStatus_ =
    DescribeWorkspaceBundlesResponse'
    { _dwbrsBundles = Nothing
    , _dwbrsNextToken = Nothing
    , _dwbrsStatus = pStatus_
    }

-- | An array of structures that contain information about the bundles.
dwbrsBundles :: Lens' DescribeWorkspaceBundlesResponse [WorkspaceBundle]
dwbrsBundles = lens _dwbrsBundles (\ s a -> s{_dwbrsBundles = a}) . _Default;

-- | If not null, more results are available. Pass this value for the
-- @NextToken@ parameter in a subsequent call to this operation to retrieve
-- the next set of items. This token is valid for one day and must be used
-- within that timeframe.
dwbrsNextToken :: Lens' DescribeWorkspaceBundlesResponse (Maybe Text)
dwbrsNextToken = lens _dwbrsNextToken (\ s a -> s{_dwbrsNextToken = a});

-- | FIXME: Undocumented member.
dwbrsStatus :: Lens' DescribeWorkspaceBundlesResponse Int
dwbrsStatus = lens _dwbrsStatus (\ s a -> s{_dwbrsStatus = a});
