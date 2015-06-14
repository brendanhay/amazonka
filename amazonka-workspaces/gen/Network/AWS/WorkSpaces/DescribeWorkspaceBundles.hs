{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.WorkSpaces.DescribeWorkspaceBundles
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Obtains information about the WorkSpace bundles that are available to
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
    , dwbOwner
    , dwbBundleIds
    , dwbNextToken

    -- * Response
    , DescribeWorkspaceBundlesResponse
    -- ** Response constructor
    , describeWorkspaceBundlesResponse
    -- ** Response lenses
    , dwbrBundles
    , dwbrNextToken
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'describeWorkspaceBundles' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dwbOwner'
--
-- * 'dwbBundleIds'
--
-- * 'dwbNextToken'
data DescribeWorkspaceBundles = DescribeWorkspaceBundles'{_dwbOwner :: Maybe Text, _dwbBundleIds :: List1 Text, _dwbNextToken :: Text} deriving (Eq, Read, Show)

-- | 'DescribeWorkspaceBundles' smart constructor.
describeWorkspaceBundles :: NonEmpty Text -> Text -> DescribeWorkspaceBundles
describeWorkspaceBundles pBundleIds pNextToken = DescribeWorkspaceBundles'{_dwbOwner = Nothing, _dwbBundleIds = _List1 # pBundleIds, _dwbNextToken = pNextToken};

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

-- | An array of strings that contains the identifiers of the bundles to
-- retrieve. This parameter cannot be combined with any other filter
-- parameter.
dwbBundleIds :: Lens' DescribeWorkspaceBundles (NonEmpty Text)
dwbBundleIds = lens _dwbBundleIds (\ s a -> s{_dwbBundleIds = a}) . _List1;

-- | The @NextToken@ value from a previous call to this operation. Pass null
-- if this is the first call.
dwbNextToken :: Lens' DescribeWorkspaceBundles Text
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
                   x .?> "Bundles" .!@ mempty <*> x .:> "NextToken")

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
              ["Owner" .= _dwbOwner, "BundleIds" .= _dwbBundleIds,
               "NextToken" .= _dwbNextToken]

instance ToPath DescribeWorkspaceBundles where
        toPath = const "/"

instance ToQuery DescribeWorkspaceBundles where
        toQuery = const mempty

-- | /See:/ 'describeWorkspaceBundlesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dwbrBundles'
--
-- * 'dwbrNextToken'
data DescribeWorkspaceBundlesResponse = DescribeWorkspaceBundlesResponse'{_dwbrBundles :: [WorkspaceBundle], _dwbrNextToken :: Text} deriving (Eq, Read, Show)

-- | 'DescribeWorkspaceBundlesResponse' smart constructor.
describeWorkspaceBundlesResponse :: Text -> DescribeWorkspaceBundlesResponse
describeWorkspaceBundlesResponse pNextToken = DescribeWorkspaceBundlesResponse'{_dwbrBundles = mempty, _dwbrNextToken = pNextToken};

-- | An array of structures that contain information about the bundles.
dwbrBundles :: Lens' DescribeWorkspaceBundlesResponse [WorkspaceBundle]
dwbrBundles = lens _dwbrBundles (\ s a -> s{_dwbrBundles = a});

-- | If not null, more results are available. Pass this value for the
-- @NextToken@ parameter in a subsequent call to this operation to retrieve
-- the next set of items. This token is valid for one day and must be used
-- within that timeframe.
dwbrNextToken :: Lens' DescribeWorkspaceBundlesResponse Text
dwbrNextToken = lens _dwbrNextToken (\ s a -> s{_dwbrNextToken = a});
