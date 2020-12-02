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
-- Module      : Network.AWS.WorkSpaces.DescribeWorkspaceBundles
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the available WorkSpace bundles.
--
--
-- You can filter the results using either bundle ID or owner, but not both.
--
--
-- This operation returns paginated results.
module Network.AWS.WorkSpaces.DescribeWorkspaceBundles
    (
    -- * Creating a Request
      describeWorkspaceBundles
    , DescribeWorkspaceBundles
    -- * Request Lenses
    , dwbBundleIds
    , dwbOwner
    , dwbNextToken

    -- * Destructuring the Response
    , describeWorkspaceBundlesResponse
    , DescribeWorkspaceBundlesResponse
    -- * Response Lenses
    , dwbrsBundles
    , dwbrsNextToken
    , dwbrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkSpaces.Types
import Network.AWS.WorkSpaces.Types.Product

-- | /See:/ 'describeWorkspaceBundles' smart constructor.
data DescribeWorkspaceBundles = DescribeWorkspaceBundles'
  { _dwbBundleIds :: !(Maybe (List1 Text))
  , _dwbOwner     :: !(Maybe Text)
  , _dwbNextToken :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeWorkspaceBundles' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dwbBundleIds' - The IDs of the bundles. This parameter cannot be combined with any other filter.
--
-- * 'dwbOwner' - The owner of the bundles. This parameter cannot be combined with any other filter. Specify @AMAZON@ to describe the bundles provided by AWS or null to describe the bundles that belong to your account.
--
-- * 'dwbNextToken' - The token for the next set of results. (You received this token from a previous call.)
describeWorkspaceBundles
    :: DescribeWorkspaceBundles
describeWorkspaceBundles =
  DescribeWorkspaceBundles'
    {_dwbBundleIds = Nothing, _dwbOwner = Nothing, _dwbNextToken = Nothing}


-- | The IDs of the bundles. This parameter cannot be combined with any other filter.
dwbBundleIds :: Lens' DescribeWorkspaceBundles (Maybe (NonEmpty Text))
dwbBundleIds = lens _dwbBundleIds (\ s a -> s{_dwbBundleIds = a}) . mapping _List1

-- | The owner of the bundles. This parameter cannot be combined with any other filter. Specify @AMAZON@ to describe the bundles provided by AWS or null to describe the bundles that belong to your account.
dwbOwner :: Lens' DescribeWorkspaceBundles (Maybe Text)
dwbOwner = lens _dwbOwner (\ s a -> s{_dwbOwner = a})

-- | The token for the next set of results. (You received this token from a previous call.)
dwbNextToken :: Lens' DescribeWorkspaceBundles (Maybe Text)
dwbNextToken = lens _dwbNextToken (\ s a -> s{_dwbNextToken = a})

instance AWSPager DescribeWorkspaceBundles where
        page rq rs
          | stop (rs ^. dwbrsNextToken) = Nothing
          | stop (rs ^. dwbrsBundles) = Nothing
          | otherwise =
            Just $ rq & dwbNextToken .~ rs ^. dwbrsNextToken

instance AWSRequest DescribeWorkspaceBundles where
        type Rs DescribeWorkspaceBundles =
             DescribeWorkspaceBundlesResponse
        request = postJSON workSpaces
        response
          = receiveJSON
              (\ s h x ->
                 DescribeWorkspaceBundlesResponse' <$>
                   (x .?> "Bundles" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeWorkspaceBundles where

instance NFData DescribeWorkspaceBundles where

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
              (catMaybes
                 [("BundleIds" .=) <$> _dwbBundleIds,
                  ("Owner" .=) <$> _dwbOwner,
                  ("NextToken" .=) <$> _dwbNextToken])

instance ToPath DescribeWorkspaceBundles where
        toPath = const "/"

instance ToQuery DescribeWorkspaceBundles where
        toQuery = const mempty

-- | /See:/ 'describeWorkspaceBundlesResponse' smart constructor.
data DescribeWorkspaceBundlesResponse = DescribeWorkspaceBundlesResponse'
  { _dwbrsBundles        :: !(Maybe [WorkspaceBundle])
  , _dwbrsNextToken      :: !(Maybe Text)
  , _dwbrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeWorkspaceBundlesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dwbrsBundles' - Information about the bundles.
--
-- * 'dwbrsNextToken' - The token to use to retrieve the next set of results, or null if there are no more results available. This token is valid for one day and must be used within that time frame.
--
-- * 'dwbrsResponseStatus' - -- | The response status code.
describeWorkspaceBundlesResponse
    :: Int -- ^ 'dwbrsResponseStatus'
    -> DescribeWorkspaceBundlesResponse
describeWorkspaceBundlesResponse pResponseStatus_ =
  DescribeWorkspaceBundlesResponse'
    { _dwbrsBundles = Nothing
    , _dwbrsNextToken = Nothing
    , _dwbrsResponseStatus = pResponseStatus_
    }


-- | Information about the bundles.
dwbrsBundles :: Lens' DescribeWorkspaceBundlesResponse [WorkspaceBundle]
dwbrsBundles = lens _dwbrsBundles (\ s a -> s{_dwbrsBundles = a}) . _Default . _Coerce

-- | The token to use to retrieve the next set of results, or null if there are no more results available. This token is valid for one day and must be used within that time frame.
dwbrsNextToken :: Lens' DescribeWorkspaceBundlesResponse (Maybe Text)
dwbrsNextToken = lens _dwbrsNextToken (\ s a -> s{_dwbrsNextToken = a})

-- | -- | The response status code.
dwbrsResponseStatus :: Lens' DescribeWorkspaceBundlesResponse Int
dwbrsResponseStatus = lens _dwbrsResponseStatus (\ s a -> s{_dwbrsResponseStatus = a})

instance NFData DescribeWorkspaceBundlesResponse
         where
