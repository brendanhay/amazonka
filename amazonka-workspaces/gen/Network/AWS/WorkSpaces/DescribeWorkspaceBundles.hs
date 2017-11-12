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
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Obtains information about the WorkSpace bundles that are available to your account in the specified region.
--
--
-- You can filter the results with either the @BundleIds@ parameter, or the @Owner@ parameter, but not both.
--
-- This operation supports pagination with the use of the @NextToken@ request and response parameters. If more results are available, the @NextToken@ response member contains a token that you pass in the next call to this operation to retrieve the next set of items.
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

-- | Contains the inputs for the 'DescribeWorkspaceBundles' operation.
--
--
--
-- /See:/ 'describeWorkspaceBundles' smart constructor.
data DescribeWorkspaceBundles = DescribeWorkspaceBundles'
  { _dwbBundleIds :: !(Maybe (List1 Text))
  , _dwbOwner     :: !(Maybe Text)
  , _dwbNextToken :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeWorkspaceBundles' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dwbBundleIds' - An array of strings that contains the identifiers of the bundles to retrieve. This parameter cannot be combined with any other filter parameter.
--
-- * 'dwbOwner' - The owner of the bundles to retrieve. This parameter cannot be combined with any other filter parameter. This contains one of the following values:     * null- Retrieves the bundles that belong to the account making the call.     * @AMAZON@ - Retrieves the bundles that are provided by AWS.
--
-- * 'dwbNextToken' - The @NextToken@ value from a previous call to this operation. Pass null if this is the first call.
describeWorkspaceBundles
    :: DescribeWorkspaceBundles
describeWorkspaceBundles =
  DescribeWorkspaceBundles'
  {_dwbBundleIds = Nothing, _dwbOwner = Nothing, _dwbNextToken = Nothing}


-- | An array of strings that contains the identifiers of the bundles to retrieve. This parameter cannot be combined with any other filter parameter.
dwbBundleIds :: Lens' DescribeWorkspaceBundles (Maybe (NonEmpty Text))
dwbBundleIds = lens _dwbBundleIds (\ s a -> s{_dwbBundleIds = a}) . mapping _List1;

-- | The owner of the bundles to retrieve. This parameter cannot be combined with any other filter parameter. This contains one of the following values:     * null- Retrieves the bundles that belong to the account making the call.     * @AMAZON@ - Retrieves the bundles that are provided by AWS.
dwbOwner :: Lens' DescribeWorkspaceBundles (Maybe Text)
dwbOwner = lens _dwbOwner (\ s a -> s{_dwbOwner = a});

-- | The @NextToken@ value from a previous call to this operation. Pass null if this is the first call.
dwbNextToken :: Lens' DescribeWorkspaceBundles (Maybe Text)
dwbNextToken = lens _dwbNextToken (\ s a -> s{_dwbNextToken = a});

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

-- | Contains the results of the 'DescribeWorkspaceBundles' operation.
--
--
--
-- /See:/ 'describeWorkspaceBundlesResponse' smart constructor.
data DescribeWorkspaceBundlesResponse = DescribeWorkspaceBundlesResponse'
  { _dwbrsBundles        :: !(Maybe [WorkspaceBundle])
  , _dwbrsNextToken      :: !(Maybe Text)
  , _dwbrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeWorkspaceBundlesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dwbrsBundles' - An array of structures that contain information about the bundles.
--
-- * 'dwbrsNextToken' - If not null, more results are available. Pass this value for the @NextToken@ parameter in a subsequent call to this operation to retrieve the next set of items. This token is valid for one day and must be used within that time frame.
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


-- | An array of structures that contain information about the bundles.
dwbrsBundles :: Lens' DescribeWorkspaceBundlesResponse [WorkspaceBundle]
dwbrsBundles = lens _dwbrsBundles (\ s a -> s{_dwbrsBundles = a}) . _Default . _Coerce;

-- | If not null, more results are available. Pass this value for the @NextToken@ parameter in a subsequent call to this operation to retrieve the next set of items. This token is valid for one day and must be used within that time frame.
dwbrsNextToken :: Lens' DescribeWorkspaceBundlesResponse (Maybe Text)
dwbrsNextToken = lens _dwbrsNextToken (\ s a -> s{_dwbrsNextToken = a});

-- | -- | The response status code.
dwbrsResponseStatus :: Lens' DescribeWorkspaceBundlesResponse Int
dwbrsResponseStatus = lens _dwbrsResponseStatus (\ s a -> s{_dwbrsResponseStatus = a});

instance NFData DescribeWorkspaceBundlesResponse
         where
