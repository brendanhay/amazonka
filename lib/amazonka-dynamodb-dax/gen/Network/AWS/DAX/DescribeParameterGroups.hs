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
-- Module      : Network.AWS.DAX.DescribeParameterGroups
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of parameter group descriptions. If a parameter group name is specified, the list will contain only the descriptions for that group.
--
--
module Network.AWS.DAX.DescribeParameterGroups
    (
    -- * Creating a Request
      describeParameterGroups
    , DescribeParameterGroups
    -- * Request Lenses
    , dpgNextToken
    , dpgParameterGroupNames
    , dpgMaxResults

    -- * Destructuring the Response
    , describeParameterGroupsResponse
    , DescribeParameterGroupsResponse
    -- * Response Lenses
    , dpgsrsNextToken
    , dpgsrsParameterGroups
    , dpgsrsResponseStatus
    ) where

import Network.AWS.DAX.Types
import Network.AWS.DAX.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeParameterGroups' smart constructor.
data DescribeParameterGroups = DescribeParameterGroups'
  { _dpgNextToken           :: !(Maybe Text)
  , _dpgParameterGroupNames :: !(Maybe [Text])
  , _dpgMaxResults          :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeParameterGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpgNextToken' - An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
--
-- * 'dpgParameterGroupNames' - The names of the parameter groups.
--
-- * 'dpgMaxResults' - The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved. The value for @MaxResults@ must be between 20 and 100.
describeParameterGroups
    :: DescribeParameterGroups
describeParameterGroups =
  DescribeParameterGroups'
    { _dpgNextToken = Nothing
    , _dpgParameterGroupNames = Nothing
    , _dpgMaxResults = Nothing
    }


-- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
dpgNextToken :: Lens' DescribeParameterGroups (Maybe Text)
dpgNextToken = lens _dpgNextToken (\ s a -> s{_dpgNextToken = a})

-- | The names of the parameter groups.
dpgParameterGroupNames :: Lens' DescribeParameterGroups [Text]
dpgParameterGroupNames = lens _dpgParameterGroupNames (\ s a -> s{_dpgParameterGroupNames = a}) . _Default . _Coerce

-- | The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved. The value for @MaxResults@ must be between 20 and 100.
dpgMaxResults :: Lens' DescribeParameterGroups (Maybe Int)
dpgMaxResults = lens _dpgMaxResults (\ s a -> s{_dpgMaxResults = a})

instance AWSRequest DescribeParameterGroups where
        type Rs DescribeParameterGroups =
             DescribeParameterGroupsResponse
        request = postJSON dax
        response
          = receiveJSON
              (\ s h x ->
                 DescribeParameterGroupsResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "ParameterGroups" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeParameterGroups where

instance NFData DescribeParameterGroups where

instance ToHeaders DescribeParameterGroups where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonDAXV3.DescribeParameterGroups" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeParameterGroups where
        toJSON DescribeParameterGroups'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _dpgNextToken,
                  ("ParameterGroupNames" .=) <$>
                    _dpgParameterGroupNames,
                  ("MaxResults" .=) <$> _dpgMaxResults])

instance ToPath DescribeParameterGroups where
        toPath = const "/"

instance ToQuery DescribeParameterGroups where
        toQuery = const mempty

-- | /See:/ 'describeParameterGroupsResponse' smart constructor.
data DescribeParameterGroupsResponse = DescribeParameterGroupsResponse'
  { _dpgsrsNextToken       :: !(Maybe Text)
  , _dpgsrsParameterGroups :: !(Maybe [ParameterGroup])
  , _dpgsrsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeParameterGroupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpgsrsNextToken' - Provides an identifier to allow retrieval of paginated results.
--
-- * 'dpgsrsParameterGroups' - An array of parameter groups. Each element in the array represents one parameter group.
--
-- * 'dpgsrsResponseStatus' - -- | The response status code.
describeParameterGroupsResponse
    :: Int -- ^ 'dpgsrsResponseStatus'
    -> DescribeParameterGroupsResponse
describeParameterGroupsResponse pResponseStatus_ =
  DescribeParameterGroupsResponse'
    { _dpgsrsNextToken = Nothing
    , _dpgsrsParameterGroups = Nothing
    , _dpgsrsResponseStatus = pResponseStatus_
    }


-- | Provides an identifier to allow retrieval of paginated results.
dpgsrsNextToken :: Lens' DescribeParameterGroupsResponse (Maybe Text)
dpgsrsNextToken = lens _dpgsrsNextToken (\ s a -> s{_dpgsrsNextToken = a})

-- | An array of parameter groups. Each element in the array represents one parameter group.
dpgsrsParameterGroups :: Lens' DescribeParameterGroupsResponse [ParameterGroup]
dpgsrsParameterGroups = lens _dpgsrsParameterGroups (\ s a -> s{_dpgsrsParameterGroups = a}) . _Default . _Coerce

-- | -- | The response status code.
dpgsrsResponseStatus :: Lens' DescribeParameterGroupsResponse Int
dpgsrsResponseStatus = lens _dpgsrsResponseStatus (\ s a -> s{_dpgsrsResponseStatus = a})

instance NFData DescribeParameterGroupsResponse where
