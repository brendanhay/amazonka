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
-- Module      : Network.AWS.AppStream.DescribeStacks
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified stacks or all stacks in the account.
--
--
module Network.AWS.AppStream.DescribeStacks
    (
    -- * Creating a Request
      describeStacks
    , DescribeStacks
    -- * Request Lenses
    , dNextToken
    , dNames

    -- * Destructuring the Response
    , describeStacksResponse
    , DescribeStacksResponse
    -- * Response Lenses
    , desrsNextToken
    , desrsStacks
    , desrsResponseStatus
    ) where

import Network.AWS.AppStream.Types
import Network.AWS.AppStream.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeStacks' smart constructor.
data DescribeStacks = DescribeStacks'
  { _dNextToken :: !(Maybe Text)
  , _dNames     :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeStacks' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dNextToken' - The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
--
-- * 'dNames' - The names of the stacks to describe.
describeStacks
    :: DescribeStacks
describeStacks = DescribeStacks' {_dNextToken = Nothing, _dNames = Nothing}


-- | The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
dNextToken :: Lens' DescribeStacks (Maybe Text)
dNextToken = lens _dNextToken (\ s a -> s{_dNextToken = a})

-- | The names of the stacks to describe.
dNames :: Lens' DescribeStacks [Text]
dNames = lens _dNames (\ s a -> s{_dNames = a}) . _Default . _Coerce

instance AWSRequest DescribeStacks where
        type Rs DescribeStacks = DescribeStacksResponse
        request = postJSON appStream
        response
          = receiveJSON
              (\ s h x ->
                 DescribeStacksResponse' <$>
                   (x .?> "NextToken") <*> (x .?> "Stacks" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeStacks where

instance NFData DescribeStacks where

instance ToHeaders DescribeStacks where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("PhotonAdminProxyService.DescribeStacks" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeStacks where
        toJSON DescribeStacks'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _dNextToken,
                  ("Names" .=) <$> _dNames])

instance ToPath DescribeStacks where
        toPath = const "/"

instance ToQuery DescribeStacks where
        toQuery = const mempty

-- | /See:/ 'describeStacksResponse' smart constructor.
data DescribeStacksResponse = DescribeStacksResponse'
  { _desrsNextToken      :: !(Maybe Text)
  , _desrsStacks         :: !(Maybe [Stack])
  , _desrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeStacksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desrsNextToken' - The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
--
-- * 'desrsStacks' - Information about the stacks.
--
-- * 'desrsResponseStatus' - -- | The response status code.
describeStacksResponse
    :: Int -- ^ 'desrsResponseStatus'
    -> DescribeStacksResponse
describeStacksResponse pResponseStatus_ =
  DescribeStacksResponse'
    { _desrsNextToken = Nothing
    , _desrsStacks = Nothing
    , _desrsResponseStatus = pResponseStatus_
    }


-- | The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
desrsNextToken :: Lens' DescribeStacksResponse (Maybe Text)
desrsNextToken = lens _desrsNextToken (\ s a -> s{_desrsNextToken = a})

-- | Information about the stacks.
desrsStacks :: Lens' DescribeStacksResponse [Stack]
desrsStacks = lens _desrsStacks (\ s a -> s{_desrsStacks = a}) . _Default . _Coerce

-- | -- | The response status code.
desrsResponseStatus :: Lens' DescribeStacksResponse Int
desrsResponseStatus = lens _desrsResponseStatus (\ s a -> s{_desrsResponseStatus = a})

instance NFData DescribeStacksResponse where
