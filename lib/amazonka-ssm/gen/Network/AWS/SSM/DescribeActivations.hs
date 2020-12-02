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
-- Module      : Network.AWS.SSM.DescribeActivations
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Details about the activation, including: the date and time the activation was created, the expiration date, the IAM role assigned to the instances in the activation, and the number of instances activated by this registration.
--
--
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeActivations
    (
    -- * Creating a Request
      describeActivations
    , DescribeActivations
    -- * Request Lenses
    , daFilters
    , daNextToken
    , daMaxResults

    -- * Destructuring the Response
    , describeActivationsResponse
    , DescribeActivationsResponse
    -- * Response Lenses
    , darsActivationList
    , darsNextToken
    , darsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'describeActivations' smart constructor.
data DescribeActivations = DescribeActivations'
  { _daFilters    :: !(Maybe [DescribeActivationsFilter])
  , _daNextToken  :: !(Maybe Text)
  , _daMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeActivations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daFilters' - A filter to view information about your activations.
--
-- * 'daNextToken' - A token to start the list. Use this token to get the next set of results.
--
-- * 'daMaxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
describeActivations
    :: DescribeActivations
describeActivations =
  DescribeActivations'
    {_daFilters = Nothing, _daNextToken = Nothing, _daMaxResults = Nothing}


-- | A filter to view information about your activations.
daFilters :: Lens' DescribeActivations [DescribeActivationsFilter]
daFilters = lens _daFilters (\ s a -> s{_daFilters = a}) . _Default . _Coerce

-- | A token to start the list. Use this token to get the next set of results.
daNextToken :: Lens' DescribeActivations (Maybe Text)
daNextToken = lens _daNextToken (\ s a -> s{_daNextToken = a})

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
daMaxResults :: Lens' DescribeActivations (Maybe Natural)
daMaxResults = lens _daMaxResults (\ s a -> s{_daMaxResults = a}) . mapping _Nat

instance AWSPager DescribeActivations where
        page rq rs
          | stop (rs ^. darsNextToken) = Nothing
          | stop (rs ^. darsActivationList) = Nothing
          | otherwise =
            Just $ rq & daNextToken .~ rs ^. darsNextToken

instance AWSRequest DescribeActivations where
        type Rs DescribeActivations =
             DescribeActivationsResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 DescribeActivationsResponse' <$>
                   (x .?> "ActivationList" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeActivations where

instance NFData DescribeActivations where

instance ToHeaders DescribeActivations where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.DescribeActivations" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeActivations where
        toJSON DescribeActivations'{..}
          = object
              (catMaybes
                 [("Filters" .=) <$> _daFilters,
                  ("NextToken" .=) <$> _daNextToken,
                  ("MaxResults" .=) <$> _daMaxResults])

instance ToPath DescribeActivations where
        toPath = const "/"

instance ToQuery DescribeActivations where
        toQuery = const mempty

-- | /See:/ 'describeActivationsResponse' smart constructor.
data DescribeActivationsResponse = DescribeActivationsResponse'
  { _darsActivationList :: !(Maybe [Activation])
  , _darsNextToken      :: !(Maybe Text)
  , _darsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeActivationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'darsActivationList' - A list of activations for your AWS account.
--
-- * 'darsNextToken' - The token for the next set of items to return. Use this token to get the next set of results.
--
-- * 'darsResponseStatus' - -- | The response status code.
describeActivationsResponse
    :: Int -- ^ 'darsResponseStatus'
    -> DescribeActivationsResponse
describeActivationsResponse pResponseStatus_ =
  DescribeActivationsResponse'
    { _darsActivationList = Nothing
    , _darsNextToken = Nothing
    , _darsResponseStatus = pResponseStatus_
    }


-- | A list of activations for your AWS account.
darsActivationList :: Lens' DescribeActivationsResponse [Activation]
darsActivationList = lens _darsActivationList (\ s a -> s{_darsActivationList = a}) . _Default . _Coerce

-- | The token for the next set of items to return. Use this token to get the next set of results.
darsNextToken :: Lens' DescribeActivationsResponse (Maybe Text)
darsNextToken = lens _darsNextToken (\ s a -> s{_darsNextToken = a})

-- | -- | The response status code.
darsResponseStatus :: Lens' DescribeActivationsResponse Int
darsResponseStatus = lens _darsResponseStatus (\ s a -> s{_darsResponseStatus = a})

instance NFData DescribeActivationsResponse where
