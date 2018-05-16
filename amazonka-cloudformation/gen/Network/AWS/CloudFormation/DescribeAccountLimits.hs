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
-- Module      : Network.AWS.CloudFormation.DescribeAccountLimits
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves your account's AWS CloudFormation limits, such as the maximum number of stacks that you can create in your account.
--
--
module Network.AWS.CloudFormation.DescribeAccountLimits
    (
    -- * Creating a Request
      describeAccountLimits
    , DescribeAccountLimits
    -- * Request Lenses
    , dalNextToken

    -- * Destructuring the Response
    , describeAccountLimitsResponse
    , DescribeAccountLimitsResponse
    -- * Response Lenses
    , dalrsNextToken
    , dalrsAccountLimits
    , dalrsResponseStatus
    ) where

import Network.AWS.CloudFormation.Types
import Network.AWS.CloudFormation.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the 'DescribeAccountLimits' action.
--
--
--
-- /See:/ 'describeAccountLimits' smart constructor.
newtype DescribeAccountLimits = DescribeAccountLimits'
  { _dalNextToken :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAccountLimits' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dalNextToken' - A string that identifies the next page of limits that you want to retrieve.
describeAccountLimits
    :: DescribeAccountLimits
describeAccountLimits = DescribeAccountLimits' {_dalNextToken = Nothing}


-- | A string that identifies the next page of limits that you want to retrieve.
dalNextToken :: Lens' DescribeAccountLimits (Maybe Text)
dalNextToken = lens _dalNextToken (\ s a -> s{_dalNextToken = a})

instance AWSRequest DescribeAccountLimits where
        type Rs DescribeAccountLimits =
             DescribeAccountLimitsResponse
        request = postQuery cloudFormation
        response
          = receiveXMLWrapper "DescribeAccountLimitsResult"
              (\ s h x ->
                 DescribeAccountLimitsResponse' <$>
                   (x .@? "NextToken") <*>
                     (x .@? "AccountLimits" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeAccountLimits where

instance NFData DescribeAccountLimits where

instance ToHeaders DescribeAccountLimits where
        toHeaders = const mempty

instance ToPath DescribeAccountLimits where
        toPath = const "/"

instance ToQuery DescribeAccountLimits where
        toQuery DescribeAccountLimits'{..}
          = mconcat
              ["Action" =: ("DescribeAccountLimits" :: ByteString),
               "Version" =: ("2010-05-15" :: ByteString),
               "NextToken" =: _dalNextToken]

-- | The output for the 'DescribeAccountLimits' action.
--
--
--
-- /See:/ 'describeAccountLimitsResponse' smart constructor.
data DescribeAccountLimitsResponse = DescribeAccountLimitsResponse'
  { _dalrsNextToken      :: !(Maybe Text)
  , _dalrsAccountLimits  :: !(Maybe [AccountLimit])
  , _dalrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAccountLimitsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dalrsNextToken' - If the output exceeds 1 MB in size, a string that identifies the next page of limits. If no additional page exists, this value is null.
--
-- * 'dalrsAccountLimits' - An account limit structure that contain a list of AWS CloudFormation account limits and their values.
--
-- * 'dalrsResponseStatus' - -- | The response status code.
describeAccountLimitsResponse
    :: Int -- ^ 'dalrsResponseStatus'
    -> DescribeAccountLimitsResponse
describeAccountLimitsResponse pResponseStatus_ =
  DescribeAccountLimitsResponse'
    { _dalrsNextToken = Nothing
    , _dalrsAccountLimits = Nothing
    , _dalrsResponseStatus = pResponseStatus_
    }


-- | If the output exceeds 1 MB in size, a string that identifies the next page of limits. If no additional page exists, this value is null.
dalrsNextToken :: Lens' DescribeAccountLimitsResponse (Maybe Text)
dalrsNextToken = lens _dalrsNextToken (\ s a -> s{_dalrsNextToken = a})

-- | An account limit structure that contain a list of AWS CloudFormation account limits and their values.
dalrsAccountLimits :: Lens' DescribeAccountLimitsResponse [AccountLimit]
dalrsAccountLimits = lens _dalrsAccountLimits (\ s a -> s{_dalrsAccountLimits = a}) . _Default . _Coerce

-- | -- | The response status code.
dalrsResponseStatus :: Lens' DescribeAccountLimitsResponse Int
dalrsResponseStatus = lens _dalrsResponseStatus (\ s a -> s{_dalrsResponseStatus = a})

instance NFData DescribeAccountLimitsResponse where
