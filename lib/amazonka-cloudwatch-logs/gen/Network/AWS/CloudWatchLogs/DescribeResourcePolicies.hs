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
-- Module      : Network.AWS.CloudWatchLogs.DescribeResourcePolicies
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the resource policies in this account.
--
--
module Network.AWS.CloudWatchLogs.DescribeResourcePolicies
    (
    -- * Creating a Request
      describeResourcePolicies
    , DescribeResourcePolicies
    -- * Request Lenses
    , drpNextToken
    , drpLimit

    -- * Destructuring the Response
    , describeResourcePoliciesResponse
    , DescribeResourcePoliciesResponse
    -- * Response Lenses
    , drprsResourcePolicies
    , drprsNextToken
    , drprsResponseStatus
    ) where

import Network.AWS.CloudWatchLogs.Types
import Network.AWS.CloudWatchLogs.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeResourcePolicies' smart constructor.
data DescribeResourcePolicies = DescribeResourcePolicies'
  { _drpNextToken :: !(Maybe Text)
  , _drpLimit     :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeResourcePolicies' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drpNextToken' - Undocumented member.
--
-- * 'drpLimit' - The maximum number of resource policies to be displayed with one call of this API.
describeResourcePolicies
    :: DescribeResourcePolicies
describeResourcePolicies =
  DescribeResourcePolicies' {_drpNextToken = Nothing, _drpLimit = Nothing}


-- | Undocumented member.
drpNextToken :: Lens' DescribeResourcePolicies (Maybe Text)
drpNextToken = lens _drpNextToken (\ s a -> s{_drpNextToken = a})

-- | The maximum number of resource policies to be displayed with one call of this API.
drpLimit :: Lens' DescribeResourcePolicies (Maybe Natural)
drpLimit = lens _drpLimit (\ s a -> s{_drpLimit = a}) . mapping _Nat

instance AWSRequest DescribeResourcePolicies where
        type Rs DescribeResourcePolicies =
             DescribeResourcePoliciesResponse
        request = postJSON cloudWatchLogs
        response
          = receiveJSON
              (\ s h x ->
                 DescribeResourcePoliciesResponse' <$>
                   (x .?> "resourcePolicies" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeResourcePolicies where

instance NFData DescribeResourcePolicies where

instance ToHeaders DescribeResourcePolicies where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Logs_20140328.DescribeResourcePolicies" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeResourcePolicies where
        toJSON DescribeResourcePolicies'{..}
          = object
              (catMaybes
                 [("nextToken" .=) <$> _drpNextToken,
                  ("limit" .=) <$> _drpLimit])

instance ToPath DescribeResourcePolicies where
        toPath = const "/"

instance ToQuery DescribeResourcePolicies where
        toQuery = const mempty

-- | /See:/ 'describeResourcePoliciesResponse' smart constructor.
data DescribeResourcePoliciesResponse = DescribeResourcePoliciesResponse'
  { _drprsResourcePolicies :: !(Maybe [ResourcePolicy])
  , _drprsNextToken        :: !(Maybe Text)
  , _drprsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeResourcePoliciesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drprsResourcePolicies' - The resource policies that exist in this account.
--
-- * 'drprsNextToken' - Undocumented member.
--
-- * 'drprsResponseStatus' - -- | The response status code.
describeResourcePoliciesResponse
    :: Int -- ^ 'drprsResponseStatus'
    -> DescribeResourcePoliciesResponse
describeResourcePoliciesResponse pResponseStatus_ =
  DescribeResourcePoliciesResponse'
    { _drprsResourcePolicies = Nothing
    , _drprsNextToken = Nothing
    , _drprsResponseStatus = pResponseStatus_
    }


-- | The resource policies that exist in this account.
drprsResourcePolicies :: Lens' DescribeResourcePoliciesResponse [ResourcePolicy]
drprsResourcePolicies = lens _drprsResourcePolicies (\ s a -> s{_drprsResourcePolicies = a}) . _Default . _Coerce

-- | Undocumented member.
drprsNextToken :: Lens' DescribeResourcePoliciesResponse (Maybe Text)
drprsNextToken = lens _drprsNextToken (\ s a -> s{_drprsNextToken = a})

-- | -- | The response status code.
drprsResponseStatus :: Lens' DescribeResourcePoliciesResponse Int
drprsResponseStatus = lens _drprsResponseStatus (\ s a -> s{_drprsResponseStatus = a})

instance NFData DescribeResourcePoliciesResponse
         where
