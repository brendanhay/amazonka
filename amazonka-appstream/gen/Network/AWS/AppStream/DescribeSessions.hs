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
-- Module      : Network.AWS.AppStream.DescribeSessions
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the streaming sessions for a stack and a fleet. If a user ID is provided, this operation returns streaming sessions for only that user. To retrieve the next set of items, pass this value for the @nextToken@ parameter in a subsequent call to this operation. If an authentication type is not provided, the operation defaults to users authenticated using a streaming URL.
--
--
module Network.AWS.AppStream.DescribeSessions
    (
    -- * Creating a Request
      describeSessions
    , DescribeSessions
    -- * Request Lenses
    , dsUserId
    , dsNextToken
    , dsLimit
    , dsAuthenticationType
    , dsStackName
    , dsFleetName

    -- * Destructuring the Response
    , describeSessionsResponse
    , DescribeSessionsResponse
    -- * Response Lenses
    , dssrsNextToken
    , dssrsSessions
    , dssrsResponseStatus
    ) where

import           Network.AWS.AppStream.Types
import           Network.AWS.AppStream.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeSessions' smart constructor.
data DescribeSessions = DescribeSessions'
    { _dsUserId             :: !(Maybe Text)
    , _dsNextToken          :: !(Maybe Text)
    , _dsLimit              :: !(Maybe Int)
    , _dsAuthenticationType :: !(Maybe AuthenticationType)
    , _dsStackName          :: !Text
    , _dsFleetName          :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeSessions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsUserId' - The user for whom to list sessions. Use null to describe all the sessions for the stack and fleet.
--
-- * 'dsNextToken' - The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
--
-- * 'dsLimit' - The size of each page of results. The default value is 20 and the maximum supported value is 50.
--
-- * 'dsAuthenticationType' - The authentication method of the user. It can be @API@ for a user authenticated using a streaming URL, or @SAML@ for a SAML federated user. If an authentication type is not provided, the operation defaults to users authenticated using a streaming URL.
--
-- * 'dsStackName' - The name of the stack for which to list sessions.
--
-- * 'dsFleetName' - The name of the fleet for which to list sessions.
describeSessions
    :: Text -- ^ 'dsStackName'
    -> Text -- ^ 'dsFleetName'
    -> DescribeSessions
describeSessions pStackName_ pFleetName_ =
    DescribeSessions'
    { _dsUserId = Nothing
    , _dsNextToken = Nothing
    , _dsLimit = Nothing
    , _dsAuthenticationType = Nothing
    , _dsStackName = pStackName_
    , _dsFleetName = pFleetName_
    }

-- | The user for whom to list sessions. Use null to describe all the sessions for the stack and fleet.
dsUserId :: Lens' DescribeSessions (Maybe Text)
dsUserId = lens _dsUserId (\ s a -> s{_dsUserId = a});

-- | The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
dsNextToken :: Lens' DescribeSessions (Maybe Text)
dsNextToken = lens _dsNextToken (\ s a -> s{_dsNextToken = a});

-- | The size of each page of results. The default value is 20 and the maximum supported value is 50.
dsLimit :: Lens' DescribeSessions (Maybe Int)
dsLimit = lens _dsLimit (\ s a -> s{_dsLimit = a});

-- | The authentication method of the user. It can be @API@ for a user authenticated using a streaming URL, or @SAML@ for a SAML federated user. If an authentication type is not provided, the operation defaults to users authenticated using a streaming URL.
dsAuthenticationType :: Lens' DescribeSessions (Maybe AuthenticationType)
dsAuthenticationType = lens _dsAuthenticationType (\ s a -> s{_dsAuthenticationType = a});

-- | The name of the stack for which to list sessions.
dsStackName :: Lens' DescribeSessions Text
dsStackName = lens _dsStackName (\ s a -> s{_dsStackName = a});

-- | The name of the fleet for which to list sessions.
dsFleetName :: Lens' DescribeSessions Text
dsFleetName = lens _dsFleetName (\ s a -> s{_dsFleetName = a});

instance AWSRequest DescribeSessions where
        type Rs DescribeSessions = DescribeSessionsResponse
        request = postJSON appStream
        response
          = receiveJSON
              (\ s h x ->
                 DescribeSessionsResponse' <$>
                   (x .?> "NextToken") <*> (x .?> "Sessions" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeSessions

instance NFData DescribeSessions

instance ToHeaders DescribeSessions where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("PhotonAdminProxyService.DescribeSessions" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeSessions where
        toJSON DescribeSessions'{..}
          = object
              (catMaybes
                 [("UserId" .=) <$> _dsUserId,
                  ("NextToken" .=) <$> _dsNextToken,
                  ("Limit" .=) <$> _dsLimit,
                  ("AuthenticationType" .=) <$> _dsAuthenticationType,
                  Just ("StackName" .= _dsStackName),
                  Just ("FleetName" .= _dsFleetName)])

instance ToPath DescribeSessions where
        toPath = const "/"

instance ToQuery DescribeSessions where
        toQuery = const mempty

-- | /See:/ 'describeSessionsResponse' smart constructor.
data DescribeSessionsResponse = DescribeSessionsResponse'
    { _dssrsNextToken      :: !(Maybe Text)
    , _dssrsSessions       :: !(Maybe [Session])
    , _dssrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeSessionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dssrsNextToken' - The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
--
-- * 'dssrsSessions' - The list of streaming sessions.
--
-- * 'dssrsResponseStatus' - -- | The response status code.
describeSessionsResponse
    :: Int -- ^ 'dssrsResponseStatus'
    -> DescribeSessionsResponse
describeSessionsResponse pResponseStatus_ =
    DescribeSessionsResponse'
    { _dssrsNextToken = Nothing
    , _dssrsSessions = Nothing
    , _dssrsResponseStatus = pResponseStatus_
    }

-- | The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
dssrsNextToken :: Lens' DescribeSessionsResponse (Maybe Text)
dssrsNextToken = lens _dssrsNextToken (\ s a -> s{_dssrsNextToken = a});

-- | The list of streaming sessions.
dssrsSessions :: Lens' DescribeSessionsResponse [Session]
dssrsSessions = lens _dssrsSessions (\ s a -> s{_dssrsSessions = a}) . _Default . _Coerce;

-- | -- | The response status code.
dssrsResponseStatus :: Lens' DescribeSessionsResponse Int
dssrsResponseStatus = lens _dssrsResponseStatus (\ s a -> s{_dssrsResponseStatus = a});

instance NFData DescribeSessionsResponse
