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
-- Module      : Network.AWS.OpsWorksCM.DescribeServers
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all configuration management servers that are identified with your account. Only the stored results from Amazon DynamoDB are returned. AWS OpsWorks CM does not query other services.
--
--
-- This operation is synchronous.
--
-- A @ResourceNotFoundException@ is thrown when the server does not exist. A @ValidationException@ is raised when parameters of the request are not valid.
--
module Network.AWS.OpsWorksCM.DescribeServers
    (
    -- * Creating a Request
      describeServers
    , DescribeServers
    -- * Request Lenses
    , dssServerName
    , dssNextToken
    , dssMaxResults

    -- * Destructuring the Response
    , describeServersResponse
    , DescribeServersResponse
    -- * Response Lenses
    , dssrsServers
    , dssrsNextToken
    , dssrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorksCM.Types
import Network.AWS.OpsWorksCM.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeServers' smart constructor.
data DescribeServers = DescribeServers'
  { _dssServerName :: !(Maybe Text)
  , _dssNextToken  :: !(Maybe Text)
  , _dssMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeServers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dssServerName' - Describes the server with the specified ServerName.
--
-- * 'dssNextToken' - NextToken is a string that is returned in some command responses. It indicates that not all entries have been returned, and that you must run at least one more request to get remaining items. To get remaining results, call @DescribeServers@ again, and assign the token from the previous results as the value of the @nextToken@ parameter. If there are no more results, the response object's @nextToken@ parameter value is @null@ . Setting a @nextToken@ value that was not returned in your previous results causes an @InvalidNextTokenException@ to occur.
--
-- * 'dssMaxResults' - To receive a paginated response, use this parameter to specify the maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
describeServers
    :: DescribeServers
describeServers =
  DescribeServers'
    { _dssServerName = Nothing
    , _dssNextToken = Nothing
    , _dssMaxResults = Nothing
    }


-- | Describes the server with the specified ServerName.
dssServerName :: Lens' DescribeServers (Maybe Text)
dssServerName = lens _dssServerName (\ s a -> s{_dssServerName = a})

-- | NextToken is a string that is returned in some command responses. It indicates that not all entries have been returned, and that you must run at least one more request to get remaining items. To get remaining results, call @DescribeServers@ again, and assign the token from the previous results as the value of the @nextToken@ parameter. If there are no more results, the response object's @nextToken@ parameter value is @null@ . Setting a @nextToken@ value that was not returned in your previous results causes an @InvalidNextTokenException@ to occur.
dssNextToken :: Lens' DescribeServers (Maybe Text)
dssNextToken = lens _dssNextToken (\ s a -> s{_dssNextToken = a})

-- | To receive a paginated response, use this parameter to specify the maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
dssMaxResults :: Lens' DescribeServers (Maybe Natural)
dssMaxResults = lens _dssMaxResults (\ s a -> s{_dssMaxResults = a}) . mapping _Nat

instance AWSRequest DescribeServers where
        type Rs DescribeServers = DescribeServersResponse
        request = postJSON opsWorksCM
        response
          = receiveJSON
              (\ s h x ->
                 DescribeServersResponse' <$>
                   (x .?> "Servers" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeServers where

instance NFData DescribeServers where

instance ToHeaders DescribeServers where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorksCM_V2016_11_01.DescribeServers" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeServers where
        toJSON DescribeServers'{..}
          = object
              (catMaybes
                 [("ServerName" .=) <$> _dssServerName,
                  ("NextToken" .=) <$> _dssNextToken,
                  ("MaxResults" .=) <$> _dssMaxResults])

instance ToPath DescribeServers where
        toPath = const "/"

instance ToQuery DescribeServers where
        toQuery = const mempty

-- | /See:/ 'describeServersResponse' smart constructor.
data DescribeServersResponse = DescribeServersResponse'
  { _dssrsServers        :: !(Maybe [Server])
  , _dssrsNextToken      :: !(Maybe Text)
  , _dssrsResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeServersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dssrsServers' - Contains the response to a @DescribeServers@ request. /For Puppet Server:/ @DescribeServersResponse$Servers$EngineAttributes@ contains PUPPET_API_CA_CERT. This is the PEM-encoded CA certificate that is used by the Puppet API over TCP port number 8140. The CA certificate is also used to sign node certificates.
--
-- * 'dssrsNextToken' - NextToken is a string that is returned in some command responses. It indicates that not all entries have been returned, and that you must run at least one more request to get remaining items. To get remaining results, call @DescribeServers@ again, and assign the token from the previous results as the value of the @nextToken@ parameter. If there are no more results, the response object's @nextToken@ parameter value is @null@ . Setting a @nextToken@ value that was not returned in your previous results causes an @InvalidNextTokenException@ to occur.
--
-- * 'dssrsResponseStatus' - -- | The response status code.
describeServersResponse
    :: Int -- ^ 'dssrsResponseStatus'
    -> DescribeServersResponse
describeServersResponse pResponseStatus_ =
  DescribeServersResponse'
    { _dssrsServers = Nothing
    , _dssrsNextToken = Nothing
    , _dssrsResponseStatus = pResponseStatus_
    }


-- | Contains the response to a @DescribeServers@ request. /For Puppet Server:/ @DescribeServersResponse$Servers$EngineAttributes@ contains PUPPET_API_CA_CERT. This is the PEM-encoded CA certificate that is used by the Puppet API over TCP port number 8140. The CA certificate is also used to sign node certificates.
dssrsServers :: Lens' DescribeServersResponse [Server]
dssrsServers = lens _dssrsServers (\ s a -> s{_dssrsServers = a}) . _Default . _Coerce

-- | NextToken is a string that is returned in some command responses. It indicates that not all entries have been returned, and that you must run at least one more request to get remaining items. To get remaining results, call @DescribeServers@ again, and assign the token from the previous results as the value of the @nextToken@ parameter. If there are no more results, the response object's @nextToken@ parameter value is @null@ . Setting a @nextToken@ value that was not returned in your previous results causes an @InvalidNextTokenException@ to occur.
dssrsNextToken :: Lens' DescribeServersResponse (Maybe Text)
dssrsNextToken = lens _dssrsNextToken (\ s a -> s{_dssrsNextToken = a})

-- | -- | The response status code.
dssrsResponseStatus :: Lens' DescribeServersResponse Int
dssrsResponseStatus = lens _dssrsResponseStatus (\ s a -> s{_dssrsResponseStatus = a})

instance NFData DescribeServersResponse where
