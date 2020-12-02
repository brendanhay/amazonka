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
-- Module      : Network.AWS.DirectConnect.DescribeDirectConnectGateways
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of direct connect gateways in your account. Deleted direct connect gateways are not returned. You can provide a direct connect gateway ID in the request to return information about the specific direct connect gateway only. Otherwise, if a direct connect gateway ID is not provided, information about all of your direct connect gateways is returned.
--
--
module Network.AWS.DirectConnect.DescribeDirectConnectGateways
    (
    -- * Creating a Request
      describeDirectConnectGateways
    , DescribeDirectConnectGateways
    -- * Request Lenses
    , ddcgDirectConnectGatewayId
    , ddcgNextToken
    , ddcgMaxResults

    -- * Destructuring the Response
    , describeDirectConnectGatewaysResponse
    , DescribeDirectConnectGatewaysResponse
    -- * Response Lenses
    , ddcgrsDirectConnectGateways
    , ddcgrsNextToken
    , ddcgrsResponseStatus
    ) where

import Network.AWS.DirectConnect.Types
import Network.AWS.DirectConnect.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the DescribeDirectConnectGateways operation.
--
--
--
-- /See:/ 'describeDirectConnectGateways' smart constructor.
data DescribeDirectConnectGateways = DescribeDirectConnectGateways'
  { _ddcgDirectConnectGatewayId :: !(Maybe Text)
  , _ddcgNextToken              :: !(Maybe Text)
  , _ddcgMaxResults             :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDirectConnectGateways' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddcgDirectConnectGatewayId' - The ID of the direct connect gateway. Example: "abcd1234-dcba-5678-be23-cdef9876ab45" Default: None
--
-- * 'ddcgNextToken' - The token provided in the previous describe result to retrieve the next page of the result. Default: None
--
-- * 'ddcgMaxResults' - The maximum number of direct connect gateways to return per page. Example: 15 Default: None
describeDirectConnectGateways
    :: DescribeDirectConnectGateways
describeDirectConnectGateways =
  DescribeDirectConnectGateways'
    { _ddcgDirectConnectGatewayId = Nothing
    , _ddcgNextToken = Nothing
    , _ddcgMaxResults = Nothing
    }


-- | The ID of the direct connect gateway. Example: "abcd1234-dcba-5678-be23-cdef9876ab45" Default: None
ddcgDirectConnectGatewayId :: Lens' DescribeDirectConnectGateways (Maybe Text)
ddcgDirectConnectGatewayId = lens _ddcgDirectConnectGatewayId (\ s a -> s{_ddcgDirectConnectGatewayId = a})

-- | The token provided in the previous describe result to retrieve the next page of the result. Default: None
ddcgNextToken :: Lens' DescribeDirectConnectGateways (Maybe Text)
ddcgNextToken = lens _ddcgNextToken (\ s a -> s{_ddcgNextToken = a})

-- | The maximum number of direct connect gateways to return per page. Example: 15 Default: None
ddcgMaxResults :: Lens' DescribeDirectConnectGateways (Maybe Int)
ddcgMaxResults = lens _ddcgMaxResults (\ s a -> s{_ddcgMaxResults = a})

instance AWSRequest DescribeDirectConnectGateways
         where
        type Rs DescribeDirectConnectGateways =
             DescribeDirectConnectGatewaysResponse
        request = postJSON directConnect
        response
          = receiveJSON
              (\ s h x ->
                 DescribeDirectConnectGatewaysResponse' <$>
                   (x .?> "directConnectGateways" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeDirectConnectGateways where

instance NFData DescribeDirectConnectGateways where

instance ToHeaders DescribeDirectConnectGateways
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OvertureService.DescribeDirectConnectGateways" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeDirectConnectGateways where
        toJSON DescribeDirectConnectGateways'{..}
          = object
              (catMaybes
                 [("directConnectGatewayId" .=) <$>
                    _ddcgDirectConnectGatewayId,
                  ("nextToken" .=) <$> _ddcgNextToken,
                  ("maxResults" .=) <$> _ddcgMaxResults])

instance ToPath DescribeDirectConnectGateways where
        toPath = const "/"

instance ToQuery DescribeDirectConnectGateways where
        toQuery = const mempty

-- | Container for the response from the DescribeDirectConnectGateways API call
--
--
--
-- /See:/ 'describeDirectConnectGatewaysResponse' smart constructor.
data DescribeDirectConnectGatewaysResponse = DescribeDirectConnectGatewaysResponse'
  { _ddcgrsDirectConnectGateways :: !(Maybe [DirectConnectGateway])
  , _ddcgrsNextToken             :: !(Maybe Text)
  , _ddcgrsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDirectConnectGatewaysResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddcgrsDirectConnectGateways' - Information about the direct connect gateways.
--
-- * 'ddcgrsNextToken' - Undocumented member.
--
-- * 'ddcgrsResponseStatus' - -- | The response status code.
describeDirectConnectGatewaysResponse
    :: Int -- ^ 'ddcgrsResponseStatus'
    -> DescribeDirectConnectGatewaysResponse
describeDirectConnectGatewaysResponse pResponseStatus_ =
  DescribeDirectConnectGatewaysResponse'
    { _ddcgrsDirectConnectGateways = Nothing
    , _ddcgrsNextToken = Nothing
    , _ddcgrsResponseStatus = pResponseStatus_
    }


-- | Information about the direct connect gateways.
ddcgrsDirectConnectGateways :: Lens' DescribeDirectConnectGatewaysResponse [DirectConnectGateway]
ddcgrsDirectConnectGateways = lens _ddcgrsDirectConnectGateways (\ s a -> s{_ddcgrsDirectConnectGateways = a}) . _Default . _Coerce

-- | Undocumented member.
ddcgrsNextToken :: Lens' DescribeDirectConnectGatewaysResponse (Maybe Text)
ddcgrsNextToken = lens _ddcgrsNextToken (\ s a -> s{_ddcgrsNextToken = a})

-- | -- | The response status code.
ddcgrsResponseStatus :: Lens' DescribeDirectConnectGatewaysResponse Int
ddcgrsResponseStatus = lens _ddcgrsResponseStatus (\ s a -> s{_ddcgrsResponseStatus = a})

instance NFData DescribeDirectConnectGatewaysResponse
         where
