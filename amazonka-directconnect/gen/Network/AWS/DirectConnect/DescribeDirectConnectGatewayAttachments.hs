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
-- Module      : Network.AWS.DirectConnect.DescribeDirectConnectGatewayAttachments
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all direct connect gateway and virtual interface (VIF) attachments. Either a direct connect gateway ID or a VIF ID must be provided in the request. If a direct connect gateway ID is provided, the response returns all VIFs attached to the direct connect gateway. If a VIF ID is provided, the response returns all direct connect gateways attached to the VIF. If both are provided, the response only returns the attachment that matches both the direct connect gateway and the VIF.
--
--
module Network.AWS.DirectConnect.DescribeDirectConnectGatewayAttachments
    (
    -- * Creating a Request
      describeDirectConnectGatewayAttachments
    , DescribeDirectConnectGatewayAttachments
    -- * Request Lenses
    , ddcgasDirectConnectGatewayId
    , ddcgasNextToken
    , ddcgasMaxResults
    , ddcgasVirtualInterfaceId

    -- * Destructuring the Response
    , describeDirectConnectGatewayAttachmentsResponse
    , DescribeDirectConnectGatewayAttachmentsResponse
    -- * Response Lenses
    , ddcgasrsNextToken
    , ddcgasrsDirectConnectGatewayAttachments
    , ddcgasrsResponseStatus
    ) where

import Network.AWS.DirectConnect.Types
import Network.AWS.DirectConnect.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the DescribeDirectConnectGatewayAttachments operation.
--
--
--
-- /See:/ 'describeDirectConnectGatewayAttachments' smart constructor.
data DescribeDirectConnectGatewayAttachments = DescribeDirectConnectGatewayAttachments'
  { _ddcgasDirectConnectGatewayId :: !(Maybe Text)
  , _ddcgasNextToken              :: !(Maybe Text)
  , _ddcgasMaxResults             :: !(Maybe Int)
  , _ddcgasVirtualInterfaceId     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDirectConnectGatewayAttachments' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddcgasDirectConnectGatewayId' - The ID of the direct connect gateway. Example: "abcd1234-dcba-5678-be23-cdef9876ab45" Default: None
--
-- * 'ddcgasNextToken' - The token provided in the previous describe result to retrieve the next page of the result. Default: None
--
-- * 'ddcgasMaxResults' - The maximum number of direct connect gateway attachments to return per page. Example: 15 Default: None
--
-- * 'ddcgasVirtualInterfaceId' - The ID of the virtual interface. Example: "dxvif-abc123ef" Default: None
describeDirectConnectGatewayAttachments
    :: DescribeDirectConnectGatewayAttachments
describeDirectConnectGatewayAttachments =
  DescribeDirectConnectGatewayAttachments'
    { _ddcgasDirectConnectGatewayId = Nothing
    , _ddcgasNextToken = Nothing
    , _ddcgasMaxResults = Nothing
    , _ddcgasVirtualInterfaceId = Nothing
    }


-- | The ID of the direct connect gateway. Example: "abcd1234-dcba-5678-be23-cdef9876ab45" Default: None
ddcgasDirectConnectGatewayId :: Lens' DescribeDirectConnectGatewayAttachments (Maybe Text)
ddcgasDirectConnectGatewayId = lens _ddcgasDirectConnectGatewayId (\ s a -> s{_ddcgasDirectConnectGatewayId = a})

-- | The token provided in the previous describe result to retrieve the next page of the result. Default: None
ddcgasNextToken :: Lens' DescribeDirectConnectGatewayAttachments (Maybe Text)
ddcgasNextToken = lens _ddcgasNextToken (\ s a -> s{_ddcgasNextToken = a})

-- | The maximum number of direct connect gateway attachments to return per page. Example: 15 Default: None
ddcgasMaxResults :: Lens' DescribeDirectConnectGatewayAttachments (Maybe Int)
ddcgasMaxResults = lens _ddcgasMaxResults (\ s a -> s{_ddcgasMaxResults = a})

-- | The ID of the virtual interface. Example: "dxvif-abc123ef" Default: None
ddcgasVirtualInterfaceId :: Lens' DescribeDirectConnectGatewayAttachments (Maybe Text)
ddcgasVirtualInterfaceId = lens _ddcgasVirtualInterfaceId (\ s a -> s{_ddcgasVirtualInterfaceId = a})

instance AWSRequest
           DescribeDirectConnectGatewayAttachments
         where
        type Rs DescribeDirectConnectGatewayAttachments =
             DescribeDirectConnectGatewayAttachmentsResponse
        request = postJSON directConnect
        response
          = receiveJSON
              (\ s h x ->
                 DescribeDirectConnectGatewayAttachmentsResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "directConnectGatewayAttachments" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable
           DescribeDirectConnectGatewayAttachments
         where

instance NFData
           DescribeDirectConnectGatewayAttachments
         where

instance ToHeaders
           DescribeDirectConnectGatewayAttachments
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OvertureService.DescribeDirectConnectGatewayAttachments"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON
           DescribeDirectConnectGatewayAttachments
         where
        toJSON DescribeDirectConnectGatewayAttachments'{..}
          = object
              (catMaybes
                 [("directConnectGatewayId" .=) <$>
                    _ddcgasDirectConnectGatewayId,
                  ("nextToken" .=) <$> _ddcgasNextToken,
                  ("maxResults" .=) <$> _ddcgasMaxResults,
                  ("virtualInterfaceId" .=) <$>
                    _ddcgasVirtualInterfaceId])

instance ToPath
           DescribeDirectConnectGatewayAttachments
         where
        toPath = const "/"

instance ToQuery
           DescribeDirectConnectGatewayAttachments
         where
        toQuery = const mempty

-- | Container for the response from the DescribeDirectConnectGatewayAttachments API call
--
--
--
-- /See:/ 'describeDirectConnectGatewayAttachmentsResponse' smart constructor.
data DescribeDirectConnectGatewayAttachmentsResponse = DescribeDirectConnectGatewayAttachmentsResponse'
  { _ddcgasrsNextToken :: !(Maybe Text)
  , _ddcgasrsDirectConnectGatewayAttachments :: !(Maybe [DirectConnectGatewayAttachment])
  , _ddcgasrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDirectConnectGatewayAttachmentsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddcgasrsNextToken' - Undocumented member.
--
-- * 'ddcgasrsDirectConnectGatewayAttachments' - Information about the direct connect gateway attachments.
--
-- * 'ddcgasrsResponseStatus' - -- | The response status code.
describeDirectConnectGatewayAttachmentsResponse
    :: Int -- ^ 'ddcgasrsResponseStatus'
    -> DescribeDirectConnectGatewayAttachmentsResponse
describeDirectConnectGatewayAttachmentsResponse pResponseStatus_ =
  DescribeDirectConnectGatewayAttachmentsResponse'
    { _ddcgasrsNextToken = Nothing
    , _ddcgasrsDirectConnectGatewayAttachments = Nothing
    , _ddcgasrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
ddcgasrsNextToken :: Lens' DescribeDirectConnectGatewayAttachmentsResponse (Maybe Text)
ddcgasrsNextToken = lens _ddcgasrsNextToken (\ s a -> s{_ddcgasrsNextToken = a})

-- | Information about the direct connect gateway attachments.
ddcgasrsDirectConnectGatewayAttachments :: Lens' DescribeDirectConnectGatewayAttachmentsResponse [DirectConnectGatewayAttachment]
ddcgasrsDirectConnectGatewayAttachments = lens _ddcgasrsDirectConnectGatewayAttachments (\ s a -> s{_ddcgasrsDirectConnectGatewayAttachments = a}) . _Default . _Coerce

-- | -- | The response status code.
ddcgasrsResponseStatus :: Lens' DescribeDirectConnectGatewayAttachmentsResponse Int
ddcgasrsResponseStatus = lens _ddcgasrsResponseStatus (\ s a -> s{_ddcgasrsResponseStatus = a})

instance NFData
           DescribeDirectConnectGatewayAttachmentsResponse
         where
