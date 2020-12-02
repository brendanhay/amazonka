{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.DescribeDirectConnectGatewayAttachments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the attachments between your Direct Connect gateways and virtual interfaces. You must specify a Direct Connect gateway, a virtual interface, or both. If you specify a Direct Connect gateway, the response contains all virtual interfaces attached to the Direct Connect gateway. If you specify a virtual interface, the response contains all Direct Connect gateways attached to the virtual interface. If you specify both, the response contains the attachment between the Direct Connect gateway and the virtual interface.
--
--
--
-- This operation returns paginated results.
module Network.AWS.DirectConnect.DescribeDirectConnectGatewayAttachments
  ( -- * Creating a Request
    describeDirectConnectGatewayAttachments,
    DescribeDirectConnectGatewayAttachments,

    -- * Request Lenses
    ddcgasDirectConnectGatewayId,
    ddcgasNextToken,
    ddcgasMaxResults,
    ddcgasVirtualInterfaceId,

    -- * Destructuring the Response
    describeDirectConnectGatewayAttachmentsResponse,
    DescribeDirectConnectGatewayAttachmentsResponse,

    -- * Response Lenses
    ddcgasrsNextToken,
    ddcgasrsDirectConnectGatewayAttachments,
    ddcgasrsResponseStatus,
  )
where

import Network.AWS.DirectConnect.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeDirectConnectGatewayAttachments' smart constructor.
data DescribeDirectConnectGatewayAttachments = DescribeDirectConnectGatewayAttachments'
  { _ddcgasDirectConnectGatewayId ::
      !( Maybe
           Text
       ),
    _ddcgasNextToken ::
      !( Maybe
           Text
       ),
    _ddcgasMaxResults ::
      !( Maybe
           Int
       ),
    _ddcgasVirtualInterfaceId ::
      !( Maybe
           Text
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeDirectConnectGatewayAttachments' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddcgasDirectConnectGatewayId' - The ID of the Direct Connect gateway.
--
-- * 'ddcgasNextToken' - The token provided in the previous call to retrieve the next page.
--
-- * 'ddcgasMaxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value. If @MaxResults@ is given a value larger than 100, only 100 results are returned.
--
-- * 'ddcgasVirtualInterfaceId' - The ID of the virtual interface.
describeDirectConnectGatewayAttachments ::
  DescribeDirectConnectGatewayAttachments
describeDirectConnectGatewayAttachments =
  DescribeDirectConnectGatewayAttachments'
    { _ddcgasDirectConnectGatewayId =
        Nothing,
      _ddcgasNextToken = Nothing,
      _ddcgasMaxResults = Nothing,
      _ddcgasVirtualInterfaceId = Nothing
    }

-- | The ID of the Direct Connect gateway.
ddcgasDirectConnectGatewayId :: Lens' DescribeDirectConnectGatewayAttachments (Maybe Text)
ddcgasDirectConnectGatewayId = lens _ddcgasDirectConnectGatewayId (\s a -> s {_ddcgasDirectConnectGatewayId = a})

-- | The token provided in the previous call to retrieve the next page.
ddcgasNextToken :: Lens' DescribeDirectConnectGatewayAttachments (Maybe Text)
ddcgasNextToken = lens _ddcgasNextToken (\s a -> s {_ddcgasNextToken = a})

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value. If @MaxResults@ is given a value larger than 100, only 100 results are returned.
ddcgasMaxResults :: Lens' DescribeDirectConnectGatewayAttachments (Maybe Int)
ddcgasMaxResults = lens _ddcgasMaxResults (\s a -> s {_ddcgasMaxResults = a})

-- | The ID of the virtual interface.
ddcgasVirtualInterfaceId :: Lens' DescribeDirectConnectGatewayAttachments (Maybe Text)
ddcgasVirtualInterfaceId = lens _ddcgasVirtualInterfaceId (\s a -> s {_ddcgasVirtualInterfaceId = a})

instance AWSPager DescribeDirectConnectGatewayAttachments where
  page rq rs
    | stop (rs ^. ddcgasrsNextToken) = Nothing
    | stop (rs ^. ddcgasrsDirectConnectGatewayAttachments) = Nothing
    | otherwise =
      Just $ rq & ddcgasNextToken .~ rs ^. ddcgasrsNextToken

instance AWSRequest DescribeDirectConnectGatewayAttachments where
  type
    Rs DescribeDirectConnectGatewayAttachments =
      DescribeDirectConnectGatewayAttachmentsResponse
  request = postJSON directConnect
  response =
    receiveJSON
      ( \s h x ->
          DescribeDirectConnectGatewayAttachmentsResponse'
            <$> (x .?> "nextToken")
            <*> (x .?> "directConnectGatewayAttachments" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeDirectConnectGatewayAttachments

instance NFData DescribeDirectConnectGatewayAttachments

instance ToHeaders DescribeDirectConnectGatewayAttachments where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "OvertureService.DescribeDirectConnectGatewayAttachments" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeDirectConnectGatewayAttachments where
  toJSON DescribeDirectConnectGatewayAttachments' {..} =
    object
      ( catMaybes
          [ ("directConnectGatewayId" .=) <$> _ddcgasDirectConnectGatewayId,
            ("nextToken" .=) <$> _ddcgasNextToken,
            ("maxResults" .=) <$> _ddcgasMaxResults,
            ("virtualInterfaceId" .=) <$> _ddcgasVirtualInterfaceId
          ]
      )

instance ToPath DescribeDirectConnectGatewayAttachments where
  toPath = const "/"

instance ToQuery DescribeDirectConnectGatewayAttachments where
  toQuery = const mempty

-- | /See:/ 'describeDirectConnectGatewayAttachmentsResponse' smart constructor.
data DescribeDirectConnectGatewayAttachmentsResponse = DescribeDirectConnectGatewayAttachmentsResponse'
  { _ddcgasrsNextToken ::
      !( Maybe
           Text
       ),
    _ddcgasrsDirectConnectGatewayAttachments ::
      !( Maybe
           [DirectConnectGatewayAttachment]
       ),
    _ddcgasrsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DescribeDirectConnectGatewayAttachmentsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddcgasrsNextToken' - The token to retrieve the next page.
--
-- * 'ddcgasrsDirectConnectGatewayAttachments' - The attachments.
--
-- * 'ddcgasrsResponseStatus' - -- | The response status code.
describeDirectConnectGatewayAttachmentsResponse ::
  -- | 'ddcgasrsResponseStatus'
  Int ->
  DescribeDirectConnectGatewayAttachmentsResponse
describeDirectConnectGatewayAttachmentsResponse pResponseStatus_ =
  DescribeDirectConnectGatewayAttachmentsResponse'
    { _ddcgasrsNextToken =
        Nothing,
      _ddcgasrsDirectConnectGatewayAttachments =
        Nothing,
      _ddcgasrsResponseStatus = pResponseStatus_
    }

-- | The token to retrieve the next page.
ddcgasrsNextToken :: Lens' DescribeDirectConnectGatewayAttachmentsResponse (Maybe Text)
ddcgasrsNextToken = lens _ddcgasrsNextToken (\s a -> s {_ddcgasrsNextToken = a})

-- | The attachments.
ddcgasrsDirectConnectGatewayAttachments :: Lens' DescribeDirectConnectGatewayAttachmentsResponse [DirectConnectGatewayAttachment]
ddcgasrsDirectConnectGatewayAttachments = lens _ddcgasrsDirectConnectGatewayAttachments (\s a -> s {_ddcgasrsDirectConnectGatewayAttachments = a}) . _Default . _Coerce

-- | -- | The response status code.
ddcgasrsResponseStatus :: Lens' DescribeDirectConnectGatewayAttachmentsResponse Int
ddcgasrsResponseStatus = lens _ddcgasrsResponseStatus (\s a -> s {_ddcgasrsResponseStatus = a})

instance NFData DescribeDirectConnectGatewayAttachmentsResponse
