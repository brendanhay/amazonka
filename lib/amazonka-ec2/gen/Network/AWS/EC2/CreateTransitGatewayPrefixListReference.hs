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
-- Module      : Network.AWS.EC2.CreateTransitGatewayPrefixListReference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a reference (route) to a prefix list in a specified transit gateway route table.
module Network.AWS.EC2.CreateTransitGatewayPrefixListReference
  ( -- * Creating a Request
    createTransitGatewayPrefixListReference,
    CreateTransitGatewayPrefixListReference,

    -- * Request Lenses
    ctgplrBlackhole,
    ctgplrTransitGatewayAttachmentId,
    ctgplrDryRun,
    ctgplrTransitGatewayRouteTableId,
    ctgplrPrefixListId,

    -- * Destructuring the Response
    createTransitGatewayPrefixListReferenceResponse,
    CreateTransitGatewayPrefixListReferenceResponse,

    -- * Response Lenses
    ctgplrrsTransitGatewayPrefixListReference,
    ctgplrrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createTransitGatewayPrefixListReference' smart constructor.
data CreateTransitGatewayPrefixListReference = CreateTransitGatewayPrefixListReference'
  { _ctgplrBlackhole ::
      !( Maybe
           Bool
       ),
    _ctgplrTransitGatewayAttachmentId ::
      !( Maybe
           Text
       ),
    _ctgplrDryRun ::
      !( Maybe
           Bool
       ),
    _ctgplrTransitGatewayRouteTableId ::
      !Text,
    _ctgplrPrefixListId ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateTransitGatewayPrefixListReference' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctgplrBlackhole' - Indicates whether to drop traffic that matches this route.
--
-- * 'ctgplrTransitGatewayAttachmentId' - The ID of the attachment to which traffic is routed.
--
-- * 'ctgplrDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'ctgplrTransitGatewayRouteTableId' - The ID of the transit gateway route table.
--
-- * 'ctgplrPrefixListId' - The ID of the prefix list that is used for destination matches.
createTransitGatewayPrefixListReference ::
  -- | 'ctgplrTransitGatewayRouteTableId'
  Text ->
  -- | 'ctgplrPrefixListId'
  Text ->
  CreateTransitGatewayPrefixListReference
createTransitGatewayPrefixListReference
  pTransitGatewayRouteTableId_
  pPrefixListId_ =
    CreateTransitGatewayPrefixListReference'
      { _ctgplrBlackhole =
          Nothing,
        _ctgplrTransitGatewayAttachmentId = Nothing,
        _ctgplrDryRun = Nothing,
        _ctgplrTransitGatewayRouteTableId =
          pTransitGatewayRouteTableId_,
        _ctgplrPrefixListId = pPrefixListId_
      }

-- | Indicates whether to drop traffic that matches this route.
ctgplrBlackhole :: Lens' CreateTransitGatewayPrefixListReference (Maybe Bool)
ctgplrBlackhole = lens _ctgplrBlackhole (\s a -> s {_ctgplrBlackhole = a})

-- | The ID of the attachment to which traffic is routed.
ctgplrTransitGatewayAttachmentId :: Lens' CreateTransitGatewayPrefixListReference (Maybe Text)
ctgplrTransitGatewayAttachmentId = lens _ctgplrTransitGatewayAttachmentId (\s a -> s {_ctgplrTransitGatewayAttachmentId = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
ctgplrDryRun :: Lens' CreateTransitGatewayPrefixListReference (Maybe Bool)
ctgplrDryRun = lens _ctgplrDryRun (\s a -> s {_ctgplrDryRun = a})

-- | The ID of the transit gateway route table.
ctgplrTransitGatewayRouteTableId :: Lens' CreateTransitGatewayPrefixListReference Text
ctgplrTransitGatewayRouteTableId = lens _ctgplrTransitGatewayRouteTableId (\s a -> s {_ctgplrTransitGatewayRouteTableId = a})

-- | The ID of the prefix list that is used for destination matches.
ctgplrPrefixListId :: Lens' CreateTransitGatewayPrefixListReference Text
ctgplrPrefixListId = lens _ctgplrPrefixListId (\s a -> s {_ctgplrPrefixListId = a})

instance AWSRequest CreateTransitGatewayPrefixListReference where
  type
    Rs CreateTransitGatewayPrefixListReference =
      CreateTransitGatewayPrefixListReferenceResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          CreateTransitGatewayPrefixListReferenceResponse'
            <$> (x .@? "transitGatewayPrefixListReference")
            <*> (pure (fromEnum s))
      )

instance Hashable CreateTransitGatewayPrefixListReference

instance NFData CreateTransitGatewayPrefixListReference

instance ToHeaders CreateTransitGatewayPrefixListReference where
  toHeaders = const mempty

instance ToPath CreateTransitGatewayPrefixListReference where
  toPath = const "/"

instance ToQuery CreateTransitGatewayPrefixListReference where
  toQuery CreateTransitGatewayPrefixListReference' {..} =
    mconcat
      [ "Action"
          =: ("CreateTransitGatewayPrefixListReference" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "Blackhole" =: _ctgplrBlackhole,
        "TransitGatewayAttachmentId" =: _ctgplrTransitGatewayAttachmentId,
        "DryRun" =: _ctgplrDryRun,
        "TransitGatewayRouteTableId" =: _ctgplrTransitGatewayRouteTableId,
        "PrefixListId" =: _ctgplrPrefixListId
      ]

-- | /See:/ 'createTransitGatewayPrefixListReferenceResponse' smart constructor.
data CreateTransitGatewayPrefixListReferenceResponse = CreateTransitGatewayPrefixListReferenceResponse'
  { _ctgplrrsTransitGatewayPrefixListReference ::
      !( Maybe
           TransitGatewayPrefixListReference
       ),
    _ctgplrrsResponseStatus ::
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

-- | Creates a value of 'CreateTransitGatewayPrefixListReferenceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctgplrrsTransitGatewayPrefixListReference' - Information about the prefix list reference.
--
-- * 'ctgplrrsResponseStatus' - -- | The response status code.
createTransitGatewayPrefixListReferenceResponse ::
  -- | 'ctgplrrsResponseStatus'
  Int ->
  CreateTransitGatewayPrefixListReferenceResponse
createTransitGatewayPrefixListReferenceResponse pResponseStatus_ =
  CreateTransitGatewayPrefixListReferenceResponse'
    { _ctgplrrsTransitGatewayPrefixListReference =
        Nothing,
      _ctgplrrsResponseStatus = pResponseStatus_
    }

-- | Information about the prefix list reference.
ctgplrrsTransitGatewayPrefixListReference :: Lens' CreateTransitGatewayPrefixListReferenceResponse (Maybe TransitGatewayPrefixListReference)
ctgplrrsTransitGatewayPrefixListReference = lens _ctgplrrsTransitGatewayPrefixListReference (\s a -> s {_ctgplrrsTransitGatewayPrefixListReference = a})

-- | -- | The response status code.
ctgplrrsResponseStatus :: Lens' CreateTransitGatewayPrefixListReferenceResponse Int
ctgplrrsResponseStatus = lens _ctgplrrsResponseStatus (\s a -> s {_ctgplrrsResponseStatus = a})

instance NFData CreateTransitGatewayPrefixListReferenceResponse
