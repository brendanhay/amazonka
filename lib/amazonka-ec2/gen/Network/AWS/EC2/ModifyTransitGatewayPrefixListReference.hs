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
-- Module      : Network.AWS.EC2.ModifyTransitGatewayPrefixListReference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a reference (route) to a prefix list in a specified transit gateway route table.
module Network.AWS.EC2.ModifyTransitGatewayPrefixListReference
  ( -- * Creating a Request
    modifyTransitGatewayPrefixListReference,
    ModifyTransitGatewayPrefixListReference,

    -- * Request Lenses
    mtgplrBlackhole,
    mtgplrTransitGatewayAttachmentId,
    mtgplrDryRun,
    mtgplrTransitGatewayRouteTableId,
    mtgplrPrefixListId,

    -- * Destructuring the Response
    modifyTransitGatewayPrefixListReferenceResponse,
    ModifyTransitGatewayPrefixListReferenceResponse,

    -- * Response Lenses
    mtgplrrsTransitGatewayPrefixListReference,
    mtgplrrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'modifyTransitGatewayPrefixListReference' smart constructor.
data ModifyTransitGatewayPrefixListReference = ModifyTransitGatewayPrefixListReference'
  { _mtgplrBlackhole ::
      !( Maybe
           Bool
       ),
    _mtgplrTransitGatewayAttachmentId ::
      !( Maybe
           Text
       ),
    _mtgplrDryRun ::
      !( Maybe
           Bool
       ),
    _mtgplrTransitGatewayRouteTableId ::
      !Text,
    _mtgplrPrefixListId ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModifyTransitGatewayPrefixListReference' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mtgplrBlackhole' - Indicates whether to drop traffic that matches this route.
--
-- * 'mtgplrTransitGatewayAttachmentId' - The ID of the attachment to which traffic is routed.
--
-- * 'mtgplrDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'mtgplrTransitGatewayRouteTableId' - The ID of the transit gateway route table.
--
-- * 'mtgplrPrefixListId' - The ID of the prefix list.
modifyTransitGatewayPrefixListReference ::
  -- | 'mtgplrTransitGatewayRouteTableId'
  Text ->
  -- | 'mtgplrPrefixListId'
  Text ->
  ModifyTransitGatewayPrefixListReference
modifyTransitGatewayPrefixListReference
  pTransitGatewayRouteTableId_
  pPrefixListId_ =
    ModifyTransitGatewayPrefixListReference'
      { _mtgplrBlackhole =
          Nothing,
        _mtgplrTransitGatewayAttachmentId = Nothing,
        _mtgplrDryRun = Nothing,
        _mtgplrTransitGatewayRouteTableId =
          pTransitGatewayRouteTableId_,
        _mtgplrPrefixListId = pPrefixListId_
      }

-- | Indicates whether to drop traffic that matches this route.
mtgplrBlackhole :: Lens' ModifyTransitGatewayPrefixListReference (Maybe Bool)
mtgplrBlackhole = lens _mtgplrBlackhole (\s a -> s {_mtgplrBlackhole = a})

-- | The ID of the attachment to which traffic is routed.
mtgplrTransitGatewayAttachmentId :: Lens' ModifyTransitGatewayPrefixListReference (Maybe Text)
mtgplrTransitGatewayAttachmentId = lens _mtgplrTransitGatewayAttachmentId (\s a -> s {_mtgplrTransitGatewayAttachmentId = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mtgplrDryRun :: Lens' ModifyTransitGatewayPrefixListReference (Maybe Bool)
mtgplrDryRun = lens _mtgplrDryRun (\s a -> s {_mtgplrDryRun = a})

-- | The ID of the transit gateway route table.
mtgplrTransitGatewayRouteTableId :: Lens' ModifyTransitGatewayPrefixListReference Text
mtgplrTransitGatewayRouteTableId = lens _mtgplrTransitGatewayRouteTableId (\s a -> s {_mtgplrTransitGatewayRouteTableId = a})

-- | The ID of the prefix list.
mtgplrPrefixListId :: Lens' ModifyTransitGatewayPrefixListReference Text
mtgplrPrefixListId = lens _mtgplrPrefixListId (\s a -> s {_mtgplrPrefixListId = a})

instance AWSRequest ModifyTransitGatewayPrefixListReference where
  type
    Rs ModifyTransitGatewayPrefixListReference =
      ModifyTransitGatewayPrefixListReferenceResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          ModifyTransitGatewayPrefixListReferenceResponse'
            <$> (x .@? "transitGatewayPrefixListReference")
            <*> (pure (fromEnum s))
      )

instance Hashable ModifyTransitGatewayPrefixListReference

instance NFData ModifyTransitGatewayPrefixListReference

instance ToHeaders ModifyTransitGatewayPrefixListReference where
  toHeaders = const mempty

instance ToPath ModifyTransitGatewayPrefixListReference where
  toPath = const "/"

instance ToQuery ModifyTransitGatewayPrefixListReference where
  toQuery ModifyTransitGatewayPrefixListReference' {..} =
    mconcat
      [ "Action"
          =: ("ModifyTransitGatewayPrefixListReference" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "Blackhole" =: _mtgplrBlackhole,
        "TransitGatewayAttachmentId" =: _mtgplrTransitGatewayAttachmentId,
        "DryRun" =: _mtgplrDryRun,
        "TransitGatewayRouteTableId" =: _mtgplrTransitGatewayRouteTableId,
        "PrefixListId" =: _mtgplrPrefixListId
      ]

-- | /See:/ 'modifyTransitGatewayPrefixListReferenceResponse' smart constructor.
data ModifyTransitGatewayPrefixListReferenceResponse = ModifyTransitGatewayPrefixListReferenceResponse'
  { _mtgplrrsTransitGatewayPrefixListReference ::
      !( Maybe
           TransitGatewayPrefixListReference
       ),
    _mtgplrrsResponseStatus ::
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

-- | Creates a value of 'ModifyTransitGatewayPrefixListReferenceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mtgplrrsTransitGatewayPrefixListReference' - Information about the prefix list reference.
--
-- * 'mtgplrrsResponseStatus' - -- | The response status code.
modifyTransitGatewayPrefixListReferenceResponse ::
  -- | 'mtgplrrsResponseStatus'
  Int ->
  ModifyTransitGatewayPrefixListReferenceResponse
modifyTransitGatewayPrefixListReferenceResponse pResponseStatus_ =
  ModifyTransitGatewayPrefixListReferenceResponse'
    { _mtgplrrsTransitGatewayPrefixListReference =
        Nothing,
      _mtgplrrsResponseStatus = pResponseStatus_
    }

-- | Information about the prefix list reference.
mtgplrrsTransitGatewayPrefixListReference :: Lens' ModifyTransitGatewayPrefixListReferenceResponse (Maybe TransitGatewayPrefixListReference)
mtgplrrsTransitGatewayPrefixListReference = lens _mtgplrrsTransitGatewayPrefixListReference (\s a -> s {_mtgplrrsTransitGatewayPrefixListReference = a})

-- | -- | The response status code.
mtgplrrsResponseStatus :: Lens' ModifyTransitGatewayPrefixListReferenceResponse Int
mtgplrrsResponseStatus = lens _mtgplrrsResponseStatus (\s a -> s {_mtgplrrsResponseStatus = a})

instance NFData ModifyTransitGatewayPrefixListReferenceResponse
