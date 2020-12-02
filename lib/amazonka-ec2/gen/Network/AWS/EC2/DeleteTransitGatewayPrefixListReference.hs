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
-- Module      : Network.AWS.EC2.DeleteTransitGatewayPrefixListReference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a reference (route) to a prefix list in a specified transit gateway route table.
module Network.AWS.EC2.DeleteTransitGatewayPrefixListReference
  ( -- * Creating a Request
    deleteTransitGatewayPrefixListReference,
    DeleteTransitGatewayPrefixListReference,

    -- * Request Lenses
    dtgplrDryRun,
    dtgplrTransitGatewayRouteTableId,
    dtgplrPrefixListId,

    -- * Destructuring the Response
    deleteTransitGatewayPrefixListReferenceResponse,
    DeleteTransitGatewayPrefixListReferenceResponse,

    -- * Response Lenses
    dtgplrrsTransitGatewayPrefixListReference,
    dtgplrrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteTransitGatewayPrefixListReference' smart constructor.
data DeleteTransitGatewayPrefixListReference = DeleteTransitGatewayPrefixListReference'
  { _dtgplrDryRun ::
      !( Maybe
           Bool
       ),
    _dtgplrTransitGatewayRouteTableId ::
      !Text,
    _dtgplrPrefixListId ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteTransitGatewayPrefixListReference' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtgplrDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dtgplrTransitGatewayRouteTableId' - The ID of the route table.
--
-- * 'dtgplrPrefixListId' - The ID of the prefix list.
deleteTransitGatewayPrefixListReference ::
  -- | 'dtgplrTransitGatewayRouteTableId'
  Text ->
  -- | 'dtgplrPrefixListId'
  Text ->
  DeleteTransitGatewayPrefixListReference
deleteTransitGatewayPrefixListReference
  pTransitGatewayRouteTableId_
  pPrefixListId_ =
    DeleteTransitGatewayPrefixListReference'
      { _dtgplrDryRun = Nothing,
        _dtgplrTransitGatewayRouteTableId =
          pTransitGatewayRouteTableId_,
        _dtgplrPrefixListId = pPrefixListId_
      }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dtgplrDryRun :: Lens' DeleteTransitGatewayPrefixListReference (Maybe Bool)
dtgplrDryRun = lens _dtgplrDryRun (\s a -> s {_dtgplrDryRun = a})

-- | The ID of the route table.
dtgplrTransitGatewayRouteTableId :: Lens' DeleteTransitGatewayPrefixListReference Text
dtgplrTransitGatewayRouteTableId = lens _dtgplrTransitGatewayRouteTableId (\s a -> s {_dtgplrTransitGatewayRouteTableId = a})

-- | The ID of the prefix list.
dtgplrPrefixListId :: Lens' DeleteTransitGatewayPrefixListReference Text
dtgplrPrefixListId = lens _dtgplrPrefixListId (\s a -> s {_dtgplrPrefixListId = a})

instance AWSRequest DeleteTransitGatewayPrefixListReference where
  type
    Rs DeleteTransitGatewayPrefixListReference =
      DeleteTransitGatewayPrefixListReferenceResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          DeleteTransitGatewayPrefixListReferenceResponse'
            <$> (x .@? "transitGatewayPrefixListReference")
            <*> (pure (fromEnum s))
      )

instance Hashable DeleteTransitGatewayPrefixListReference

instance NFData DeleteTransitGatewayPrefixListReference

instance ToHeaders DeleteTransitGatewayPrefixListReference where
  toHeaders = const mempty

instance ToPath DeleteTransitGatewayPrefixListReference where
  toPath = const "/"

instance ToQuery DeleteTransitGatewayPrefixListReference where
  toQuery DeleteTransitGatewayPrefixListReference' {..} =
    mconcat
      [ "Action"
          =: ("DeleteTransitGatewayPrefixListReference" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "DryRun" =: _dtgplrDryRun,
        "TransitGatewayRouteTableId" =: _dtgplrTransitGatewayRouteTableId,
        "PrefixListId" =: _dtgplrPrefixListId
      ]

-- | /See:/ 'deleteTransitGatewayPrefixListReferenceResponse' smart constructor.
data DeleteTransitGatewayPrefixListReferenceResponse = DeleteTransitGatewayPrefixListReferenceResponse'
  { _dtgplrrsTransitGatewayPrefixListReference ::
      !( Maybe
           TransitGatewayPrefixListReference
       ),
    _dtgplrrsResponseStatus ::
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

-- | Creates a value of 'DeleteTransitGatewayPrefixListReferenceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtgplrrsTransitGatewayPrefixListReference' - Information about the deleted prefix list reference.
--
-- * 'dtgplrrsResponseStatus' - -- | The response status code.
deleteTransitGatewayPrefixListReferenceResponse ::
  -- | 'dtgplrrsResponseStatus'
  Int ->
  DeleteTransitGatewayPrefixListReferenceResponse
deleteTransitGatewayPrefixListReferenceResponse pResponseStatus_ =
  DeleteTransitGatewayPrefixListReferenceResponse'
    { _dtgplrrsTransitGatewayPrefixListReference =
        Nothing,
      _dtgplrrsResponseStatus = pResponseStatus_
    }

-- | Information about the deleted prefix list reference.
dtgplrrsTransitGatewayPrefixListReference :: Lens' DeleteTransitGatewayPrefixListReferenceResponse (Maybe TransitGatewayPrefixListReference)
dtgplrrsTransitGatewayPrefixListReference = lens _dtgplrrsTransitGatewayPrefixListReference (\s a -> s {_dtgplrrsTransitGatewayPrefixListReference = a})

-- | -- | The response status code.
dtgplrrsResponseStatus :: Lens' DeleteTransitGatewayPrefixListReferenceResponse Int
dtgplrrsResponseStatus = lens _dtgplrrsResponseStatus (\s a -> s {_dtgplrrsResponseStatus = a})

instance NFData DeleteTransitGatewayPrefixListReferenceResponse
