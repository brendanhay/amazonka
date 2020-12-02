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
-- Module      : Network.AWS.EC2.DeleteTransitGatewayPeeringAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a transit gateway peering attachment.
module Network.AWS.EC2.DeleteTransitGatewayPeeringAttachment
  ( -- * Creating a Request
    deleteTransitGatewayPeeringAttachment,
    DeleteTransitGatewayPeeringAttachment,

    -- * Request Lenses
    dtgpatDryRun,
    dtgpatTransitGatewayAttachmentId,

    -- * Destructuring the Response
    deleteTransitGatewayPeeringAttachmentResponse,
    DeleteTransitGatewayPeeringAttachmentResponse,

    -- * Response Lenses
    dtgparsTransitGatewayPeeringAttachment,
    dtgparsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteTransitGatewayPeeringAttachment' smart constructor.
data DeleteTransitGatewayPeeringAttachment = DeleteTransitGatewayPeeringAttachment'
  { _dtgpatDryRun ::
      !(Maybe Bool),
    _dtgpatTransitGatewayAttachmentId ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteTransitGatewayPeeringAttachment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtgpatDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dtgpatTransitGatewayAttachmentId' - The ID of the transit gateway peering attachment.
deleteTransitGatewayPeeringAttachment ::
  -- | 'dtgpatTransitGatewayAttachmentId'
  Text ->
  DeleteTransitGatewayPeeringAttachment
deleteTransitGatewayPeeringAttachment pTransitGatewayAttachmentId_ =
  DeleteTransitGatewayPeeringAttachment'
    { _dtgpatDryRun = Nothing,
      _dtgpatTransitGatewayAttachmentId =
        pTransitGatewayAttachmentId_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dtgpatDryRun :: Lens' DeleteTransitGatewayPeeringAttachment (Maybe Bool)
dtgpatDryRun = lens _dtgpatDryRun (\s a -> s {_dtgpatDryRun = a})

-- | The ID of the transit gateway peering attachment.
dtgpatTransitGatewayAttachmentId :: Lens' DeleteTransitGatewayPeeringAttachment Text
dtgpatTransitGatewayAttachmentId = lens _dtgpatTransitGatewayAttachmentId (\s a -> s {_dtgpatTransitGatewayAttachmentId = a})

instance AWSRequest DeleteTransitGatewayPeeringAttachment where
  type
    Rs DeleteTransitGatewayPeeringAttachment =
      DeleteTransitGatewayPeeringAttachmentResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          DeleteTransitGatewayPeeringAttachmentResponse'
            <$> (x .@? "transitGatewayPeeringAttachment") <*> (pure (fromEnum s))
      )

instance Hashable DeleteTransitGatewayPeeringAttachment

instance NFData DeleteTransitGatewayPeeringAttachment

instance ToHeaders DeleteTransitGatewayPeeringAttachment where
  toHeaders = const mempty

instance ToPath DeleteTransitGatewayPeeringAttachment where
  toPath = const "/"

instance ToQuery DeleteTransitGatewayPeeringAttachment where
  toQuery DeleteTransitGatewayPeeringAttachment' {..} =
    mconcat
      [ "Action"
          =: ("DeleteTransitGatewayPeeringAttachment" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "DryRun" =: _dtgpatDryRun,
        "TransitGatewayAttachmentId" =: _dtgpatTransitGatewayAttachmentId
      ]

-- | /See:/ 'deleteTransitGatewayPeeringAttachmentResponse' smart constructor.
data DeleteTransitGatewayPeeringAttachmentResponse = DeleteTransitGatewayPeeringAttachmentResponse'
  { _dtgparsTransitGatewayPeeringAttachment ::
      !( Maybe
           TransitGatewayPeeringAttachment
       ),
    _dtgparsResponseStatus ::
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

-- | Creates a value of 'DeleteTransitGatewayPeeringAttachmentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtgparsTransitGatewayPeeringAttachment' - The transit gateway peering attachment.
--
-- * 'dtgparsResponseStatus' - -- | The response status code.
deleteTransitGatewayPeeringAttachmentResponse ::
  -- | 'dtgparsResponseStatus'
  Int ->
  DeleteTransitGatewayPeeringAttachmentResponse
deleteTransitGatewayPeeringAttachmentResponse pResponseStatus_ =
  DeleteTransitGatewayPeeringAttachmentResponse'
    { _dtgparsTransitGatewayPeeringAttachment =
        Nothing,
      _dtgparsResponseStatus = pResponseStatus_
    }

-- | The transit gateway peering attachment.
dtgparsTransitGatewayPeeringAttachment :: Lens' DeleteTransitGatewayPeeringAttachmentResponse (Maybe TransitGatewayPeeringAttachment)
dtgparsTransitGatewayPeeringAttachment = lens _dtgparsTransitGatewayPeeringAttachment (\s a -> s {_dtgparsTransitGatewayPeeringAttachment = a})

-- | -- | The response status code.
dtgparsResponseStatus :: Lens' DeleteTransitGatewayPeeringAttachmentResponse Int
dtgparsResponseStatus = lens _dtgparsResponseStatus (\s a -> s {_dtgparsResponseStatus = a})

instance NFData DeleteTransitGatewayPeeringAttachmentResponse
