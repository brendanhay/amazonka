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
-- Module      : Network.AWS.EC2.RejectTransitGatewayPeeringAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rejects a transit gateway peering attachment request.
module Network.AWS.EC2.RejectTransitGatewayPeeringAttachment
  ( -- * Creating a Request
    rejectTransitGatewayPeeringAttachment,
    RejectTransitGatewayPeeringAttachment,

    -- * Request Lenses
    rtgpaDryRun,
    rtgpaTransitGatewayAttachmentId,

    -- * Destructuring the Response
    rejectTransitGatewayPeeringAttachmentResponse,
    RejectTransitGatewayPeeringAttachmentResponse,

    -- * Response Lenses
    rtgparsTransitGatewayPeeringAttachment,
    rtgparsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'rejectTransitGatewayPeeringAttachment' smart constructor.
data RejectTransitGatewayPeeringAttachment = RejectTransitGatewayPeeringAttachment'
  { _rtgpaDryRun ::
      !(Maybe Bool),
    _rtgpaTransitGatewayAttachmentId ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RejectTransitGatewayPeeringAttachment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtgpaDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'rtgpaTransitGatewayAttachmentId' - The ID of the transit gateway peering attachment.
rejectTransitGatewayPeeringAttachment ::
  -- | 'rtgpaTransitGatewayAttachmentId'
  Text ->
  RejectTransitGatewayPeeringAttachment
rejectTransitGatewayPeeringAttachment pTransitGatewayAttachmentId_ =
  RejectTransitGatewayPeeringAttachment'
    { _rtgpaDryRun = Nothing,
      _rtgpaTransitGatewayAttachmentId =
        pTransitGatewayAttachmentId_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
rtgpaDryRun :: Lens' RejectTransitGatewayPeeringAttachment (Maybe Bool)
rtgpaDryRun = lens _rtgpaDryRun (\s a -> s {_rtgpaDryRun = a})

-- | The ID of the transit gateway peering attachment.
rtgpaTransitGatewayAttachmentId :: Lens' RejectTransitGatewayPeeringAttachment Text
rtgpaTransitGatewayAttachmentId = lens _rtgpaTransitGatewayAttachmentId (\s a -> s {_rtgpaTransitGatewayAttachmentId = a})

instance AWSRequest RejectTransitGatewayPeeringAttachment where
  type
    Rs RejectTransitGatewayPeeringAttachment =
      RejectTransitGatewayPeeringAttachmentResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          RejectTransitGatewayPeeringAttachmentResponse'
            <$> (x .@? "transitGatewayPeeringAttachment") <*> (pure (fromEnum s))
      )

instance Hashable RejectTransitGatewayPeeringAttachment

instance NFData RejectTransitGatewayPeeringAttachment

instance ToHeaders RejectTransitGatewayPeeringAttachment where
  toHeaders = const mempty

instance ToPath RejectTransitGatewayPeeringAttachment where
  toPath = const "/"

instance ToQuery RejectTransitGatewayPeeringAttachment where
  toQuery RejectTransitGatewayPeeringAttachment' {..} =
    mconcat
      [ "Action"
          =: ("RejectTransitGatewayPeeringAttachment" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "DryRun" =: _rtgpaDryRun,
        "TransitGatewayAttachmentId" =: _rtgpaTransitGatewayAttachmentId
      ]

-- | /See:/ 'rejectTransitGatewayPeeringAttachmentResponse' smart constructor.
data RejectTransitGatewayPeeringAttachmentResponse = RejectTransitGatewayPeeringAttachmentResponse'
  { _rtgparsTransitGatewayPeeringAttachment ::
      !( Maybe
           TransitGatewayPeeringAttachment
       ),
    _rtgparsResponseStatus ::
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

-- | Creates a value of 'RejectTransitGatewayPeeringAttachmentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtgparsTransitGatewayPeeringAttachment' - The transit gateway peering attachment.
--
-- * 'rtgparsResponseStatus' - -- | The response status code.
rejectTransitGatewayPeeringAttachmentResponse ::
  -- | 'rtgparsResponseStatus'
  Int ->
  RejectTransitGatewayPeeringAttachmentResponse
rejectTransitGatewayPeeringAttachmentResponse pResponseStatus_ =
  RejectTransitGatewayPeeringAttachmentResponse'
    { _rtgparsTransitGatewayPeeringAttachment =
        Nothing,
      _rtgparsResponseStatus = pResponseStatus_
    }

-- | The transit gateway peering attachment.
rtgparsTransitGatewayPeeringAttachment :: Lens' RejectTransitGatewayPeeringAttachmentResponse (Maybe TransitGatewayPeeringAttachment)
rtgparsTransitGatewayPeeringAttachment = lens _rtgparsTransitGatewayPeeringAttachment (\s a -> s {_rtgparsTransitGatewayPeeringAttachment = a})

-- | -- | The response status code.
rtgparsResponseStatus :: Lens' RejectTransitGatewayPeeringAttachmentResponse Int
rtgparsResponseStatus = lens _rtgparsResponseStatus (\s a -> s {_rtgparsResponseStatus = a})

instance NFData RejectTransitGatewayPeeringAttachmentResponse
