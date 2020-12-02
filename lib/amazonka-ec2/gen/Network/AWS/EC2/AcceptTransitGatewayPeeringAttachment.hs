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
-- Module      : Network.AWS.EC2.AcceptTransitGatewayPeeringAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts a transit gateway peering attachment request. The peering attachment must be in the @pendingAcceptance@ state.
module Network.AWS.EC2.AcceptTransitGatewayPeeringAttachment
  ( -- * Creating a Request
    acceptTransitGatewayPeeringAttachment,
    AcceptTransitGatewayPeeringAttachment,

    -- * Request Lenses
    atgpaDryRun,
    atgpaTransitGatewayAttachmentId,

    -- * Destructuring the Response
    acceptTransitGatewayPeeringAttachmentResponse,
    AcceptTransitGatewayPeeringAttachmentResponse,

    -- * Response Lenses
    atgparsTransitGatewayPeeringAttachment,
    atgparsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'acceptTransitGatewayPeeringAttachment' smart constructor.
data AcceptTransitGatewayPeeringAttachment = AcceptTransitGatewayPeeringAttachment'
  { _atgpaDryRun ::
      !(Maybe Bool),
    _atgpaTransitGatewayAttachmentId ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AcceptTransitGatewayPeeringAttachment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atgpaDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'atgpaTransitGatewayAttachmentId' - The ID of the transit gateway attachment.
acceptTransitGatewayPeeringAttachment ::
  -- | 'atgpaTransitGatewayAttachmentId'
  Text ->
  AcceptTransitGatewayPeeringAttachment
acceptTransitGatewayPeeringAttachment pTransitGatewayAttachmentId_ =
  AcceptTransitGatewayPeeringAttachment'
    { _atgpaDryRun = Nothing,
      _atgpaTransitGatewayAttachmentId =
        pTransitGatewayAttachmentId_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
atgpaDryRun :: Lens' AcceptTransitGatewayPeeringAttachment (Maybe Bool)
atgpaDryRun = lens _atgpaDryRun (\s a -> s {_atgpaDryRun = a})

-- | The ID of the transit gateway attachment.
atgpaTransitGatewayAttachmentId :: Lens' AcceptTransitGatewayPeeringAttachment Text
atgpaTransitGatewayAttachmentId = lens _atgpaTransitGatewayAttachmentId (\s a -> s {_atgpaTransitGatewayAttachmentId = a})

instance AWSRequest AcceptTransitGatewayPeeringAttachment where
  type
    Rs AcceptTransitGatewayPeeringAttachment =
      AcceptTransitGatewayPeeringAttachmentResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          AcceptTransitGatewayPeeringAttachmentResponse'
            <$> (x .@? "transitGatewayPeeringAttachment") <*> (pure (fromEnum s))
      )

instance Hashable AcceptTransitGatewayPeeringAttachment

instance NFData AcceptTransitGatewayPeeringAttachment

instance ToHeaders AcceptTransitGatewayPeeringAttachment where
  toHeaders = const mempty

instance ToPath AcceptTransitGatewayPeeringAttachment where
  toPath = const "/"

instance ToQuery AcceptTransitGatewayPeeringAttachment where
  toQuery AcceptTransitGatewayPeeringAttachment' {..} =
    mconcat
      [ "Action"
          =: ("AcceptTransitGatewayPeeringAttachment" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "DryRun" =: _atgpaDryRun,
        "TransitGatewayAttachmentId" =: _atgpaTransitGatewayAttachmentId
      ]

-- | /See:/ 'acceptTransitGatewayPeeringAttachmentResponse' smart constructor.
data AcceptTransitGatewayPeeringAttachmentResponse = AcceptTransitGatewayPeeringAttachmentResponse'
  { _atgparsTransitGatewayPeeringAttachment ::
      !( Maybe
           TransitGatewayPeeringAttachment
       ),
    _atgparsResponseStatus ::
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

-- | Creates a value of 'AcceptTransitGatewayPeeringAttachmentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atgparsTransitGatewayPeeringAttachment' - The transit gateway peering attachment.
--
-- * 'atgparsResponseStatus' - -- | The response status code.
acceptTransitGatewayPeeringAttachmentResponse ::
  -- | 'atgparsResponseStatus'
  Int ->
  AcceptTransitGatewayPeeringAttachmentResponse
acceptTransitGatewayPeeringAttachmentResponse pResponseStatus_ =
  AcceptTransitGatewayPeeringAttachmentResponse'
    { _atgparsTransitGatewayPeeringAttachment =
        Nothing,
      _atgparsResponseStatus = pResponseStatus_
    }

-- | The transit gateway peering attachment.
atgparsTransitGatewayPeeringAttachment :: Lens' AcceptTransitGatewayPeeringAttachmentResponse (Maybe TransitGatewayPeeringAttachment)
atgparsTransitGatewayPeeringAttachment = lens _atgparsTransitGatewayPeeringAttachment (\s a -> s {_atgparsTransitGatewayPeeringAttachment = a})

-- | -- | The response status code.
atgparsResponseStatus :: Lens' AcceptTransitGatewayPeeringAttachmentResponse Int
atgparsResponseStatus = lens _atgparsResponseStatus (\s a -> s {_atgparsResponseStatus = a})

instance NFData AcceptTransitGatewayPeeringAttachmentResponse
