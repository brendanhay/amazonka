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
-- Module      : Network.AWS.EC2.AcceptTransitGatewayVPCAttachment
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts a request to attach a VPC to a transit gateway.
--
--
-- The VPC attachment must be in the @pendingAcceptance@ state. Use 'DescribeTransitGatewayVpcAttachments' to view your pending VPC attachment requests. Use 'RejectTransitGatewayVpcAttachment' to reject a VPC attachment request.
--
module Network.AWS.EC2.AcceptTransitGatewayVPCAttachment
    (
    -- * Creating a Request
      acceptTransitGatewayVPCAttachment
    , AcceptTransitGatewayVPCAttachment
    -- * Request Lenses
    , atgvaDryRun
    , atgvaTransitGatewayAttachmentId

    -- * Destructuring the Response
    , acceptTransitGatewayVPCAttachmentResponse
    , AcceptTransitGatewayVPCAttachmentResponse
    -- * Response Lenses
    , atgvarsTransitGatewayVPCAttachment
    , atgvarsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'acceptTransitGatewayVPCAttachment' smart constructor.
data AcceptTransitGatewayVPCAttachment = AcceptTransitGatewayVPCAttachment'
  { _atgvaDryRun                     :: !(Maybe Bool)
  , _atgvaTransitGatewayAttachmentId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AcceptTransitGatewayVPCAttachment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atgvaDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'atgvaTransitGatewayAttachmentId' - The ID of the attachment.
acceptTransitGatewayVPCAttachment
    :: Text -- ^ 'atgvaTransitGatewayAttachmentId'
    -> AcceptTransitGatewayVPCAttachment
acceptTransitGatewayVPCAttachment pTransitGatewayAttachmentId_ =
  AcceptTransitGatewayVPCAttachment'
    { _atgvaDryRun = Nothing
    , _atgvaTransitGatewayAttachmentId = pTransitGatewayAttachmentId_
    }


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
atgvaDryRun :: Lens' AcceptTransitGatewayVPCAttachment (Maybe Bool)
atgvaDryRun = lens _atgvaDryRun (\ s a -> s{_atgvaDryRun = a})

-- | The ID of the attachment.
atgvaTransitGatewayAttachmentId :: Lens' AcceptTransitGatewayVPCAttachment Text
atgvaTransitGatewayAttachmentId = lens _atgvaTransitGatewayAttachmentId (\ s a -> s{_atgvaTransitGatewayAttachmentId = a})

instance AWSRequest AcceptTransitGatewayVPCAttachment
         where
        type Rs AcceptTransitGatewayVPCAttachment =
             AcceptTransitGatewayVPCAttachmentResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 AcceptTransitGatewayVPCAttachmentResponse' <$>
                   (x .@? "transitGatewayVpcAttachment") <*>
                     (pure (fromEnum s)))

instance Hashable AcceptTransitGatewayVPCAttachment
         where

instance NFData AcceptTransitGatewayVPCAttachment
         where

instance ToHeaders AcceptTransitGatewayVPCAttachment
         where
        toHeaders = const mempty

instance ToPath AcceptTransitGatewayVPCAttachment
         where
        toPath = const "/"

instance ToQuery AcceptTransitGatewayVPCAttachment
         where
        toQuery AcceptTransitGatewayVPCAttachment'{..}
          = mconcat
              ["Action" =:
                 ("AcceptTransitGatewayVpcAttachment" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _atgvaDryRun,
               "TransitGatewayAttachmentId" =:
                 _atgvaTransitGatewayAttachmentId]

-- | /See:/ 'acceptTransitGatewayVPCAttachmentResponse' smart constructor.
data AcceptTransitGatewayVPCAttachmentResponse = AcceptTransitGatewayVPCAttachmentResponse'
  { _atgvarsTransitGatewayVPCAttachment :: !(Maybe TransitGatewayVPCAttachment)
  , _atgvarsResponseStatus              :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AcceptTransitGatewayVPCAttachmentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atgvarsTransitGatewayVPCAttachment' - The VPC attachment.
--
-- * 'atgvarsResponseStatus' - -- | The response status code.
acceptTransitGatewayVPCAttachmentResponse
    :: Int -- ^ 'atgvarsResponseStatus'
    -> AcceptTransitGatewayVPCAttachmentResponse
acceptTransitGatewayVPCAttachmentResponse pResponseStatus_ =
  AcceptTransitGatewayVPCAttachmentResponse'
    { _atgvarsTransitGatewayVPCAttachment = Nothing
    , _atgvarsResponseStatus = pResponseStatus_
    }


-- | The VPC attachment.
atgvarsTransitGatewayVPCAttachment :: Lens' AcceptTransitGatewayVPCAttachmentResponse (Maybe TransitGatewayVPCAttachment)
atgvarsTransitGatewayVPCAttachment = lens _atgvarsTransitGatewayVPCAttachment (\ s a -> s{_atgvarsTransitGatewayVPCAttachment = a})

-- | -- | The response status code.
atgvarsResponseStatus :: Lens' AcceptTransitGatewayVPCAttachmentResponse Int
atgvarsResponseStatus = lens _atgvarsResponseStatus (\ s a -> s{_atgvarsResponseStatus = a})

instance NFData
           AcceptTransitGatewayVPCAttachmentResponse
         where
