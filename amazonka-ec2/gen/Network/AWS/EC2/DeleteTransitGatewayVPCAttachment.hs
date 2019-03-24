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
-- Module      : Network.AWS.EC2.DeleteTransitGatewayVPCAttachment
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified VPC attachment.
--
--
module Network.AWS.EC2.DeleteTransitGatewayVPCAttachment
    (
    -- * Creating a Request
      deleteTransitGatewayVPCAttachment
    , DeleteTransitGatewayVPCAttachment
    -- * Request Lenses
    , dtgvaDryRun
    , dtgvaTransitGatewayAttachmentId

    -- * Destructuring the Response
    , deleteTransitGatewayVPCAttachmentResponse
    , DeleteTransitGatewayVPCAttachmentResponse
    -- * Response Lenses
    , dtgvpcarsTransitGatewayVPCAttachment
    , dtgvpcarsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteTransitGatewayVPCAttachment' smart constructor.
data DeleteTransitGatewayVPCAttachment = DeleteTransitGatewayVPCAttachment'
  { _dtgvaDryRun                     :: !(Maybe Bool)
  , _dtgvaTransitGatewayAttachmentId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteTransitGatewayVPCAttachment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtgvaDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dtgvaTransitGatewayAttachmentId' - The ID of the attachment.
deleteTransitGatewayVPCAttachment
    :: Text -- ^ 'dtgvaTransitGatewayAttachmentId'
    -> DeleteTransitGatewayVPCAttachment
deleteTransitGatewayVPCAttachment pTransitGatewayAttachmentId_ =
  DeleteTransitGatewayVPCAttachment'
    { _dtgvaDryRun = Nothing
    , _dtgvaTransitGatewayAttachmentId = pTransitGatewayAttachmentId_
    }


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dtgvaDryRun :: Lens' DeleteTransitGatewayVPCAttachment (Maybe Bool)
dtgvaDryRun = lens _dtgvaDryRun (\ s a -> s{_dtgvaDryRun = a})

-- | The ID of the attachment.
dtgvaTransitGatewayAttachmentId :: Lens' DeleteTransitGatewayVPCAttachment Text
dtgvaTransitGatewayAttachmentId = lens _dtgvaTransitGatewayAttachmentId (\ s a -> s{_dtgvaTransitGatewayAttachmentId = a})

instance AWSRequest DeleteTransitGatewayVPCAttachment
         where
        type Rs DeleteTransitGatewayVPCAttachment =
             DeleteTransitGatewayVPCAttachmentResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DeleteTransitGatewayVPCAttachmentResponse' <$>
                   (x .@? "transitGatewayVpcAttachment") <*>
                     (pure (fromEnum s)))

instance Hashable DeleteTransitGatewayVPCAttachment
         where

instance NFData DeleteTransitGatewayVPCAttachment
         where

instance ToHeaders DeleteTransitGatewayVPCAttachment
         where
        toHeaders = const mempty

instance ToPath DeleteTransitGatewayVPCAttachment
         where
        toPath = const "/"

instance ToQuery DeleteTransitGatewayVPCAttachment
         where
        toQuery DeleteTransitGatewayVPCAttachment'{..}
          = mconcat
              ["Action" =:
                 ("DeleteTransitGatewayVpcAttachment" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _dtgvaDryRun,
               "TransitGatewayAttachmentId" =:
                 _dtgvaTransitGatewayAttachmentId]

-- | /See:/ 'deleteTransitGatewayVPCAttachmentResponse' smart constructor.
data DeleteTransitGatewayVPCAttachmentResponse = DeleteTransitGatewayVPCAttachmentResponse'
  { _dtgvpcarsTransitGatewayVPCAttachment :: !(Maybe TransitGatewayVPCAttachment)
  , _dtgvpcarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteTransitGatewayVPCAttachmentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtgvpcarsTransitGatewayVPCAttachment' - Information about the deleted VPC attachment.
--
-- * 'dtgvpcarsResponseStatus' - -- | The response status code.
deleteTransitGatewayVPCAttachmentResponse
    :: Int -- ^ 'dtgvpcarsResponseStatus'
    -> DeleteTransitGatewayVPCAttachmentResponse
deleteTransitGatewayVPCAttachmentResponse pResponseStatus_ =
  DeleteTransitGatewayVPCAttachmentResponse'
    { _dtgvpcarsTransitGatewayVPCAttachment = Nothing
    , _dtgvpcarsResponseStatus = pResponseStatus_
    }


-- | Information about the deleted VPC attachment.
dtgvpcarsTransitGatewayVPCAttachment :: Lens' DeleteTransitGatewayVPCAttachmentResponse (Maybe TransitGatewayVPCAttachment)
dtgvpcarsTransitGatewayVPCAttachment = lens _dtgvpcarsTransitGatewayVPCAttachment (\ s a -> s{_dtgvpcarsTransitGatewayVPCAttachment = a})

-- | -- | The response status code.
dtgvpcarsResponseStatus :: Lens' DeleteTransitGatewayVPCAttachmentResponse Int
dtgvpcarsResponseStatus = lens _dtgvpcarsResponseStatus (\ s a -> s{_dtgvpcarsResponseStatus = a})

instance NFData
           DeleteTransitGatewayVPCAttachmentResponse
         where
