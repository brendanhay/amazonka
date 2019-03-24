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
-- Module      : Network.AWS.EC2.CreateTransitGatewayVPCAttachment
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches the specified VPC to the specified transit gateway.
--
--
-- If you attach a VPC with a CIDR range that overlaps the CIDR range of a VPC that is already attached, the new VPC CIDR range is not propagated to the default propagation route table.
--
-- To send VPC traffic to an attached transit gateway, add a route to the VPC route table using 'CreateRoute' .
--
module Network.AWS.EC2.CreateTransitGatewayVPCAttachment
    (
    -- * Creating a Request
      createTransitGatewayVPCAttachment
    , CreateTransitGatewayVPCAttachment
    -- * Request Lenses
    , ctgvaTagSpecifications
    , ctgvaOptions
    , ctgvaDryRun
    , ctgvaTransitGatewayId
    , ctgvaVPCId
    , ctgvaSubnetIds

    -- * Destructuring the Response
    , createTransitGatewayVPCAttachmentResponse
    , CreateTransitGatewayVPCAttachmentResponse
    -- * Response Lenses
    , ctgvarsTransitGatewayVPCAttachment
    , ctgvarsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createTransitGatewayVPCAttachment' smart constructor.
data CreateTransitGatewayVPCAttachment = CreateTransitGatewayVPCAttachment'
  { _ctgvaTagSpecifications :: !(Maybe [TagSpecification])
  , _ctgvaOptions :: !(Maybe CreateTransitGatewayVPCAttachmentRequestOptions)
  , _ctgvaDryRun :: !(Maybe Bool)
  , _ctgvaTransitGatewayId :: !Text
  , _ctgvaVPCId :: !Text
  , _ctgvaSubnetIds :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateTransitGatewayVPCAttachment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctgvaTagSpecifications' - The tags to apply to the VPC attachment.
--
-- * 'ctgvaOptions' - The VPC attachment options.
--
-- * 'ctgvaDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'ctgvaTransitGatewayId' - The ID of the transit gateway.
--
-- * 'ctgvaVPCId' - The ID of the VPC.
--
-- * 'ctgvaSubnetIds' - The IDs of one or more subnets. You can specify only one subnet per Availability Zone. You must specify at least one subnet, but we recommend that you specify two subnets for better availability. The transit gateway uses one IP address from each specified subnet.
createTransitGatewayVPCAttachment
    :: Text -- ^ 'ctgvaTransitGatewayId'
    -> Text -- ^ 'ctgvaVPCId'
    -> CreateTransitGatewayVPCAttachment
createTransitGatewayVPCAttachment pTransitGatewayId_ pVPCId_ =
  CreateTransitGatewayVPCAttachment'
    { _ctgvaTagSpecifications = Nothing
    , _ctgvaOptions = Nothing
    , _ctgvaDryRun = Nothing
    , _ctgvaTransitGatewayId = pTransitGatewayId_
    , _ctgvaVPCId = pVPCId_
    , _ctgvaSubnetIds = mempty
    }


-- | The tags to apply to the VPC attachment.
ctgvaTagSpecifications :: Lens' CreateTransitGatewayVPCAttachment [TagSpecification]
ctgvaTagSpecifications = lens _ctgvaTagSpecifications (\ s a -> s{_ctgvaTagSpecifications = a}) . _Default . _Coerce

-- | The VPC attachment options.
ctgvaOptions :: Lens' CreateTransitGatewayVPCAttachment (Maybe CreateTransitGatewayVPCAttachmentRequestOptions)
ctgvaOptions = lens _ctgvaOptions (\ s a -> s{_ctgvaOptions = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
ctgvaDryRun :: Lens' CreateTransitGatewayVPCAttachment (Maybe Bool)
ctgvaDryRun = lens _ctgvaDryRun (\ s a -> s{_ctgvaDryRun = a})

-- | The ID of the transit gateway.
ctgvaTransitGatewayId :: Lens' CreateTransitGatewayVPCAttachment Text
ctgvaTransitGatewayId = lens _ctgvaTransitGatewayId (\ s a -> s{_ctgvaTransitGatewayId = a})

-- | The ID of the VPC.
ctgvaVPCId :: Lens' CreateTransitGatewayVPCAttachment Text
ctgvaVPCId = lens _ctgvaVPCId (\ s a -> s{_ctgvaVPCId = a})

-- | The IDs of one or more subnets. You can specify only one subnet per Availability Zone. You must specify at least one subnet, but we recommend that you specify two subnets for better availability. The transit gateway uses one IP address from each specified subnet.
ctgvaSubnetIds :: Lens' CreateTransitGatewayVPCAttachment [Text]
ctgvaSubnetIds = lens _ctgvaSubnetIds (\ s a -> s{_ctgvaSubnetIds = a}) . _Coerce

instance AWSRequest CreateTransitGatewayVPCAttachment
         where
        type Rs CreateTransitGatewayVPCAttachment =
             CreateTransitGatewayVPCAttachmentResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 CreateTransitGatewayVPCAttachmentResponse' <$>
                   (x .@? "transitGatewayVpcAttachment") <*>
                     (pure (fromEnum s)))

instance Hashable CreateTransitGatewayVPCAttachment
         where

instance NFData CreateTransitGatewayVPCAttachment
         where

instance ToHeaders CreateTransitGatewayVPCAttachment
         where
        toHeaders = const mempty

instance ToPath CreateTransitGatewayVPCAttachment
         where
        toPath = const "/"

instance ToQuery CreateTransitGatewayVPCAttachment
         where
        toQuery CreateTransitGatewayVPCAttachment'{..}
          = mconcat
              ["Action" =:
                 ("CreateTransitGatewayVpcAttachment" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery
                 (toQueryList "TagSpecifications" <$>
                    _ctgvaTagSpecifications),
               "Options" =: _ctgvaOptions, "DryRun" =: _ctgvaDryRun,
               "TransitGatewayId" =: _ctgvaTransitGatewayId,
               "VpcId" =: _ctgvaVPCId,
               toQueryList "SubnetIds" _ctgvaSubnetIds]

-- | /See:/ 'createTransitGatewayVPCAttachmentResponse' smart constructor.
data CreateTransitGatewayVPCAttachmentResponse = CreateTransitGatewayVPCAttachmentResponse'
  { _ctgvarsTransitGatewayVPCAttachment :: !(Maybe TransitGatewayVPCAttachment)
  , _ctgvarsResponseStatus              :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateTransitGatewayVPCAttachmentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctgvarsTransitGatewayVPCAttachment' - Information about the VPC attachment.
--
-- * 'ctgvarsResponseStatus' - -- | The response status code.
createTransitGatewayVPCAttachmentResponse
    :: Int -- ^ 'ctgvarsResponseStatus'
    -> CreateTransitGatewayVPCAttachmentResponse
createTransitGatewayVPCAttachmentResponse pResponseStatus_ =
  CreateTransitGatewayVPCAttachmentResponse'
    { _ctgvarsTransitGatewayVPCAttachment = Nothing
    , _ctgvarsResponseStatus = pResponseStatus_
    }


-- | Information about the VPC attachment.
ctgvarsTransitGatewayVPCAttachment :: Lens' CreateTransitGatewayVPCAttachmentResponse (Maybe TransitGatewayVPCAttachment)
ctgvarsTransitGatewayVPCAttachment = lens _ctgvarsTransitGatewayVPCAttachment (\ s a -> s{_ctgvarsTransitGatewayVPCAttachment = a})

-- | -- | The response status code.
ctgvarsResponseStatus :: Lens' CreateTransitGatewayVPCAttachmentResponse Int
ctgvarsResponseStatus = lens _ctgvarsResponseStatus (\ s a -> s{_ctgvarsResponseStatus = a})

instance NFData
           CreateTransitGatewayVPCAttachmentResponse
         where
