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
-- Module      : Network.AWS.EC2.AssociateVPCCidrBlock
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a CIDR block with your VPC. You can associate a secondary IPv4 CIDR block, or you can associate an Amazon-provided IPv6 CIDR block. The IPv6 CIDR block size is fixed at /56.
--
--
-- For more information about associating CIDR blocks with your VPC and applicable restrictions, see <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Subnets.html#VPC_Sizing VPC and Subnet Sizing> in the /Amazon Virtual Private Cloud User Guide/ .
--
module Network.AWS.EC2.AssociateVPCCidrBlock
    (
    -- * Creating a Request
      associateVPCCidrBlock
    , AssociateVPCCidrBlock
    -- * Request Lenses
    , avcbCidrBlock
    , avcbAmazonProvidedIPv6CidrBlock
    , avcbVPCId

    -- * Destructuring the Response
    , associateVPCCidrBlockResponse
    , AssociateVPCCidrBlockResponse
    -- * Response Lenses
    , avcbrsVPCId
    , avcbrsCidrBlockAssociation
    , avcbrsIPv6CidrBlockAssociation
    , avcbrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'associateVPCCidrBlock' smart constructor.
data AssociateVPCCidrBlock = AssociateVPCCidrBlock'
  { _avcbCidrBlock                   :: !(Maybe Text)
  , _avcbAmazonProvidedIPv6CidrBlock :: !(Maybe Bool)
  , _avcbVPCId                       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateVPCCidrBlock' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avcbCidrBlock' - An IPv4 CIDR block to associate with the VPC.
--
-- * 'avcbAmazonProvidedIPv6CidrBlock' - Requests an Amazon-provided IPv6 CIDR block with a /56 prefix length for the VPC. You cannot specify the range of IPv6 addresses, or the size of the CIDR block.
--
-- * 'avcbVPCId' - The ID of the VPC.
associateVPCCidrBlock
    :: Text -- ^ 'avcbVPCId'
    -> AssociateVPCCidrBlock
associateVPCCidrBlock pVPCId_ =
  AssociateVPCCidrBlock'
    { _avcbCidrBlock = Nothing
    , _avcbAmazonProvidedIPv6CidrBlock = Nothing
    , _avcbVPCId = pVPCId_
    }


-- | An IPv4 CIDR block to associate with the VPC.
avcbCidrBlock :: Lens' AssociateVPCCidrBlock (Maybe Text)
avcbCidrBlock = lens _avcbCidrBlock (\ s a -> s{_avcbCidrBlock = a})

-- | Requests an Amazon-provided IPv6 CIDR block with a /56 prefix length for the VPC. You cannot specify the range of IPv6 addresses, or the size of the CIDR block.
avcbAmazonProvidedIPv6CidrBlock :: Lens' AssociateVPCCidrBlock (Maybe Bool)
avcbAmazonProvidedIPv6CidrBlock = lens _avcbAmazonProvidedIPv6CidrBlock (\ s a -> s{_avcbAmazonProvidedIPv6CidrBlock = a})

-- | The ID of the VPC.
avcbVPCId :: Lens' AssociateVPCCidrBlock Text
avcbVPCId = lens _avcbVPCId (\ s a -> s{_avcbVPCId = a})

instance AWSRequest AssociateVPCCidrBlock where
        type Rs AssociateVPCCidrBlock =
             AssociateVPCCidrBlockResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 AssociateVPCCidrBlockResponse' <$>
                   (x .@? "vpcId") <*> (x .@? "cidrBlockAssociation")
                     <*> (x .@? "ipv6CidrBlockAssociation")
                     <*> (pure (fromEnum s)))

instance Hashable AssociateVPCCidrBlock where

instance NFData AssociateVPCCidrBlock where

instance ToHeaders AssociateVPCCidrBlock where
        toHeaders = const mempty

instance ToPath AssociateVPCCidrBlock where
        toPath = const "/"

instance ToQuery AssociateVPCCidrBlock where
        toQuery AssociateVPCCidrBlock'{..}
          = mconcat
              ["Action" =: ("AssociateVpcCidrBlock" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "CidrBlock" =: _avcbCidrBlock,
               "AmazonProvidedIpv6CidrBlock" =:
                 _avcbAmazonProvidedIPv6CidrBlock,
               "VpcId" =: _avcbVPCId]

-- | /See:/ 'associateVPCCidrBlockResponse' smart constructor.
data AssociateVPCCidrBlockResponse = AssociateVPCCidrBlockResponse'
  { _avcbrsVPCId                    :: !(Maybe Text)
  , _avcbrsCidrBlockAssociation     :: !(Maybe VPCCidrBlockAssociation)
  , _avcbrsIPv6CidrBlockAssociation :: !(Maybe VPCIPv6CidrBlockAssociation)
  , _avcbrsResponseStatus           :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateVPCCidrBlockResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avcbrsVPCId' - The ID of the VPC.
--
-- * 'avcbrsCidrBlockAssociation' - Information about the IPv4 CIDR block association.
--
-- * 'avcbrsIPv6CidrBlockAssociation' - Information about the IPv6 CIDR block association.
--
-- * 'avcbrsResponseStatus' - -- | The response status code.
associateVPCCidrBlockResponse
    :: Int -- ^ 'avcbrsResponseStatus'
    -> AssociateVPCCidrBlockResponse
associateVPCCidrBlockResponse pResponseStatus_ =
  AssociateVPCCidrBlockResponse'
    { _avcbrsVPCId = Nothing
    , _avcbrsCidrBlockAssociation = Nothing
    , _avcbrsIPv6CidrBlockAssociation = Nothing
    , _avcbrsResponseStatus = pResponseStatus_
    }


-- | The ID of the VPC.
avcbrsVPCId :: Lens' AssociateVPCCidrBlockResponse (Maybe Text)
avcbrsVPCId = lens _avcbrsVPCId (\ s a -> s{_avcbrsVPCId = a})

-- | Information about the IPv4 CIDR block association.
avcbrsCidrBlockAssociation :: Lens' AssociateVPCCidrBlockResponse (Maybe VPCCidrBlockAssociation)
avcbrsCidrBlockAssociation = lens _avcbrsCidrBlockAssociation (\ s a -> s{_avcbrsCidrBlockAssociation = a})

-- | Information about the IPv6 CIDR block association.
avcbrsIPv6CidrBlockAssociation :: Lens' AssociateVPCCidrBlockResponse (Maybe VPCIPv6CidrBlockAssociation)
avcbrsIPv6CidrBlockAssociation = lens _avcbrsIPv6CidrBlockAssociation (\ s a -> s{_avcbrsIPv6CidrBlockAssociation = a})

-- | -- | The response status code.
avcbrsResponseStatus :: Lens' AssociateVPCCidrBlockResponse Int
avcbrsResponseStatus = lens _avcbrsResponseStatus (\ s a -> s{_avcbrsResponseStatus = a})

instance NFData AssociateVPCCidrBlockResponse where
