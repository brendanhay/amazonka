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
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a CIDR block with your VPC. You can only associate a single Amazon-provided IPv6 CIDR block with your VPC. The IPv6 CIDR block size is fixed at /56.
--
--
module Network.AWS.EC2.AssociateVPCCidrBlock
    (
    -- * Creating a Request
      associateVPCCidrBlock
    , AssociateVPCCidrBlock
    -- * Request Lenses
    , avcbAmazonProvidedIPv6CidrBlock
    , avcbVPCId

    -- * Destructuring the Response
    , associateVPCCidrBlockResponse
    , AssociateVPCCidrBlockResponse
    -- * Response Lenses
    , avcbrsVPCId
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
  { _avcbAmazonProvidedIPv6CidrBlock :: {-# NOUNPACK #-}!(Maybe Bool)
  , _avcbVPCId                       :: {-# NOUNPACK #-}!Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateVPCCidrBlock' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avcbAmazonProvidedIPv6CidrBlock' - Requests an Amazon-provided IPv6 CIDR block with a /56 prefix length for the VPC. You cannot specify the range of IPv6 addresses, or the size of the CIDR block.
--
-- * 'avcbVPCId' - The ID of the VPC.
associateVPCCidrBlock
    :: Text -- ^ 'avcbVPCId'
    -> AssociateVPCCidrBlock
associateVPCCidrBlock pVPCId_ =
  AssociateVPCCidrBlock'
  {_avcbAmazonProvidedIPv6CidrBlock = Nothing, _avcbVPCId = pVPCId_}


-- | Requests an Amazon-provided IPv6 CIDR block with a /56 prefix length for the VPC. You cannot specify the range of IPv6 addresses, or the size of the CIDR block.
avcbAmazonProvidedIPv6CidrBlock :: Lens' AssociateVPCCidrBlock (Maybe Bool)
avcbAmazonProvidedIPv6CidrBlock = lens _avcbAmazonProvidedIPv6CidrBlock (\ s a -> s{_avcbAmazonProvidedIPv6CidrBlock = a});

-- | The ID of the VPC.
avcbVPCId :: Lens' AssociateVPCCidrBlock Text
avcbVPCId = lens _avcbVPCId (\ s a -> s{_avcbVPCId = a});

instance AWSRequest AssociateVPCCidrBlock where
        type Rs AssociateVPCCidrBlock =
             AssociateVPCCidrBlockResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 AssociateVPCCidrBlockResponse' <$>
                   (x .@? "vpcId") <*>
                     (x .@? "ipv6CidrBlockAssociation")
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
               "AmazonProvidedIpv6CidrBlock" =:
                 _avcbAmazonProvidedIPv6CidrBlock,
               "VpcId" =: _avcbVPCId]

-- | /See:/ 'associateVPCCidrBlockResponse' smart constructor.
data AssociateVPCCidrBlockResponse = AssociateVPCCidrBlockResponse'
  { _avcbrsVPCId :: {-# NOUNPACK #-}!(Maybe Text)
  , _avcbrsIPv6CidrBlockAssociation :: {-# NOUNPACK #-}!(Maybe VPCIPv6CidrBlockAssociation)
  , _avcbrsResponseStatus :: {-# NOUNPACK #-}!Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateVPCCidrBlockResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avcbrsVPCId' - The ID of the VPC.
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
  , _avcbrsIPv6CidrBlockAssociation = Nothing
  , _avcbrsResponseStatus = pResponseStatus_
  }


-- | The ID of the VPC.
avcbrsVPCId :: Lens' AssociateVPCCidrBlockResponse (Maybe Text)
avcbrsVPCId = lens _avcbrsVPCId (\ s a -> s{_avcbrsVPCId = a});

-- | Information about the IPv6 CIDR block association.
avcbrsIPv6CidrBlockAssociation :: Lens' AssociateVPCCidrBlockResponse (Maybe VPCIPv6CidrBlockAssociation)
avcbrsIPv6CidrBlockAssociation = lens _avcbrsIPv6CidrBlockAssociation (\ s a -> s{_avcbrsIPv6CidrBlockAssociation = a});

-- | -- | The response status code.
avcbrsResponseStatus :: Lens' AssociateVPCCidrBlockResponse Int
avcbrsResponseStatus = lens _avcbrsResponseStatus (\ s a -> s{_avcbrsResponseStatus = a});

instance NFData AssociateVPCCidrBlockResponse where
