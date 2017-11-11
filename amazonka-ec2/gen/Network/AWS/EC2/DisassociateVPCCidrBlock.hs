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
-- Module      : Network.AWS.EC2.DisassociateVPCCidrBlock
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a CIDR block from a VPC. Currently, you can disassociate an IPv6 CIDR block only. You must detach or delete all gateways and resources that are associated with the CIDR block before you can disassociate it.
--
--
module Network.AWS.EC2.DisassociateVPCCidrBlock
    (
    -- * Creating a Request
      disassociateVPCCidrBlock
    , DisassociateVPCCidrBlock
    -- * Request Lenses
    , dvcbAssociationId

    -- * Destructuring the Response
    , disassociateVPCCidrBlockResponse
    , DisassociateVPCCidrBlockResponse
    -- * Response Lenses
    , dvcbrsVPCId
    , dvcbrsIPv6CidrBlockAssociation
    , dvcbrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disassociateVPCCidrBlock' smart constructor.
newtype DisassociateVPCCidrBlock = DisassociateVPCCidrBlock'
  { _dvcbAssociationId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateVPCCidrBlock' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvcbAssociationId' - The association ID for the CIDR block.
disassociateVPCCidrBlock
    :: Text -- ^ 'dvcbAssociationId'
    -> DisassociateVPCCidrBlock
disassociateVPCCidrBlock pAssociationId_ =
  DisassociateVPCCidrBlock' {_dvcbAssociationId = pAssociationId_}


-- | The association ID for the CIDR block.
dvcbAssociationId :: Lens' DisassociateVPCCidrBlock Text
dvcbAssociationId = lens _dvcbAssociationId (\ s a -> s{_dvcbAssociationId = a});

instance AWSRequest DisassociateVPCCidrBlock where
        type Rs DisassociateVPCCidrBlock =
             DisassociateVPCCidrBlockResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DisassociateVPCCidrBlockResponse' <$>
                   (x .@? "vpcId") <*>
                     (x .@? "ipv6CidrBlockAssociation")
                     <*> (pure (fromEnum s)))

instance Hashable DisassociateVPCCidrBlock where

instance NFData DisassociateVPCCidrBlock where

instance ToHeaders DisassociateVPCCidrBlock where
        toHeaders = const mempty

instance ToPath DisassociateVPCCidrBlock where
        toPath = const "/"

instance ToQuery DisassociateVPCCidrBlock where
        toQuery DisassociateVPCCidrBlock'{..}
          = mconcat
              ["Action" =:
                 ("DisassociateVpcCidrBlock" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "AssociationId" =: _dvcbAssociationId]

-- | /See:/ 'disassociateVPCCidrBlockResponse' smart constructor.
data DisassociateVPCCidrBlockResponse = DisassociateVPCCidrBlockResponse'
  { _dvcbrsVPCId :: {-# NOUNPACK #-}!(Maybe Text)
  , _dvcbrsIPv6CidrBlockAssociation :: {-# NOUNPACK #-}!(Maybe VPCIPv6CidrBlockAssociation)
  , _dvcbrsResponseStatus :: {-# NOUNPACK #-}!Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateVPCCidrBlockResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvcbrsVPCId' - The ID of the VPC.
--
-- * 'dvcbrsIPv6CidrBlockAssociation' - Information about the IPv6 CIDR block association.
--
-- * 'dvcbrsResponseStatus' - -- | The response status code.
disassociateVPCCidrBlockResponse
    :: Int -- ^ 'dvcbrsResponseStatus'
    -> DisassociateVPCCidrBlockResponse
disassociateVPCCidrBlockResponse pResponseStatus_ =
  DisassociateVPCCidrBlockResponse'
  { _dvcbrsVPCId = Nothing
  , _dvcbrsIPv6CidrBlockAssociation = Nothing
  , _dvcbrsResponseStatus = pResponseStatus_
  }


-- | The ID of the VPC.
dvcbrsVPCId :: Lens' DisassociateVPCCidrBlockResponse (Maybe Text)
dvcbrsVPCId = lens _dvcbrsVPCId (\ s a -> s{_dvcbrsVPCId = a});

-- | Information about the IPv6 CIDR block association.
dvcbrsIPv6CidrBlockAssociation :: Lens' DisassociateVPCCidrBlockResponse (Maybe VPCIPv6CidrBlockAssociation)
dvcbrsIPv6CidrBlockAssociation = lens _dvcbrsIPv6CidrBlockAssociation (\ s a -> s{_dvcbrsIPv6CidrBlockAssociation = a});

-- | -- | The response status code.
dvcbrsResponseStatus :: Lens' DisassociateVPCCidrBlockResponse Int
dvcbrsResponseStatus = lens _dvcbrsResponseStatus (\ s a -> s{_dvcbrsResponseStatus = a});

instance NFData DisassociateVPCCidrBlockResponse
         where
