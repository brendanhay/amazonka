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
-- Module      : Network.AWS.EC2.DisassociateSubnetCidrBlock
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a CIDR block from a subnet. Currently, you can disassociate an IPv6 CIDR block only. You must detach or delete all gateways and resources that are associated with the CIDR block before you can disassociate it.
--
--
module Network.AWS.EC2.DisassociateSubnetCidrBlock
    (
    -- * Creating a Request
      disassociateSubnetCidrBlock
    , DisassociateSubnetCidrBlock
    -- * Request Lenses
    , dscbAssociationId

    -- * Destructuring the Response
    , disassociateSubnetCidrBlockResponse
    , DisassociateSubnetCidrBlockResponse
    -- * Response Lenses
    , dscbrsSubnetId
    , dscbrsIPv6CidrBlockAssociation
    , dscbrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disassociateSubnetCidrBlock' smart constructor.
newtype DisassociateSubnetCidrBlock = DisassociateSubnetCidrBlock'
  { _dscbAssociationId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateSubnetCidrBlock' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dscbAssociationId' - The association ID for the CIDR block.
disassociateSubnetCidrBlock
    :: Text -- ^ 'dscbAssociationId'
    -> DisassociateSubnetCidrBlock
disassociateSubnetCidrBlock pAssociationId_ =
  DisassociateSubnetCidrBlock' {_dscbAssociationId = pAssociationId_}


-- | The association ID for the CIDR block.
dscbAssociationId :: Lens' DisassociateSubnetCidrBlock Text
dscbAssociationId = lens _dscbAssociationId (\ s a -> s{_dscbAssociationId = a})

instance AWSRequest DisassociateSubnetCidrBlock where
        type Rs DisassociateSubnetCidrBlock =
             DisassociateSubnetCidrBlockResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DisassociateSubnetCidrBlockResponse' <$>
                   (x .@? "subnetId") <*>
                     (x .@? "ipv6CidrBlockAssociation")
                     <*> (pure (fromEnum s)))

instance Hashable DisassociateSubnetCidrBlock where

instance NFData DisassociateSubnetCidrBlock where

instance ToHeaders DisassociateSubnetCidrBlock where
        toHeaders = const mempty

instance ToPath DisassociateSubnetCidrBlock where
        toPath = const "/"

instance ToQuery DisassociateSubnetCidrBlock where
        toQuery DisassociateSubnetCidrBlock'{..}
          = mconcat
              ["Action" =:
                 ("DisassociateSubnetCidrBlock" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "AssociationId" =: _dscbAssociationId]

-- | /See:/ 'disassociateSubnetCidrBlockResponse' smart constructor.
data DisassociateSubnetCidrBlockResponse = DisassociateSubnetCidrBlockResponse'
  { _dscbrsSubnetId                 :: !(Maybe Text)
  , _dscbrsIPv6CidrBlockAssociation :: !(Maybe SubnetIPv6CidrBlockAssociation)
  , _dscbrsResponseStatus           :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateSubnetCidrBlockResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dscbrsSubnetId' - The ID of the subnet.
--
-- * 'dscbrsIPv6CidrBlockAssociation' - Information about the IPv6 CIDR block association.
--
-- * 'dscbrsResponseStatus' - -- | The response status code.
disassociateSubnetCidrBlockResponse
    :: Int -- ^ 'dscbrsResponseStatus'
    -> DisassociateSubnetCidrBlockResponse
disassociateSubnetCidrBlockResponse pResponseStatus_ =
  DisassociateSubnetCidrBlockResponse'
    { _dscbrsSubnetId = Nothing
    , _dscbrsIPv6CidrBlockAssociation = Nothing
    , _dscbrsResponseStatus = pResponseStatus_
    }


-- | The ID of the subnet.
dscbrsSubnetId :: Lens' DisassociateSubnetCidrBlockResponse (Maybe Text)
dscbrsSubnetId = lens _dscbrsSubnetId (\ s a -> s{_dscbrsSubnetId = a})

-- | Information about the IPv6 CIDR block association.
dscbrsIPv6CidrBlockAssociation :: Lens' DisassociateSubnetCidrBlockResponse (Maybe SubnetIPv6CidrBlockAssociation)
dscbrsIPv6CidrBlockAssociation = lens _dscbrsIPv6CidrBlockAssociation (\ s a -> s{_dscbrsIPv6CidrBlockAssociation = a})

-- | -- | The response status code.
dscbrsResponseStatus :: Lens' DisassociateSubnetCidrBlockResponse Int
dscbrsResponseStatus = lens _dscbrsResponseStatus (\ s a -> s{_dscbrsResponseStatus = a})

instance NFData DisassociateSubnetCidrBlockResponse
         where
