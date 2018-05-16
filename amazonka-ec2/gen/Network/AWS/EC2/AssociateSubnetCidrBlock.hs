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
-- Module      : Network.AWS.EC2.AssociateSubnetCidrBlock
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a CIDR block with your subnet. You can only associate a single IPv6 CIDR block with your subnet. An IPv6 CIDR block must have a prefix length of /64.
--
--
module Network.AWS.EC2.AssociateSubnetCidrBlock
    (
    -- * Creating a Request
      associateSubnetCidrBlock
    , AssociateSubnetCidrBlock
    -- * Request Lenses
    , ascbIPv6CidrBlock
    , ascbSubnetId

    -- * Destructuring the Response
    , associateSubnetCidrBlockResponse
    , AssociateSubnetCidrBlockResponse
    -- * Response Lenses
    , ascbrsSubnetId
    , ascbrsIPv6CidrBlockAssociation
    , ascbrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'associateSubnetCidrBlock' smart constructor.
data AssociateSubnetCidrBlock = AssociateSubnetCidrBlock'
  { _ascbIPv6CidrBlock :: !Text
  , _ascbSubnetId      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateSubnetCidrBlock' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ascbIPv6CidrBlock' - The IPv6 CIDR block for your subnet. The subnet must have a /64 prefix length.
--
-- * 'ascbSubnetId' - The ID of your subnet.
associateSubnetCidrBlock
    :: Text -- ^ 'ascbIPv6CidrBlock'
    -> Text -- ^ 'ascbSubnetId'
    -> AssociateSubnetCidrBlock
associateSubnetCidrBlock pIPv6CidrBlock_ pSubnetId_ =
  AssociateSubnetCidrBlock'
    {_ascbIPv6CidrBlock = pIPv6CidrBlock_, _ascbSubnetId = pSubnetId_}


-- | The IPv6 CIDR block for your subnet. The subnet must have a /64 prefix length.
ascbIPv6CidrBlock :: Lens' AssociateSubnetCidrBlock Text
ascbIPv6CidrBlock = lens _ascbIPv6CidrBlock (\ s a -> s{_ascbIPv6CidrBlock = a})

-- | The ID of your subnet.
ascbSubnetId :: Lens' AssociateSubnetCidrBlock Text
ascbSubnetId = lens _ascbSubnetId (\ s a -> s{_ascbSubnetId = a})

instance AWSRequest AssociateSubnetCidrBlock where
        type Rs AssociateSubnetCidrBlock =
             AssociateSubnetCidrBlockResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 AssociateSubnetCidrBlockResponse' <$>
                   (x .@? "subnetId") <*>
                     (x .@? "ipv6CidrBlockAssociation")
                     <*> (pure (fromEnum s)))

instance Hashable AssociateSubnetCidrBlock where

instance NFData AssociateSubnetCidrBlock where

instance ToHeaders AssociateSubnetCidrBlock where
        toHeaders = const mempty

instance ToPath AssociateSubnetCidrBlock where
        toPath = const "/"

instance ToQuery AssociateSubnetCidrBlock where
        toQuery AssociateSubnetCidrBlock'{..}
          = mconcat
              ["Action" =:
                 ("AssociateSubnetCidrBlock" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "Ipv6CidrBlock" =: _ascbIPv6CidrBlock,
               "SubnetId" =: _ascbSubnetId]

-- | /See:/ 'associateSubnetCidrBlockResponse' smart constructor.
data AssociateSubnetCidrBlockResponse = AssociateSubnetCidrBlockResponse'
  { _ascbrsSubnetId                 :: !(Maybe Text)
  , _ascbrsIPv6CidrBlockAssociation :: !(Maybe SubnetIPv6CidrBlockAssociation)
  , _ascbrsResponseStatus           :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateSubnetCidrBlockResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ascbrsSubnetId' - The ID of the subnet.
--
-- * 'ascbrsIPv6CidrBlockAssociation' - Information about the IPv6 CIDR block association.
--
-- * 'ascbrsResponseStatus' - -- | The response status code.
associateSubnetCidrBlockResponse
    :: Int -- ^ 'ascbrsResponseStatus'
    -> AssociateSubnetCidrBlockResponse
associateSubnetCidrBlockResponse pResponseStatus_ =
  AssociateSubnetCidrBlockResponse'
    { _ascbrsSubnetId = Nothing
    , _ascbrsIPv6CidrBlockAssociation = Nothing
    , _ascbrsResponseStatus = pResponseStatus_
    }


-- | The ID of the subnet.
ascbrsSubnetId :: Lens' AssociateSubnetCidrBlockResponse (Maybe Text)
ascbrsSubnetId = lens _ascbrsSubnetId (\ s a -> s{_ascbrsSubnetId = a})

-- | Information about the IPv6 CIDR block association.
ascbrsIPv6CidrBlockAssociation :: Lens' AssociateSubnetCidrBlockResponse (Maybe SubnetIPv6CidrBlockAssociation)
ascbrsIPv6CidrBlockAssociation = lens _ascbrsIPv6CidrBlockAssociation (\ s a -> s{_ascbrsIPv6CidrBlockAssociation = a})

-- | -- | The response status code.
ascbrsResponseStatus :: Lens' AssociateSubnetCidrBlockResponse Int
ascbrsResponseStatus = lens _ascbrsResponseStatus (\ s a -> s{_ascbrsResponseStatus = a})

instance NFData AssociateSubnetCidrBlockResponse
         where
