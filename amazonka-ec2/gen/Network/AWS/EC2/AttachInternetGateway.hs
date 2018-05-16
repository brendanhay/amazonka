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
-- Module      : Network.AWS.EC2.AttachInternetGateway
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches an Internet gateway to a VPC, enabling connectivity between the Internet and the VPC. For more information about your VPC and Internet gateway, see the <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/ Amazon Virtual Private Cloud User Guide> .
--
--
module Network.AWS.EC2.AttachInternetGateway
    (
    -- * Creating a Request
      attachInternetGateway
    , AttachInternetGateway
    -- * Request Lenses
    , aigDryRun
    , aigInternetGatewayId
    , aigVPCId

    -- * Destructuring the Response
    , attachInternetGatewayResponse
    , AttachInternetGatewayResponse
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for AttachInternetGateway.
--
--
--
-- /See:/ 'attachInternetGateway' smart constructor.
data AttachInternetGateway = AttachInternetGateway'
  { _aigDryRun            :: !(Maybe Bool)
  , _aigInternetGatewayId :: !Text
  , _aigVPCId             :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttachInternetGateway' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aigDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'aigInternetGatewayId' - The ID of the Internet gateway.
--
-- * 'aigVPCId' - The ID of the VPC.
attachInternetGateway
    :: Text -- ^ 'aigInternetGatewayId'
    -> Text -- ^ 'aigVPCId'
    -> AttachInternetGateway
attachInternetGateway pInternetGatewayId_ pVPCId_ =
  AttachInternetGateway'
    { _aigDryRun = Nothing
    , _aigInternetGatewayId = pInternetGatewayId_
    , _aigVPCId = pVPCId_
    }


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
aigDryRun :: Lens' AttachInternetGateway (Maybe Bool)
aigDryRun = lens _aigDryRun (\ s a -> s{_aigDryRun = a})

-- | The ID of the Internet gateway.
aigInternetGatewayId :: Lens' AttachInternetGateway Text
aigInternetGatewayId = lens _aigInternetGatewayId (\ s a -> s{_aigInternetGatewayId = a})

-- | The ID of the VPC.
aigVPCId :: Lens' AttachInternetGateway Text
aigVPCId = lens _aigVPCId (\ s a -> s{_aigVPCId = a})

instance AWSRequest AttachInternetGateway where
        type Rs AttachInternetGateway =
             AttachInternetGatewayResponse
        request = postQuery ec2
        response = receiveNull AttachInternetGatewayResponse'

instance Hashable AttachInternetGateway where

instance NFData AttachInternetGateway where

instance ToHeaders AttachInternetGateway where
        toHeaders = const mempty

instance ToPath AttachInternetGateway where
        toPath = const "/"

instance ToQuery AttachInternetGateway where
        toQuery AttachInternetGateway'{..}
          = mconcat
              ["Action" =: ("AttachInternetGateway" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _aigDryRun,
               "InternetGatewayId" =: _aigInternetGatewayId,
               "VpcId" =: _aigVPCId]

-- | /See:/ 'attachInternetGatewayResponse' smart constructor.
data AttachInternetGatewayResponse =
  AttachInternetGatewayResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttachInternetGatewayResponse' with the minimum fields required to make a request.
--
attachInternetGatewayResponse
    :: AttachInternetGatewayResponse
attachInternetGatewayResponse = AttachInternetGatewayResponse'


instance NFData AttachInternetGatewayResponse where
