{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AttachInternetGateway
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Attaches an Internet gateway to a VPC, enabling connectivity between the
-- Internet and the VPC. For more information about your VPC and Internet
-- gateway, see the
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/ Amazon Virtual Private Cloud User Guide>.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AttachInternetGateway.html>
module Network.AWS.EC2.AttachInternetGateway
    (
    -- * Request
      AttachInternetGateway
    -- ** Request constructor
    , attachInternetGateway
    -- ** Request lenses
    , aigrqDryRun
    , aigrqInternetGatewayId
    , aigrqVPCId

    -- * Response
    , AttachInternetGatewayResponse
    -- ** Response constructor
    , attachInternetGatewayResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'attachInternetGateway' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aigrqDryRun'
--
-- * 'aigrqInternetGatewayId'
--
-- * 'aigrqVPCId'
data AttachInternetGateway = AttachInternetGateway'
    { _aigrqDryRun            :: !(Maybe Bool)
    , _aigrqInternetGatewayId :: !Text
    , _aigrqVPCId             :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AttachInternetGateway' smart constructor.
attachInternetGateway :: Text -> Text -> AttachInternetGateway
attachInternetGateway pInternetGatewayId_ pVPCId_ =
    AttachInternetGateway'
    { _aigrqDryRun = Nothing
    , _aigrqInternetGatewayId = pInternetGatewayId_
    , _aigrqVPCId = pVPCId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
aigrqDryRun :: Lens' AttachInternetGateway (Maybe Bool)
aigrqDryRun = lens _aigrqDryRun (\ s a -> s{_aigrqDryRun = a});

-- | The ID of the Internet gateway.
aigrqInternetGatewayId :: Lens' AttachInternetGateway Text
aigrqInternetGatewayId = lens _aigrqInternetGatewayId (\ s a -> s{_aigrqInternetGatewayId = a});

-- | The ID of the VPC.
aigrqVPCId :: Lens' AttachInternetGateway Text
aigrqVPCId = lens _aigrqVPCId (\ s a -> s{_aigrqVPCId = a});

instance AWSRequest AttachInternetGateway where
        type Sv AttachInternetGateway = EC2
        type Rs AttachInternetGateway =
             AttachInternetGatewayResponse
        request = post
        response = receiveNull AttachInternetGatewayResponse'

instance ToHeaders AttachInternetGateway where
        toHeaders = const mempty

instance ToPath AttachInternetGateway where
        toPath = const "/"

instance ToQuery AttachInternetGateway where
        toQuery AttachInternetGateway'{..}
          = mconcat
              ["Action" =: ("AttachInternetGateway" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _aigrqDryRun,
               "InternetGatewayId" =: _aigrqInternetGatewayId,
               "VpcId" =: _aigrqVPCId]

-- | /See:/ 'attachInternetGatewayResponse' smart constructor.
data AttachInternetGatewayResponse =
    AttachInternetGatewayResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AttachInternetGatewayResponse' smart constructor.
attachInternetGatewayResponse :: AttachInternetGatewayResponse
attachInternetGatewayResponse = AttachInternetGatewayResponse'
