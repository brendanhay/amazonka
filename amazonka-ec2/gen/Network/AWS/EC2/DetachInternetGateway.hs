{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DetachInternetGateway
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Detaches an Internet gateway from a VPC, disabling connectivity between
-- the Internet and the VPC. The VPC must not contain any running instances
-- with Elastic IP addresses.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DetachInternetGateway.html>
module Network.AWS.EC2.DetachInternetGateway
    (
    -- * Request
      DetachInternetGateway
    -- ** Request constructor
    , detachInternetGateway
    -- ** Request lenses
    , digrqDryRun
    , digrqInternetGatewayId
    , digrqVPCId

    -- * Response
    , DetachInternetGatewayResponse
    -- ** Response constructor
    , detachInternetGatewayResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'detachInternetGateway' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'digrqDryRun'
--
-- * 'digrqInternetGatewayId'
--
-- * 'digrqVPCId'
data DetachInternetGateway = DetachInternetGateway'
    { _digrqDryRun            :: !(Maybe Bool)
    , _digrqInternetGatewayId :: !Text
    , _digrqVPCId             :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DetachInternetGateway' smart constructor.
detachInternetGateway :: Text -> Text -> DetachInternetGateway
detachInternetGateway pInternetGatewayId pVPCId =
    DetachInternetGateway'
    { _digrqDryRun = Nothing
    , _digrqInternetGatewayId = pInternetGatewayId
    , _digrqVPCId = pVPCId
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
digrqDryRun :: Lens' DetachInternetGateway (Maybe Bool)
digrqDryRun = lens _digrqDryRun (\ s a -> s{_digrqDryRun = a});

-- | The ID of the Internet gateway.
digrqInternetGatewayId :: Lens' DetachInternetGateway Text
digrqInternetGatewayId = lens _digrqInternetGatewayId (\ s a -> s{_digrqInternetGatewayId = a});

-- | The ID of the VPC.
digrqVPCId :: Lens' DetachInternetGateway Text
digrqVPCId = lens _digrqVPCId (\ s a -> s{_digrqVPCId = a});

instance AWSRequest DetachInternetGateway where
        type Sv DetachInternetGateway = EC2
        type Rs DetachInternetGateway =
             DetachInternetGatewayResponse
        request = post
        response = receiveNull DetachInternetGatewayResponse'

instance ToHeaders DetachInternetGateway where
        toHeaders = const mempty

instance ToPath DetachInternetGateway where
        toPath = const "/"

instance ToQuery DetachInternetGateway where
        toQuery DetachInternetGateway'{..}
          = mconcat
              ["Action" =: ("DetachInternetGateway" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _digrqDryRun,
               "InternetGatewayId" =: _digrqInternetGatewayId,
               "VpcId" =: _digrqVPCId]

-- | /See:/ 'detachInternetGatewayResponse' smart constructor.
data DetachInternetGatewayResponse =
    DetachInternetGatewayResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DetachInternetGatewayResponse' smart constructor.
detachInternetGatewayResponse :: DetachInternetGatewayResponse
detachInternetGatewayResponse = DetachInternetGatewayResponse'
