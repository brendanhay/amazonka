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
-- Module      : Network.AWS.EC2.EnableVPCClassicLink
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables a VPC for ClassicLink. You can then link EC2-Classic instances
-- to your ClassicLink-enabled VPC to allow communication over private IP
-- addresses. You cannot enable your VPC for ClassicLink if any of your
-- VPC\'s route tables have existing routes for address ranges within the
-- '10.0.0.0\/8' IP address range, excluding local routes for VPCs in the
-- '10.0.0.0\/16' and '10.1.0.0\/16' IP address ranges. For more
-- information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink>
-- in the Amazon Elastic Compute Cloud User Guide.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-EnableVPCClassicLink.html AWS API Reference> for EnableVPCClassicLink.
module Network.AWS.EC2.EnableVPCClassicLink
    (
    -- * Creating a Request
      enableVPCClassicLink
    , EnableVPCClassicLink
    -- * Request Lenses
    , evclDryRun
    , evclVPCId

    -- * Destructuring the Response
    , enableVPCClassicLinkResponse
    , EnableVPCClassicLinkResponse
    -- * Response Lenses
    , evclrsReturn
    , evclrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'enableVPCClassicLink' smart constructor.
data EnableVPCClassicLink = EnableVPCClassicLink'
    { _evclDryRun :: !(Maybe Bool)
    , _evclVPCId  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'EnableVPCClassicLink' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'evclDryRun'
--
-- * 'evclVPCId'
enableVPCClassicLink
    :: Text -- ^ 'evclVPCId'
    -> EnableVPCClassicLink
enableVPCClassicLink pVPCId_ =
    EnableVPCClassicLink'
    { _evclDryRun = Nothing
    , _evclVPCId = pVPCId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is 'DryRunOperation'.
-- Otherwise, it is 'UnauthorizedOperation'.
evclDryRun :: Lens' EnableVPCClassicLink (Maybe Bool)
evclDryRun = lens _evclDryRun (\ s a -> s{_evclDryRun = a});

-- | The ID of the VPC.
evclVPCId :: Lens' EnableVPCClassicLink Text
evclVPCId = lens _evclVPCId (\ s a -> s{_evclVPCId = a});

instance AWSRequest EnableVPCClassicLink where
        type Rs EnableVPCClassicLink =
             EnableVPCClassicLinkResponse
        request = postQuery eC2
        response
          = receiveXML
              (\ s h x ->
                 EnableVPCClassicLinkResponse' <$>
                   (x .@? "return") <*> (pure (fromEnum s)))

instance ToHeaders EnableVPCClassicLink where
        toHeaders = const mempty

instance ToPath EnableVPCClassicLink where
        toPath = const "/"

instance ToQuery EnableVPCClassicLink where
        toQuery EnableVPCClassicLink'{..}
          = mconcat
              ["Action" =: ("EnableVpcClassicLink" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _evclDryRun, "VpcId" =: _evclVPCId]

-- | /See:/ 'enableVPCClassicLinkResponse' smart constructor.
data EnableVPCClassicLinkResponse = EnableVPCClassicLinkResponse'
    { _evclrsReturn :: !(Maybe Bool)
    , _evclrsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'EnableVPCClassicLinkResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'evclrsReturn'
--
-- * 'evclrsStatus'
enableVPCClassicLinkResponse
    :: Int -- ^ 'evclrsStatus'
    -> EnableVPCClassicLinkResponse
enableVPCClassicLinkResponse pStatus_ =
    EnableVPCClassicLinkResponse'
    { _evclrsReturn = Nothing
    , _evclrsStatus = pStatus_
    }

-- | Returns 'true' if the request succeeds; otherwise, it returns an error.
evclrsReturn :: Lens' EnableVPCClassicLinkResponse (Maybe Bool)
evclrsReturn = lens _evclrsReturn (\ s a -> s{_evclrsReturn = a});

-- | The response status code.
evclrsStatus :: Lens' EnableVPCClassicLinkResponse Int
evclrsStatus = lens _evclrsStatus (\ s a -> s{_evclrsStatus = a});
