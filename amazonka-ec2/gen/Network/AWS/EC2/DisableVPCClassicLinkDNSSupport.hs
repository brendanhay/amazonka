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
-- Module      : Network.AWS.EC2.DisableVPCClassicLinkDNSSupport
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables ClassicLink DNS support for a VPC. If disabled, DNS hostnames resolve to public IP addresses when addressed between a linked EC2-Classic instance and instances in the VPC to which it's linked. For more information about ClassicLink, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink> in the /Amazon Elastic Compute Cloud User Guide/ .
--
--
module Network.AWS.EC2.DisableVPCClassicLinkDNSSupport
    (
    -- * Creating a Request
      disableVPCClassicLinkDNSSupport
    , DisableVPCClassicLinkDNSSupport
    -- * Request Lenses
    , dvcldsVPCId

    -- * Destructuring the Response
    , disableVPCClassicLinkDNSSupportResponse
    , DisableVPCClassicLinkDNSSupportResponse
    -- * Response Lenses
    , dvcldsrsReturn
    , dvcldsrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for DisableVpcClassicLinkDnsSupport.
--
--
--
-- /See:/ 'disableVPCClassicLinkDNSSupport' smart constructor.
newtype DisableVPCClassicLinkDNSSupport = DisableVPCClassicLinkDNSSupport'
  { _dvcldsVPCId :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisableVPCClassicLinkDNSSupport' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvcldsVPCId' - The ID of the VPC.
disableVPCClassicLinkDNSSupport
    :: DisableVPCClassicLinkDNSSupport
disableVPCClassicLinkDNSSupport =
  DisableVPCClassicLinkDNSSupport' {_dvcldsVPCId = Nothing}


-- | The ID of the VPC.
dvcldsVPCId :: Lens' DisableVPCClassicLinkDNSSupport (Maybe Text)
dvcldsVPCId = lens _dvcldsVPCId (\ s a -> s{_dvcldsVPCId = a})

instance AWSRequest DisableVPCClassicLinkDNSSupport
         where
        type Rs DisableVPCClassicLinkDNSSupport =
             DisableVPCClassicLinkDNSSupportResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DisableVPCClassicLinkDNSSupportResponse' <$>
                   (x .@? "return") <*> (pure (fromEnum s)))

instance Hashable DisableVPCClassicLinkDNSSupport
         where

instance NFData DisableVPCClassicLinkDNSSupport where

instance ToHeaders DisableVPCClassicLinkDNSSupport
         where
        toHeaders = const mempty

instance ToPath DisableVPCClassicLinkDNSSupport where
        toPath = const "/"

instance ToQuery DisableVPCClassicLinkDNSSupport
         where
        toQuery DisableVPCClassicLinkDNSSupport'{..}
          = mconcat
              ["Action" =:
                 ("DisableVpcClassicLinkDnsSupport" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "VpcId" =: _dvcldsVPCId]

-- | Contains the output of DisableVpcClassicLinkDnsSupport.
--
--
--
-- /See:/ 'disableVPCClassicLinkDNSSupportResponse' smart constructor.
data DisableVPCClassicLinkDNSSupportResponse = DisableVPCClassicLinkDNSSupportResponse'
  { _dvcldsrsReturn         :: !(Maybe Bool)
  , _dvcldsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisableVPCClassicLinkDNSSupportResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvcldsrsReturn' - Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- * 'dvcldsrsResponseStatus' - -- | The response status code.
disableVPCClassicLinkDNSSupportResponse
    :: Int -- ^ 'dvcldsrsResponseStatus'
    -> DisableVPCClassicLinkDNSSupportResponse
disableVPCClassicLinkDNSSupportResponse pResponseStatus_ =
  DisableVPCClassicLinkDNSSupportResponse'
    {_dvcldsrsReturn = Nothing, _dvcldsrsResponseStatus = pResponseStatus_}


-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
dvcldsrsReturn :: Lens' DisableVPCClassicLinkDNSSupportResponse (Maybe Bool)
dvcldsrsReturn = lens _dvcldsrsReturn (\ s a -> s{_dvcldsrsReturn = a})

-- | -- | The response status code.
dvcldsrsResponseStatus :: Lens' DisableVPCClassicLinkDNSSupportResponse Int
dvcldsrsResponseStatus = lens _dvcldsrsResponseStatus (\ s a -> s{_dvcldsrsResponseStatus = a})

instance NFData
           DisableVPCClassicLinkDNSSupportResponse
         where
