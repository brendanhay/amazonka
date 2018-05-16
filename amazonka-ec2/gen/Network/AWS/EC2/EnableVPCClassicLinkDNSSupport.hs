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
-- Module      : Network.AWS.EC2.EnableVPCClassicLinkDNSSupport
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables a VPC to support DNS hostname resolution for ClassicLink. If enabled, the DNS hostname of a linked EC2-Classic instance resolves to its private IP address when addressed from an instance in the VPC to which it's linked. Similarly, the DNS hostname of an instance in a VPC resolves to its private IP address when addressed from a linked EC2-Classic instance. For more information about ClassicLink, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink> in the /Amazon Elastic Compute Cloud User Guide/ .
--
--
module Network.AWS.EC2.EnableVPCClassicLinkDNSSupport
    (
    -- * Creating a Request
      enableVPCClassicLinkDNSSupport
    , EnableVPCClassicLinkDNSSupport
    -- * Request Lenses
    , evcldsVPCId

    -- * Destructuring the Response
    , enableVPCClassicLinkDNSSupportResponse
    , EnableVPCClassicLinkDNSSupportResponse
    -- * Response Lenses
    , evcldsrsReturn
    , evcldsrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for EnableVpcClassicLinkDnsSupport.
--
--
--
-- /See:/ 'enableVPCClassicLinkDNSSupport' smart constructor.
newtype EnableVPCClassicLinkDNSSupport = EnableVPCClassicLinkDNSSupport'
  { _evcldsVPCId :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnableVPCClassicLinkDNSSupport' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'evcldsVPCId' - The ID of the VPC.
enableVPCClassicLinkDNSSupport
    :: EnableVPCClassicLinkDNSSupport
enableVPCClassicLinkDNSSupport =
  EnableVPCClassicLinkDNSSupport' {_evcldsVPCId = Nothing}


-- | The ID of the VPC.
evcldsVPCId :: Lens' EnableVPCClassicLinkDNSSupport (Maybe Text)
evcldsVPCId = lens _evcldsVPCId (\ s a -> s{_evcldsVPCId = a})

instance AWSRequest EnableVPCClassicLinkDNSSupport
         where
        type Rs EnableVPCClassicLinkDNSSupport =
             EnableVPCClassicLinkDNSSupportResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 EnableVPCClassicLinkDNSSupportResponse' <$>
                   (x .@? "return") <*> (pure (fromEnum s)))

instance Hashable EnableVPCClassicLinkDNSSupport
         where

instance NFData EnableVPCClassicLinkDNSSupport where

instance ToHeaders EnableVPCClassicLinkDNSSupport
         where
        toHeaders = const mempty

instance ToPath EnableVPCClassicLinkDNSSupport where
        toPath = const "/"

instance ToQuery EnableVPCClassicLinkDNSSupport where
        toQuery EnableVPCClassicLinkDNSSupport'{..}
          = mconcat
              ["Action" =:
                 ("EnableVpcClassicLinkDnsSupport" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "VpcId" =: _evcldsVPCId]

-- | Contains the output of EnableVpcClassicLinkDnsSupport.
--
--
--
-- /See:/ 'enableVPCClassicLinkDNSSupportResponse' smart constructor.
data EnableVPCClassicLinkDNSSupportResponse = EnableVPCClassicLinkDNSSupportResponse'
  { _evcldsrsReturn         :: !(Maybe Bool)
  , _evcldsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnableVPCClassicLinkDNSSupportResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'evcldsrsReturn' - Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- * 'evcldsrsResponseStatus' - -- | The response status code.
enableVPCClassicLinkDNSSupportResponse
    :: Int -- ^ 'evcldsrsResponseStatus'
    -> EnableVPCClassicLinkDNSSupportResponse
enableVPCClassicLinkDNSSupportResponse pResponseStatus_ =
  EnableVPCClassicLinkDNSSupportResponse'
    {_evcldsrsReturn = Nothing, _evcldsrsResponseStatus = pResponseStatus_}


-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
evcldsrsReturn :: Lens' EnableVPCClassicLinkDNSSupportResponse (Maybe Bool)
evcldsrsReturn = lens _evcldsrsReturn (\ s a -> s{_evcldsrsReturn = a})

-- | -- | The response status code.
evcldsrsResponseStatus :: Lens' EnableVPCClassicLinkDNSSupportResponse Int
evcldsrsResponseStatus = lens _evcldsrsResponseStatus (\ s a -> s{_evcldsrsResponseStatus = a})

instance NFData
           EnableVPCClassicLinkDNSSupportResponse
         where
