{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.StartVPCEndpointServicePrivateDNSVerification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates the verification process to prove that the service provider owns the private DNS name domain for the endpoint service.
--
--
-- The service provider must successfully perform the verification before the consumer can use the name to access the service.
--
-- Before the service provider runs this command, they must add a record to the DNS server. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/endpoint-services-dns-validation.html#add-dns-txt-record Adding a TXT Record to Your Domain's DNS Server > in the /Amazon VPC User Guide/ .
module Network.AWS.EC2.StartVPCEndpointServicePrivateDNSVerification
  ( -- * Creating a Request
    startVPCEndpointServicePrivateDNSVerification,
    StartVPCEndpointServicePrivateDNSVerification,

    -- * Request Lenses
    svespdvDryRun,
    svespdvServiceId,

    -- * Destructuring the Response
    startVPCEndpointServicePrivateDNSVerificationResponse,
    StartVPCEndpointServicePrivateDNSVerificationResponse,

    -- * Response Lenses
    svespdvrsReturnValue,
    svespdvrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startVPCEndpointServicePrivateDNSVerification' smart constructor.
data StartVPCEndpointServicePrivateDNSVerification = StartVPCEndpointServicePrivateDNSVerification'
  { _svespdvDryRun ::
      !( Maybe
           Bool
       ),
    _svespdvServiceId ::
      !Text
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'StartVPCEndpointServicePrivateDNSVerification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'svespdvDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'svespdvServiceId' - The ID of the endpoint service.
startVPCEndpointServicePrivateDNSVerification ::
  -- | 'svespdvServiceId'
  Text ->
  StartVPCEndpointServicePrivateDNSVerification
startVPCEndpointServicePrivateDNSVerification pServiceId_ =
  StartVPCEndpointServicePrivateDNSVerification'
    { _svespdvDryRun =
        Nothing,
      _svespdvServiceId = pServiceId_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
svespdvDryRun :: Lens' StartVPCEndpointServicePrivateDNSVerification (Maybe Bool)
svespdvDryRun = lens _svespdvDryRun (\s a -> s {_svespdvDryRun = a})

-- | The ID of the endpoint service.
svespdvServiceId :: Lens' StartVPCEndpointServicePrivateDNSVerification Text
svespdvServiceId = lens _svespdvServiceId (\s a -> s {_svespdvServiceId = a})

instance AWSRequest StartVPCEndpointServicePrivateDNSVerification where
  type
    Rs StartVPCEndpointServicePrivateDNSVerification =
      StartVPCEndpointServicePrivateDNSVerificationResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          StartVPCEndpointServicePrivateDNSVerificationResponse'
            <$> (x .@? "return") <*> (pure (fromEnum s))
      )

instance Hashable StartVPCEndpointServicePrivateDNSVerification

instance NFData StartVPCEndpointServicePrivateDNSVerification

instance ToHeaders StartVPCEndpointServicePrivateDNSVerification where
  toHeaders = const mempty

instance ToPath StartVPCEndpointServicePrivateDNSVerification where
  toPath = const "/"

instance ToQuery StartVPCEndpointServicePrivateDNSVerification where
  toQuery StartVPCEndpointServicePrivateDNSVerification' {..} =
    mconcat
      [ "Action"
          =: ("StartVpcEndpointServicePrivateDnsVerification" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "DryRun" =: _svespdvDryRun,
        "ServiceId" =: _svespdvServiceId
      ]

-- | /See:/ 'startVPCEndpointServicePrivateDNSVerificationResponse' smart constructor.
data StartVPCEndpointServicePrivateDNSVerificationResponse = StartVPCEndpointServicePrivateDNSVerificationResponse'
  { _svespdvrsReturnValue ::
      !( Maybe
           Bool
       ),
    _svespdvrsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'StartVPCEndpointServicePrivateDNSVerificationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'svespdvrsReturnValue' - Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- * 'svespdvrsResponseStatus' - -- | The response status code.
startVPCEndpointServicePrivateDNSVerificationResponse ::
  -- | 'svespdvrsResponseStatus'
  Int ->
  StartVPCEndpointServicePrivateDNSVerificationResponse
startVPCEndpointServicePrivateDNSVerificationResponse
  pResponseStatus_ =
    StartVPCEndpointServicePrivateDNSVerificationResponse'
      { _svespdvrsReturnValue =
          Nothing,
        _svespdvrsResponseStatus =
          pResponseStatus_
      }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
svespdvrsReturnValue :: Lens' StartVPCEndpointServicePrivateDNSVerificationResponse (Maybe Bool)
svespdvrsReturnValue = lens _svespdvrsReturnValue (\s a -> s {_svespdvrsReturnValue = a})

-- | -- | The response status code.
svespdvrsResponseStatus :: Lens' StartVPCEndpointServicePrivateDNSVerificationResponse Int
svespdvrsResponseStatus = lens _svespdvrsResponseStatus (\s a -> s {_svespdvrsResponseStatus = a})

instance
  NFData
    StartVPCEndpointServicePrivateDNSVerificationResponse
