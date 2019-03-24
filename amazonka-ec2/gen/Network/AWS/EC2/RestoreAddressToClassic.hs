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
-- Module      : Network.AWS.EC2.RestoreAddressToClassic
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores an Elastic IP address that was previously moved to the EC2-VPC platform back to the EC2-Classic platform. You cannot move an Elastic IP address that was originally allocated for use in EC2-VPC. The Elastic IP address must not be associated with an instance or network interface.
--
--
module Network.AWS.EC2.RestoreAddressToClassic
    (
    -- * Creating a Request
      restoreAddressToClassic
    , RestoreAddressToClassic
    -- * Request Lenses
    , ratcDryRun
    , ratcPublicIP

    -- * Destructuring the Response
    , restoreAddressToClassicResponse
    , RestoreAddressToClassicResponse
    -- * Response Lenses
    , ratcrsStatus
    , ratcrsPublicIP
    , ratcrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'restoreAddressToClassic' smart constructor.
data RestoreAddressToClassic = RestoreAddressToClassic'
  { _ratcDryRun   :: !(Maybe Bool)
  , _ratcPublicIP :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RestoreAddressToClassic' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ratcDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'ratcPublicIP' - The Elastic IP address.
restoreAddressToClassic
    :: Text -- ^ 'ratcPublicIP'
    -> RestoreAddressToClassic
restoreAddressToClassic pPublicIP_ =
  RestoreAddressToClassic' {_ratcDryRun = Nothing, _ratcPublicIP = pPublicIP_}


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
ratcDryRun :: Lens' RestoreAddressToClassic (Maybe Bool)
ratcDryRun = lens _ratcDryRun (\ s a -> s{_ratcDryRun = a})

-- | The Elastic IP address.
ratcPublicIP :: Lens' RestoreAddressToClassic Text
ratcPublicIP = lens _ratcPublicIP (\ s a -> s{_ratcPublicIP = a})

instance AWSRequest RestoreAddressToClassic where
        type Rs RestoreAddressToClassic =
             RestoreAddressToClassicResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 RestoreAddressToClassicResponse' <$>
                   (x .@? "status") <*> (x .@? "publicIp") <*>
                     (pure (fromEnum s)))

instance Hashable RestoreAddressToClassic where

instance NFData RestoreAddressToClassic where

instance ToHeaders RestoreAddressToClassic where
        toHeaders = const mempty

instance ToPath RestoreAddressToClassic where
        toPath = const "/"

instance ToQuery RestoreAddressToClassic where
        toQuery RestoreAddressToClassic'{..}
          = mconcat
              ["Action" =:
                 ("RestoreAddressToClassic" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _ratcDryRun, "PublicIp" =: _ratcPublicIP]

-- | /See:/ 'restoreAddressToClassicResponse' smart constructor.
data RestoreAddressToClassicResponse = RestoreAddressToClassicResponse'
  { _ratcrsStatus         :: !(Maybe AddressStatus)
  , _ratcrsPublicIP       :: !(Maybe Text)
  , _ratcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RestoreAddressToClassicResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ratcrsStatus' - The move status for the IP address.
--
-- * 'ratcrsPublicIP' - The Elastic IP address.
--
-- * 'ratcrsResponseStatus' - -- | The response status code.
restoreAddressToClassicResponse
    :: Int -- ^ 'ratcrsResponseStatus'
    -> RestoreAddressToClassicResponse
restoreAddressToClassicResponse pResponseStatus_ =
  RestoreAddressToClassicResponse'
    { _ratcrsStatus = Nothing
    , _ratcrsPublicIP = Nothing
    , _ratcrsResponseStatus = pResponseStatus_
    }


-- | The move status for the IP address.
ratcrsStatus :: Lens' RestoreAddressToClassicResponse (Maybe AddressStatus)
ratcrsStatus = lens _ratcrsStatus (\ s a -> s{_ratcrsStatus = a})

-- | The Elastic IP address.
ratcrsPublicIP :: Lens' RestoreAddressToClassicResponse (Maybe Text)
ratcrsPublicIP = lens _ratcrsPublicIP (\ s a -> s{_ratcrsPublicIP = a})

-- | -- | The response status code.
ratcrsResponseStatus :: Lens' RestoreAddressToClassicResponse Int
ratcrsResponseStatus = lens _ratcrsResponseStatus (\ s a -> s{_ratcrsResponseStatus = a})

instance NFData RestoreAddressToClassicResponse where
