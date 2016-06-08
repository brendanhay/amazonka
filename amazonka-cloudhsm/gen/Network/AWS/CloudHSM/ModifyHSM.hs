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
-- Module      : Network.AWS.CloudHSM.ModifyHSM
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies an HSM.
--
-- This operation can result in the HSM being offline for up to 15 minutes while the AWS CloudHSM service is reconfigured. If you are modifying a production HSM, you should ensure that your AWS CloudHSM service is configured for high availability, and consider executing this operation during a maintenance window.
module Network.AWS.CloudHSM.ModifyHSM
    (
    -- * Creating a Request
      modifyHSM
    , ModifyHSM
    -- * Request Lenses
    , mhIAMRoleARN
    , mhSubnetId
    , mhSyslogIP
    , mhExternalId
    , mhEniIP
    , mhHSMARN

    -- * Destructuring the Response
    , modifyHSMResponse
    , ModifyHSMResponse
    -- * Response Lenses
    , mhsmrsHSMARN
    , mhsmrsResponseStatus
    ) where

import           Network.AWS.CloudHSM.Types
import           Network.AWS.CloudHSM.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the inputs for the < ModifyHsm> operation.
--
-- /See:/ 'modifyHSM' smart constructor.
data ModifyHSM = ModifyHSM'
    { _mhIAMRoleARN :: !(Maybe Text)
    , _mhSubnetId   :: !(Maybe Text)
    , _mhSyslogIP   :: !(Maybe Text)
    , _mhExternalId :: !(Maybe Text)
    , _mhEniIP      :: !(Maybe Text)
    , _mhHSMARN     :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ModifyHSM' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mhIAMRoleARN'
--
-- * 'mhSubnetId'
--
-- * 'mhSyslogIP'
--
-- * 'mhExternalId'
--
-- * 'mhEniIP'
--
-- * 'mhHSMARN'
modifyHSM
    :: Text -- ^ 'mhHSMARN'
    -> ModifyHSM
modifyHSM pHSMARN_ =
    ModifyHSM'
    { _mhIAMRoleARN = Nothing
    , _mhSubnetId = Nothing
    , _mhSyslogIP = Nothing
    , _mhExternalId = Nothing
    , _mhEniIP = Nothing
    , _mhHSMARN = pHSMARN_
    }

-- | The new IAM role ARN.
mhIAMRoleARN :: Lens' ModifyHSM (Maybe Text)
mhIAMRoleARN = lens _mhIAMRoleARN (\ s a -> s{_mhIAMRoleARN = a});

-- | The new identifier of the subnet that the HSM is in. The new subnet must be in the same Availability Zone as the current subnet.
mhSubnetId :: Lens' ModifyHSM (Maybe Text)
mhSubnetId = lens _mhSubnetId (\ s a -> s{_mhSubnetId = a});

-- | The new IP address for the syslog monitoring server. The AWS CloudHSM service only supports one syslog monitoring server.
mhSyslogIP :: Lens' ModifyHSM (Maybe Text)
mhSyslogIP = lens _mhSyslogIP (\ s a -> s{_mhSyslogIP = a});

-- | The new external ID.
mhExternalId :: Lens' ModifyHSM (Maybe Text)
mhExternalId = lens _mhExternalId (\ s a -> s{_mhExternalId = a});

-- | The new IP address for the elastic network interface (ENI) attached to the HSM.
--
-- If the HSM is moved to a different subnet, and an IP address is not specified, an IP address will be randomly chosen from the CIDR range of the new subnet.
mhEniIP :: Lens' ModifyHSM (Maybe Text)
mhEniIP = lens _mhEniIP (\ s a -> s{_mhEniIP = a});

-- | The ARN of the HSM to modify.
mhHSMARN :: Lens' ModifyHSM Text
mhHSMARN = lens _mhHSMARN (\ s a -> s{_mhHSMARN = a});

instance AWSRequest ModifyHSM where
        type Rs ModifyHSM = ModifyHSMResponse
        request = postJSON cloudHSM
        response
          = receiveJSON
              (\ s h x ->
                 ModifyHSMResponse' <$>
                   (x .?> "HsmArn") <*> (pure (fromEnum s)))

instance Hashable ModifyHSM

instance NFData ModifyHSM

instance ToHeaders ModifyHSM where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CloudHsmFrontendService.ModifyHsm" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ModifyHSM where
        toJSON ModifyHSM'{..}
          = object
              (catMaybes
                 [("IamRoleArn" .=) <$> _mhIAMRoleARN,
                  ("SubnetId" .=) <$> _mhSubnetId,
                  ("SyslogIp" .=) <$> _mhSyslogIP,
                  ("ExternalId" .=) <$> _mhExternalId,
                  ("EniIp" .=) <$> _mhEniIP,
                  Just ("HsmArn" .= _mhHSMARN)])

instance ToPath ModifyHSM where
        toPath = const "/"

instance ToQuery ModifyHSM where
        toQuery = const mempty

-- | Contains the output of the < ModifyHsm> operation.
--
-- /See:/ 'modifyHSMResponse' smart constructor.
data ModifyHSMResponse = ModifyHSMResponse'
    { _mhsmrsHSMARN         :: !(Maybe Text)
    , _mhsmrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ModifyHSMResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mhsmrsHSMARN'
--
-- * 'mhsmrsResponseStatus'
modifyHSMResponse
    :: Int -- ^ 'mhsmrsResponseStatus'
    -> ModifyHSMResponse
modifyHSMResponse pResponseStatus_ =
    ModifyHSMResponse'
    { _mhsmrsHSMARN = Nothing
    , _mhsmrsResponseStatus = pResponseStatus_
    }

-- | The ARN of the HSM.
mhsmrsHSMARN :: Lens' ModifyHSMResponse (Maybe Text)
mhsmrsHSMARN = lens _mhsmrsHSMARN (\ s a -> s{_mhsmrsHSMARN = a});

-- | The response status code.
mhsmrsResponseStatus :: Lens' ModifyHSMResponse Int
mhsmrsResponseStatus = lens _mhsmrsResponseStatus (\ s a -> s{_mhsmrsResponseStatus = a});

instance NFData ModifyHSMResponse
