{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.ModifyHSM
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Modifies an HSM.
--
-- <http://docs.aws.amazon.com/cloudhsm/latest/dg/API_ModifyHSM.html>
module Network.AWS.CloudHSM.ModifyHSM
    (
    -- * Request
      ModifyHSM
    -- ** Request constructor
    , modifyHSM
    -- ** Request lenses
    , mhIAMRoleARN
    , mhSubnetId
    , mhSyslogIP
    , mhExternalId
    , mhEniIP
    , mhHSMARN

    -- * Response
    , ModifyHSMResponse
    -- ** Response constructor
    , modifyHSMResponse
    -- ** Response lenses
    , mhsmrsHSMARN
    , mhsmrsStatus
    ) where

import           Network.AWS.CloudHSM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the inputs for the ModifyHsm action.
--
-- /See:/ 'modifyHSM' smart constructor.
--
-- The fields accessible through corresponding lenses are:
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
data ModifyHSM = ModifyHSM'
    { _mhIAMRoleARN :: !(Maybe Text)
    , _mhSubnetId   :: !(Maybe Text)
    , _mhSyslogIP   :: !(Maybe Text)
    , _mhExternalId :: !(Maybe Text)
    , _mhEniIP      :: !(Maybe Text)
    , _mhHSMARN     :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifyHSM' smart constructor.
modifyHSM :: Text -> ModifyHSM
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

-- | The new identifier of the subnet that the HSM is in.
mhSubnetId :: Lens' ModifyHSM (Maybe Text)
mhSubnetId = lens _mhSubnetId (\ s a -> s{_mhSubnetId = a});

-- | The new IP address for the syslog monitoring server.
mhSyslogIP :: Lens' ModifyHSM (Maybe Text)
mhSyslogIP = lens _mhSyslogIP (\ s a -> s{_mhSyslogIP = a});

-- | The new external ID.
mhExternalId :: Lens' ModifyHSM (Maybe Text)
mhExternalId = lens _mhExternalId (\ s a -> s{_mhExternalId = a});

-- | The new IP address for the elastic network interface attached to the
-- HSM.
mhEniIP :: Lens' ModifyHSM (Maybe Text)
mhEniIP = lens _mhEniIP (\ s a -> s{_mhEniIP = a});

-- | The ARN of the HSM to modify.
mhHSMARN :: Lens' ModifyHSM Text
mhHSMARN = lens _mhHSMARN (\ s a -> s{_mhHSMARN = a});

instance AWSRequest ModifyHSM where
        type Sv ModifyHSM = CloudHSM
        type Rs ModifyHSM = ModifyHSMResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ModifyHSMResponse' <$>
                   (x .?> "HsmArn") <*> (pure (fromEnum s)))

instance ToHeaders ModifyHSM where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CloudHsmFrontendService.ModifyHSM" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ModifyHSM where
        toJSON ModifyHSM'{..}
          = object
              ["IamRoleArn" .= _mhIAMRoleARN,
               "SubnetId" .= _mhSubnetId, "SyslogIp" .= _mhSyslogIP,
               "ExternalId" .= _mhExternalId, "EniIp" .= _mhEniIP,
               "HsmArn" .= _mhHSMARN]

instance ToPath ModifyHSM where
        toPath = const mempty

instance ToQuery ModifyHSM where
        toQuery = const mempty

-- | Contains the output of the ModifyHsm action.
--
-- /See:/ 'modifyHSMResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mhsmrsHSMARN'
--
-- * 'mhsmrsStatus'
data ModifyHSMResponse = ModifyHSMResponse'
    { _mhsmrsHSMARN :: !(Maybe Text)
    , _mhsmrsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifyHSMResponse' smart constructor.
modifyHSMResponse :: Int -> ModifyHSMResponse
modifyHSMResponse pStatus_ =
    ModifyHSMResponse'
    { _mhsmrsHSMARN = Nothing
    , _mhsmrsStatus = pStatus_
    }

-- | The ARN of the HSM.
mhsmrsHSMARN :: Lens' ModifyHSMResponse (Maybe Text)
mhsmrsHSMARN = lens _mhsmrsHSMARN (\ s a -> s{_mhsmrsHSMARN = a});

-- | FIXME: Undocumented member.
mhsmrsStatus :: Lens' ModifyHSMResponse Int
mhsmrsStatus = lens _mhsmrsStatus (\ s a -> s{_mhsmrsStatus = a});
