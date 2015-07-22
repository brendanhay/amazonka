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
    , mhrqIAMRoleARN
    , mhrqSubnetId
    , mhrqSyslogIP
    , mhrqExternalId
    , mhrqEniIP
    , mhrqHSMARN

    -- * Response
    , ModifyHSMResponse
    -- ** Response constructor
    , modifyHSMResponse
    -- ** Response lenses
    , mhrsHSMARN
    , mhrsStatus
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
-- * 'mhrqIAMRoleARN'
--
-- * 'mhrqSubnetId'
--
-- * 'mhrqSyslogIP'
--
-- * 'mhrqExternalId'
--
-- * 'mhrqEniIP'
--
-- * 'mhrqHSMARN'
data ModifyHSM = ModifyHSM'
    { _mhrqIAMRoleARN :: !(Maybe Text)
    , _mhrqSubnetId   :: !(Maybe Text)
    , _mhrqSyslogIP   :: !(Maybe Text)
    , _mhrqExternalId :: !(Maybe Text)
    , _mhrqEniIP      :: !(Maybe Text)
    , _mhrqHSMARN     :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifyHSM' smart constructor.
modifyHSM :: Text -> ModifyHSM
modifyHSM pHSMARN =
    ModifyHSM'
    { _mhrqIAMRoleARN = Nothing
    , _mhrqSubnetId = Nothing
    , _mhrqSyslogIP = Nothing
    , _mhrqExternalId = Nothing
    , _mhrqEniIP = Nothing
    , _mhrqHSMARN = pHSMARN
    }

-- | The new IAM role ARN.
mhrqIAMRoleARN :: Lens' ModifyHSM (Maybe Text)
mhrqIAMRoleARN = lens _mhrqIAMRoleARN (\ s a -> s{_mhrqIAMRoleARN = a});

-- | The new identifier of the subnet that the HSM is in.
mhrqSubnetId :: Lens' ModifyHSM (Maybe Text)
mhrqSubnetId = lens _mhrqSubnetId (\ s a -> s{_mhrqSubnetId = a});

-- | The new IP address for the syslog monitoring server.
mhrqSyslogIP :: Lens' ModifyHSM (Maybe Text)
mhrqSyslogIP = lens _mhrqSyslogIP (\ s a -> s{_mhrqSyslogIP = a});

-- | The new external ID.
mhrqExternalId :: Lens' ModifyHSM (Maybe Text)
mhrqExternalId = lens _mhrqExternalId (\ s a -> s{_mhrqExternalId = a});

-- | The new IP address for the elastic network interface attached to the
-- HSM.
mhrqEniIP :: Lens' ModifyHSM (Maybe Text)
mhrqEniIP = lens _mhrqEniIP (\ s a -> s{_mhrqEniIP = a});

-- | The ARN of the HSM to modify.
mhrqHSMARN :: Lens' ModifyHSM Text
mhrqHSMARN = lens _mhrqHSMARN (\ s a -> s{_mhrqHSMARN = a});

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
              ["IamRoleArn" .= _mhrqIAMRoleARN,
               "SubnetId" .= _mhrqSubnetId,
               "SyslogIp" .= _mhrqSyslogIP,
               "ExternalId" .= _mhrqExternalId,
               "EniIp" .= _mhrqEniIP, "HsmArn" .= _mhrqHSMARN]

instance ToPath ModifyHSM where
        toPath = const "/"

instance ToQuery ModifyHSM where
        toQuery = const mempty

-- | Contains the output of the ModifyHsm action.
--
-- /See:/ 'modifyHSMResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mhrsHSMARN'
--
-- * 'mhrsStatus'
data ModifyHSMResponse = ModifyHSMResponse'
    { _mhrsHSMARN :: !(Maybe Text)
    , _mhrsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifyHSMResponse' smart constructor.
modifyHSMResponse :: Int -> ModifyHSMResponse
modifyHSMResponse pStatus =
    ModifyHSMResponse'
    { _mhrsHSMARN = Nothing
    , _mhrsStatus = pStatus
    }

-- | The ARN of the HSM.
mhrsHSMARN :: Lens' ModifyHSMResponse (Maybe Text)
mhrsHSMARN = lens _mhrsHSMARN (\ s a -> s{_mhrsHSMARN = a});

-- | FIXME: Undocumented member.
mhrsStatus :: Lens' ModifyHSMResponse Int
mhrsStatus = lens _mhrsStatus (\ s a -> s{_mhrsStatus = a});
