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
-- Module      : Network.AWS.EC2.GetPasswordData
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the encrypted administrator password for an instance running Windows.
--
-- The Windows password is generated at boot if the 'EC2Config' service plugin, 'Ec2SetPassword', is enabled. This usually only happens the first time an AMI is launched, and then 'Ec2SetPassword' is automatically disabled. The password is not generated for rebundled AMIs unless 'Ec2SetPassword' is enabled before bundling.
--
-- The password is encrypted using the key pair that you specified when you launched the instance. You must provide the corresponding key pair file.
--
-- Password generation and encryption takes a few moments. We recommend that you wait up to 15 minutes after launching an instance before trying to retrieve the generated password.
module Network.AWS.EC2.GetPasswordData
    (
    -- * Creating a Request
      getPasswordData
    , GetPasswordData
    -- * Request Lenses
    , gpdDryRun
    , gpdInstanceId

    -- * Destructuring the Response
    , getPasswordDataResponse
    , GetPasswordDataResponse
    -- * Response Lenses
    , gpdrsResponseStatus
    , gpdrsInstanceId
    , gpdrsPasswordData
    , gpdrsTimestamp
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for GetPasswordData.
--
-- /See:/ 'getPasswordData' smart constructor.
data GetPasswordData = GetPasswordData'
    { _gpdDryRun     :: !(Maybe Bool)
    , _gpdInstanceId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetPasswordData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpdDryRun'
--
-- * 'gpdInstanceId'
getPasswordData
    :: Text -- ^ 'gpdInstanceId'
    -> GetPasswordData
getPasswordData pInstanceId_ =
    GetPasswordData'
    { _gpdDryRun = Nothing
    , _gpdInstanceId = pInstanceId_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
gpdDryRun :: Lens' GetPasswordData (Maybe Bool)
gpdDryRun = lens _gpdDryRun (\ s a -> s{_gpdDryRun = a});

-- | The ID of the Windows instance.
gpdInstanceId :: Lens' GetPasswordData Text
gpdInstanceId = lens _gpdInstanceId (\ s a -> s{_gpdInstanceId = a});

instance AWSRequest GetPasswordData where
        type Rs GetPasswordData = GetPasswordDataResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 GetPasswordDataResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "instanceId") <*>
                     (x .@ "passwordData")
                     <*> (x .@ "timestamp"))

instance Hashable GetPasswordData

instance NFData GetPasswordData

instance ToHeaders GetPasswordData where
        toHeaders = const mempty

instance ToPath GetPasswordData where
        toPath = const "/"

instance ToQuery GetPasswordData where
        toQuery GetPasswordData'{..}
          = mconcat
              ["Action" =: ("GetPasswordData" :: ByteString),
               "Version" =: ("2015-10-01" :: ByteString),
               "DryRun" =: _gpdDryRun,
               "InstanceId" =: _gpdInstanceId]

-- | Contains the output of GetPasswordData.
--
-- /See:/ 'getPasswordDataResponse' smart constructor.
data GetPasswordDataResponse = GetPasswordDataResponse'
    { _gpdrsResponseStatus :: !Int
    , _gpdrsInstanceId     :: !Text
    , _gpdrsPasswordData   :: !Text
    , _gpdrsTimestamp      :: !ISO8601
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetPasswordDataResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpdrsResponseStatus'
--
-- * 'gpdrsInstanceId'
--
-- * 'gpdrsPasswordData'
--
-- * 'gpdrsTimestamp'
getPasswordDataResponse
    :: Int -- ^ 'gpdrsResponseStatus'
    -> Text -- ^ 'gpdrsInstanceId'
    -> Text -- ^ 'gpdrsPasswordData'
    -> UTCTime -- ^ 'gpdrsTimestamp'
    -> GetPasswordDataResponse
getPasswordDataResponse pResponseStatus_ pInstanceId_ pPasswordData_ pTimestamp_ =
    GetPasswordDataResponse'
    { _gpdrsResponseStatus = pResponseStatus_
    , _gpdrsInstanceId = pInstanceId_
    , _gpdrsPasswordData = pPasswordData_
    , _gpdrsTimestamp = _Time # pTimestamp_
    }

-- | The response status code.
gpdrsResponseStatus :: Lens' GetPasswordDataResponse Int
gpdrsResponseStatus = lens _gpdrsResponseStatus (\ s a -> s{_gpdrsResponseStatus = a});

-- | The ID of the Windows instance.
gpdrsInstanceId :: Lens' GetPasswordDataResponse Text
gpdrsInstanceId = lens _gpdrsInstanceId (\ s a -> s{_gpdrsInstanceId = a});

-- | The password of the instance.
gpdrsPasswordData :: Lens' GetPasswordDataResponse Text
gpdrsPasswordData = lens _gpdrsPasswordData (\ s a -> s{_gpdrsPasswordData = a});

-- | The time the data was last updated.
gpdrsTimestamp :: Lens' GetPasswordDataResponse UTCTime
gpdrsTimestamp = lens _gpdrsTimestamp (\ s a -> s{_gpdrsTimestamp = a}) . _Time;

instance NFData GetPasswordDataResponse
