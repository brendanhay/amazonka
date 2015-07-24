{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.GetConfig
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Gets the configuration files necessary to connect to all high
-- availability partition groups the client is associated with.
--
-- <http://docs.aws.amazon.com/cloudhsm/latest/dg/API_GetConfig.html>
module Network.AWS.CloudHSM.GetConfig
    (
    -- * Request
      GetConfig
    -- ** Request constructor
    , getConfig
    -- ** Request lenses
    , gcClientARN
    , gcClientVersion
    , gcHAPGList

    -- * Response
    , GetConfigResponse
    -- ** Response constructor
    , getConfigResponse
    -- ** Response lenses
    , gcrsConfigFile
    , gcrsConfigCred
    , gcrsConfigType
    , gcrsStatus
    ) where

import           Network.AWS.CloudHSM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getConfig' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gcClientARN'
--
-- * 'gcClientVersion'
--
-- * 'gcHAPGList'
data GetConfig = GetConfig'
    { _gcClientARN     :: !Text
    , _gcClientVersion :: !ClientVersion
    , _gcHAPGList      :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetConfig' smart constructor.
getConfig :: Text -> ClientVersion -> GetConfig
getConfig pClientARN_ pClientVersion_ =
    GetConfig'
    { _gcClientARN = pClientARN_
    , _gcClientVersion = pClientVersion_
    , _gcHAPGList = mempty
    }

-- | The ARN of the client.
gcClientARN :: Lens' GetConfig Text
gcClientARN = lens _gcClientARN (\ s a -> s{_gcClientARN = a});

-- | The client version.
gcClientVersion :: Lens' GetConfig ClientVersion
gcClientVersion = lens _gcClientVersion (\ s a -> s{_gcClientVersion = a});

-- | A list of ARNs that identify the high-availability partition groups that
-- are associated with the client.
gcHAPGList :: Lens' GetConfig [Text]
gcHAPGList = lens _gcHAPGList (\ s a -> s{_gcHAPGList = a});

instance AWSRequest GetConfig where
        type Sv GetConfig = CloudHSM
        type Rs GetConfig = GetConfigResponse
        request = postJSON "GetConfig"
        response
          = receiveJSON
              (\ s h x ->
                 GetConfigResponse' <$>
                   (x .?> "ConfigFile") <*> (x .?> "ConfigCred") <*>
                     (x .?> "ConfigType")
                     <*> (pure (fromEnum s)))

instance ToHeaders GetConfig where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CloudHsmFrontendService.GetConfig" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetConfig where
        toJSON GetConfig'{..}
          = object
              ["ClientArn" .= _gcClientARN,
               "ClientVersion" .= _gcClientVersion,
               "HapgList" .= _gcHAPGList]

instance ToPath GetConfig where
        toPath = const "/"

instance ToQuery GetConfig where
        toQuery = const mempty

-- | /See:/ 'getConfigResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gcrsConfigFile'
--
-- * 'gcrsConfigCred'
--
-- * 'gcrsConfigType'
--
-- * 'gcrsStatus'
data GetConfigResponse = GetConfigResponse'
    { _gcrsConfigFile :: !(Maybe Text)
    , _gcrsConfigCred :: !(Maybe Text)
    , _gcrsConfigType :: !(Maybe Text)
    , _gcrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetConfigResponse' smart constructor.
getConfigResponse :: Int -> GetConfigResponse
getConfigResponse pStatus_ =
    GetConfigResponse'
    { _gcrsConfigFile = Nothing
    , _gcrsConfigCred = Nothing
    , _gcrsConfigType = Nothing
    , _gcrsStatus = pStatus_
    }

-- | The chrystoki.conf configuration file.
gcrsConfigFile :: Lens' GetConfigResponse (Maybe Text)
gcrsConfigFile = lens _gcrsConfigFile (\ s a -> s{_gcrsConfigFile = a});

-- | The certificate file containing the server.pem files of the HSMs.
gcrsConfigCred :: Lens' GetConfigResponse (Maybe Text)
gcrsConfigCred = lens _gcrsConfigCred (\ s a -> s{_gcrsConfigCred = a});

-- | The type of credentials.
gcrsConfigType :: Lens' GetConfigResponse (Maybe Text)
gcrsConfigType = lens _gcrsConfigType (\ s a -> s{_gcrsConfigType = a});

-- | FIXME: Undocumented member.
gcrsStatus :: Lens' GetConfigResponse Int
gcrsStatus = lens _gcrsStatus (\ s a -> s{_gcrsStatus = a});
