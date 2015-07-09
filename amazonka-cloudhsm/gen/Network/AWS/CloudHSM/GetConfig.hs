{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.GetConfig
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Gets the configuration files necessary to connect to all high
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
    , gcrConfigFile
    , gcrConfigCred
    , gcrConfigType
    , gcrStatus
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
getConfig pClientARN pClientVersion =
    GetConfig'
    { _gcClientARN = pClientARN
    , _gcClientVersion = pClientVersion
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
        request = postJSON
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
-- * 'gcrConfigFile'
--
-- * 'gcrConfigCred'
--
-- * 'gcrConfigType'
--
-- * 'gcrStatus'
data GetConfigResponse = GetConfigResponse'
    { _gcrConfigFile :: !(Maybe Text)
    , _gcrConfigCred :: !(Maybe Text)
    , _gcrConfigType :: !(Maybe Text)
    , _gcrStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetConfigResponse' smart constructor.
getConfigResponse :: Int -> GetConfigResponse
getConfigResponse pStatus =
    GetConfigResponse'
    { _gcrConfigFile = Nothing
    , _gcrConfigCred = Nothing
    , _gcrConfigType = Nothing
    , _gcrStatus = pStatus
    }

-- | The chrystoki.conf configuration file.
gcrConfigFile :: Lens' GetConfigResponse (Maybe Text)
gcrConfigFile = lens _gcrConfigFile (\ s a -> s{_gcrConfigFile = a});

-- | The certificate file containing the server.pem files of the HSMs.
gcrConfigCred :: Lens' GetConfigResponse (Maybe Text)
gcrConfigCred = lens _gcrConfigCred (\ s a -> s{_gcrConfigCred = a});

-- | The type of credentials.
gcrConfigType :: Lens' GetConfigResponse (Maybe Text)
gcrConfigType = lens _gcrConfigType (\ s a -> s{_gcrConfigType = a});

-- | FIXME: Undocumented member.
gcrStatus :: Lens' GetConfigResponse Int
gcrStatus = lens _gcrStatus (\ s a -> s{_gcrStatus = a});
