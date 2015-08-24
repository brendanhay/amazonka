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
-- Module      : Network.AWS.CloudHSM.GetConfig
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the configuration files necessary to connect to all high
-- availability partition groups the client is associated with.
--
-- /See:/ <http://docs.aws.amazon.com/cloudhsm/latest/dg/API_GetConfig.html AWS API Reference> for GetConfig.
module Network.AWS.CloudHSM.GetConfig
    (
    -- * Creating a Request
      getConfig
    , GetConfig
    -- * Request Lenses
    , gcClientARN
    , gcClientVersion
    , gcHAPGList

    -- * Destructuring the Response
    , getConfigResponse
    , GetConfigResponse
    -- * Response Lenses
    , gcrsConfigFile
    , gcrsConfigCred
    , gcrsConfigType
    , gcrsStatus
    ) where

import           Network.AWS.CloudHSM.Types
import           Network.AWS.CloudHSM.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getConfig' smart constructor.
data GetConfig = GetConfig'
    { _gcClientARN     :: !Text
    , _gcClientVersion :: !ClientVersion
    , _gcHAPGList      :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcClientARN'
--
-- * 'gcClientVersion'
--
-- * 'gcHAPGList'
getConfig
    :: Text -- ^ 'gcClientARN'
    -> ClientVersion -- ^ 'gcClientVersion'
    -> GetConfig
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
gcHAPGList = lens _gcHAPGList (\ s a -> s{_gcHAPGList = a}) . _Coerce;

instance AWSRequest GetConfig where
        type Rs GetConfig = GetConfigResponse
        request = postJSON cloudHSM
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
              (catMaybes
                 [Just ("ClientArn" .= _gcClientARN),
                  Just ("ClientVersion" .= _gcClientVersion),
                  Just ("HapgList" .= _gcHAPGList)])

instance ToPath GetConfig where
        toPath = const "/"

instance ToQuery GetConfig where
        toQuery = const mempty

-- | /See:/ 'getConfigResponse' smart constructor.
data GetConfigResponse = GetConfigResponse'
    { _gcrsConfigFile :: !(Maybe Text)
    , _gcrsConfigCred :: !(Maybe Text)
    , _gcrsConfigType :: !(Maybe Text)
    , _gcrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcrsConfigFile'
--
-- * 'gcrsConfigCred'
--
-- * 'gcrsConfigType'
--
-- * 'gcrsStatus'
getConfigResponse
    :: Int -- ^ 'gcrsStatus'
    -> GetConfigResponse
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

-- | The response status code.
gcrsStatus :: Lens' GetConfigResponse Int
gcrsStatus = lens _gcrsStatus (\ s a -> s{_gcrsStatus = a});
