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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__ . For more information, see <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs> , the <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference> .
--
--
-- __For information about the current version of AWS CloudHSM__ , see <http://aws.amazon.com/cloudhsm/ AWS CloudHSM> , the <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference> .
--
-- Gets the configuration files necessary to connect to all high availability partition groups the client is associated with.
--
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
    , gcrsResponseStatus
    ) where

import Network.AWS.CloudHSM.Types
import Network.AWS.CloudHSM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getConfig' smart constructor.
data GetConfig = GetConfig'
  { _gcClientARN     :: !Text
  , _gcClientVersion :: !ClientVersion
  , _gcHAPGList      :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcClientARN' - The ARN of the client.
--
-- * 'gcClientVersion' - The client version.
--
-- * 'gcHAPGList' - A list of ARNs that identify the high-availability partition groups that are associated with the client.
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
gcClientARN = lens _gcClientARN (\ s a -> s{_gcClientARN = a})

-- | The client version.
gcClientVersion :: Lens' GetConfig ClientVersion
gcClientVersion = lens _gcClientVersion (\ s a -> s{_gcClientVersion = a})

-- | A list of ARNs that identify the high-availability partition groups that are associated with the client.
gcHAPGList :: Lens' GetConfig [Text]
gcHAPGList = lens _gcHAPGList (\ s a -> s{_gcHAPGList = a}) . _Coerce

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

instance Hashable GetConfig where

instance NFData GetConfig where

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
  { _gcrsConfigFile     :: !(Maybe Text)
  , _gcrsConfigCred     :: !(Maybe Text)
  , _gcrsConfigType     :: !(Maybe Text)
  , _gcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcrsConfigFile' - The chrystoki.conf configuration file.
--
-- * 'gcrsConfigCred' - The certificate file containing the server.pem files of the HSMs.
--
-- * 'gcrsConfigType' - The type of credentials.
--
-- * 'gcrsResponseStatus' - -- | The response status code.
getConfigResponse
    :: Int -- ^ 'gcrsResponseStatus'
    -> GetConfigResponse
getConfigResponse pResponseStatus_ =
  GetConfigResponse'
    { _gcrsConfigFile = Nothing
    , _gcrsConfigCred = Nothing
    , _gcrsConfigType = Nothing
    , _gcrsResponseStatus = pResponseStatus_
    }


-- | The chrystoki.conf configuration file.
gcrsConfigFile :: Lens' GetConfigResponse (Maybe Text)
gcrsConfigFile = lens _gcrsConfigFile (\ s a -> s{_gcrsConfigFile = a})

-- | The certificate file containing the server.pem files of the HSMs.
gcrsConfigCred :: Lens' GetConfigResponse (Maybe Text)
gcrsConfigCred = lens _gcrsConfigCred (\ s a -> s{_gcrsConfigCred = a})

-- | The type of credentials.
gcrsConfigType :: Lens' GetConfigResponse (Maybe Text)
gcrsConfigType = lens _gcrsConfigType (\ s a -> s{_gcrsConfigType = a})

-- | -- | The response status code.
gcrsResponseStatus :: Lens' GetConfigResponse Int
gcrsResponseStatus = lens _gcrsResponseStatus (\ s a -> s{_gcrsResponseStatus = a})

instance NFData GetConfigResponse where
