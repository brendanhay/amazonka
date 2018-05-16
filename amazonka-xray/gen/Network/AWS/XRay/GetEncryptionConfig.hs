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
-- Module      : Network.AWS.XRay.GetEncryptionConfig
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the current encryption configuration for X-Ray data.
--
--
module Network.AWS.XRay.GetEncryptionConfig
    (
    -- * Creating a Request
      getEncryptionConfig
    , GetEncryptionConfig

    -- * Destructuring the Response
    , getEncryptionConfigResponse
    , GetEncryptionConfigResponse
    -- * Response Lenses
    , gecrsEncryptionConfig
    , gecrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.XRay.Types
import Network.AWS.XRay.Types.Product

-- | /See:/ 'getEncryptionConfig' smart constructor.
data GetEncryptionConfig =
  GetEncryptionConfig'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetEncryptionConfig' with the minimum fields required to make a request.
--
getEncryptionConfig
    :: GetEncryptionConfig
getEncryptionConfig = GetEncryptionConfig'


instance AWSRequest GetEncryptionConfig where
        type Rs GetEncryptionConfig =
             GetEncryptionConfigResponse
        request = postJSON xRay
        response
          = receiveJSON
              (\ s h x ->
                 GetEncryptionConfigResponse' <$>
                   (x .?> "EncryptionConfig") <*> (pure (fromEnum s)))

instance Hashable GetEncryptionConfig where

instance NFData GetEncryptionConfig where

instance ToHeaders GetEncryptionConfig where
        toHeaders = const mempty

instance ToJSON GetEncryptionConfig where
        toJSON = const (Object mempty)

instance ToPath GetEncryptionConfig where
        toPath = const "/EncryptionConfig"

instance ToQuery GetEncryptionConfig where
        toQuery = const mempty

-- | /See:/ 'getEncryptionConfigResponse' smart constructor.
data GetEncryptionConfigResponse = GetEncryptionConfigResponse'
  { _gecrsEncryptionConfig :: !(Maybe EncryptionConfig)
  , _gecrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetEncryptionConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gecrsEncryptionConfig' - The encryption configuration document.
--
-- * 'gecrsResponseStatus' - -- | The response status code.
getEncryptionConfigResponse
    :: Int -- ^ 'gecrsResponseStatus'
    -> GetEncryptionConfigResponse
getEncryptionConfigResponse pResponseStatus_ =
  GetEncryptionConfigResponse'
    {_gecrsEncryptionConfig = Nothing, _gecrsResponseStatus = pResponseStatus_}


-- | The encryption configuration document.
gecrsEncryptionConfig :: Lens' GetEncryptionConfigResponse (Maybe EncryptionConfig)
gecrsEncryptionConfig = lens _gecrsEncryptionConfig (\ s a -> s{_gecrsEncryptionConfig = a})

-- | -- | The response status code.
gecrsResponseStatus :: Lens' GetEncryptionConfigResponse Int
gecrsResponseStatus = lens _gecrsResponseStatus (\ s a -> s{_gecrsResponseStatus = a})

instance NFData GetEncryptionConfigResponse where
