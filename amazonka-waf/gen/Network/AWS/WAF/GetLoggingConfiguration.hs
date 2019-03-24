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
-- Module      : Network.AWS.WAF.GetLoggingConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the 'LoggingConfiguration' for the specified web ACL.
--
--
module Network.AWS.WAF.GetLoggingConfiguration
    (
    -- * Creating a Request
      getLoggingConfiguration
    , GetLoggingConfiguration
    -- * Request Lenses
    , glcResourceARN

    -- * Destructuring the Response
    , getLoggingConfigurationResponse
    , GetLoggingConfigurationResponse
    -- * Response Lenses
    , glcrsLoggingConfiguration
    , glcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAF.Types
import Network.AWS.WAF.Types.Product

-- | /See:/ 'getLoggingConfiguration' smart constructor.
newtype GetLoggingConfiguration = GetLoggingConfiguration'
  { _glcResourceARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetLoggingConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'glcResourceARN' - The Amazon Resource Name (ARN) of the web ACL for which you want to get the 'LoggingConfiguration' .
getLoggingConfiguration
    :: Text -- ^ 'glcResourceARN'
    -> GetLoggingConfiguration
getLoggingConfiguration pResourceARN_ =
  GetLoggingConfiguration' {_glcResourceARN = pResourceARN_}


-- | The Amazon Resource Name (ARN) of the web ACL for which you want to get the 'LoggingConfiguration' .
glcResourceARN :: Lens' GetLoggingConfiguration Text
glcResourceARN = lens _glcResourceARN (\ s a -> s{_glcResourceARN = a})

instance AWSRequest GetLoggingConfiguration where
        type Rs GetLoggingConfiguration =
             GetLoggingConfigurationResponse
        request = postJSON waf
        response
          = receiveJSON
              (\ s h x ->
                 GetLoggingConfigurationResponse' <$>
                   (x .?> "LoggingConfiguration") <*>
                     (pure (fromEnum s)))

instance Hashable GetLoggingConfiguration where

instance NFData GetLoggingConfiguration where

instance ToHeaders GetLoggingConfiguration where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_20150824.GetLoggingConfiguration" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetLoggingConfiguration where
        toJSON GetLoggingConfiguration'{..}
          = object
              (catMaybes [Just ("ResourceArn" .= _glcResourceARN)])

instance ToPath GetLoggingConfiguration where
        toPath = const "/"

instance ToQuery GetLoggingConfiguration where
        toQuery = const mempty

-- | /See:/ 'getLoggingConfigurationResponse' smart constructor.
data GetLoggingConfigurationResponse = GetLoggingConfigurationResponse'
  { _glcrsLoggingConfiguration :: !(Maybe LoggingConfiguration)
  , _glcrsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetLoggingConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'glcrsLoggingConfiguration' - The 'LoggingConfiguration' for the specified web ACL.
--
-- * 'glcrsResponseStatus' - -- | The response status code.
getLoggingConfigurationResponse
    :: Int -- ^ 'glcrsResponseStatus'
    -> GetLoggingConfigurationResponse
getLoggingConfigurationResponse pResponseStatus_ =
  GetLoggingConfigurationResponse'
    { _glcrsLoggingConfiguration = Nothing
    , _glcrsResponseStatus = pResponseStatus_
    }


-- | The 'LoggingConfiguration' for the specified web ACL.
glcrsLoggingConfiguration :: Lens' GetLoggingConfigurationResponse (Maybe LoggingConfiguration)
glcrsLoggingConfiguration = lens _glcrsLoggingConfiguration (\ s a -> s{_glcrsLoggingConfiguration = a})

-- | -- | The response status code.
glcrsResponseStatus :: Lens' GetLoggingConfigurationResponse Int
glcrsResponseStatus = lens _glcrsResponseStatus (\ s a -> s{_glcrsResponseStatus = a})

instance NFData GetLoggingConfigurationResponse where
