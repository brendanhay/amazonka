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
-- Module      : Network.AWS.IoT.GetLoggingOptions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the logging options.
--
--
module Network.AWS.IoT.GetLoggingOptions
    (
    -- * Creating a Request
      getLoggingOptions
    , GetLoggingOptions

    -- * Destructuring the Response
    , getLoggingOptionsResponse
    , GetLoggingOptionsResponse
    -- * Response Lenses
    , glorsLogLevel
    , glorsRoleARN
    , glorsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the GetLoggingOptions operation.
--
--
--
-- /See:/ 'getLoggingOptions' smart constructor.
data GetLoggingOptions =
  GetLoggingOptions'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetLoggingOptions' with the minimum fields required to make a request.
--
getLoggingOptions
    :: GetLoggingOptions
getLoggingOptions = GetLoggingOptions'


instance AWSRequest GetLoggingOptions where
        type Rs GetLoggingOptions = GetLoggingOptionsResponse
        request = get ioT
        response
          = receiveJSON
              (\ s h x ->
                 GetLoggingOptionsResponse' <$>
                   (x .?> "logLevel") <*> (x .?> "roleArn") <*>
                     (pure (fromEnum s)))

instance Hashable GetLoggingOptions where

instance NFData GetLoggingOptions where

instance ToHeaders GetLoggingOptions where
        toHeaders = const mempty

instance ToPath GetLoggingOptions where
        toPath = const "/loggingOptions"

instance ToQuery GetLoggingOptions where
        toQuery = const mempty

-- | The output from the GetLoggingOptions operation.
--
--
--
-- /See:/ 'getLoggingOptionsResponse' smart constructor.
data GetLoggingOptionsResponse = GetLoggingOptionsResponse'
  { _glorsLogLevel       :: !(Maybe LogLevel)
  , _glorsRoleARN        :: !(Maybe Text)
  , _glorsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetLoggingOptionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'glorsLogLevel' - The logging level.
--
-- * 'glorsRoleARN' - The ARN of the IAM role that grants access.
--
-- * 'glorsResponseStatus' - -- | The response status code.
getLoggingOptionsResponse
    :: Int -- ^ 'glorsResponseStatus'
    -> GetLoggingOptionsResponse
getLoggingOptionsResponse pResponseStatus_ =
  GetLoggingOptionsResponse'
    { _glorsLogLevel = Nothing
    , _glorsRoleARN = Nothing
    , _glorsResponseStatus = pResponseStatus_
    }


-- | The logging level.
glorsLogLevel :: Lens' GetLoggingOptionsResponse (Maybe LogLevel)
glorsLogLevel = lens _glorsLogLevel (\ s a -> s{_glorsLogLevel = a})

-- | The ARN of the IAM role that grants access.
glorsRoleARN :: Lens' GetLoggingOptionsResponse (Maybe Text)
glorsRoleARN = lens _glorsRoleARN (\ s a -> s{_glorsRoleARN = a})

-- | -- | The response status code.
glorsResponseStatus :: Lens' GetLoggingOptionsResponse Int
glorsResponseStatus = lens _glorsResponseStatus (\ s a -> s{_glorsResponseStatus = a})

instance NFData GetLoggingOptionsResponse where
