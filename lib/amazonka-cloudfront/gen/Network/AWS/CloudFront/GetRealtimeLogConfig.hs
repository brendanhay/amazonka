{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.GetRealtimeLogConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a real-time log configuration.
--
--
-- To get a real-time log configuration, you can provide the configuration’s name or its Amazon Resource Name (ARN). You must provide at least one. If you provide both, CloudFront uses the name to identify the real-time log configuration to get.
module Network.AWS.CloudFront.GetRealtimeLogConfig
  ( -- * Creating a Request
    getRealtimeLogConfig,
    GetRealtimeLogConfig,

    -- * Request Lenses
    grlcARN,
    grlcName,

    -- * Destructuring the Response
    getRealtimeLogConfigResponse,
    GetRealtimeLogConfigResponse,

    -- * Response Lenses
    grlcrsRealtimeLogConfig,
    grlcrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getRealtimeLogConfig' smart constructor.
data GetRealtimeLogConfig = GetRealtimeLogConfig'
  { _grlcARN ::
      !(Maybe Text),
    _grlcName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetRealtimeLogConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grlcARN' - The Amazon Resource Name (ARN) of the real-time log configuration to get.
--
-- * 'grlcName' - The name of the real-time log configuration to get.
getRealtimeLogConfig ::
  GetRealtimeLogConfig
getRealtimeLogConfig =
  GetRealtimeLogConfig' {_grlcARN = Nothing, _grlcName = Nothing}

-- | The Amazon Resource Name (ARN) of the real-time log configuration to get.
grlcARN :: Lens' GetRealtimeLogConfig (Maybe Text)
grlcARN = lens _grlcARN (\s a -> s {_grlcARN = a})

-- | The name of the real-time log configuration to get.
grlcName :: Lens' GetRealtimeLogConfig (Maybe Text)
grlcName = lens _grlcName (\s a -> s {_grlcName = a})

instance AWSRequest GetRealtimeLogConfig where
  type Rs GetRealtimeLogConfig = GetRealtimeLogConfigResponse
  request = postXML cloudFront
  response =
    receiveXML
      ( \s h x ->
          GetRealtimeLogConfigResponse'
            <$> (x .@? "RealtimeLogConfig") <*> (pure (fromEnum s))
      )

instance Hashable GetRealtimeLogConfig

instance NFData GetRealtimeLogConfig

instance ToElement GetRealtimeLogConfig where
  toElement =
    mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}GetRealtimeLogConfigRequest"

instance ToHeaders GetRealtimeLogConfig where
  toHeaders = const mempty

instance ToPath GetRealtimeLogConfig where
  toPath = const "/2020-05-31/get-realtime-log-config/"

instance ToQuery GetRealtimeLogConfig where
  toQuery = const mempty

instance ToXML GetRealtimeLogConfig where
  toXML GetRealtimeLogConfig' {..} =
    mconcat ["ARN" @= _grlcARN, "Name" @= _grlcName]

-- | /See:/ 'getRealtimeLogConfigResponse' smart constructor.
data GetRealtimeLogConfigResponse = GetRealtimeLogConfigResponse'
  { _grlcrsRealtimeLogConfig ::
      !(Maybe RealtimeLogConfig),
    _grlcrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetRealtimeLogConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grlcrsRealtimeLogConfig' - A real-time log configuration.
--
-- * 'grlcrsResponseStatus' - -- | The response status code.
getRealtimeLogConfigResponse ::
  -- | 'grlcrsResponseStatus'
  Int ->
  GetRealtimeLogConfigResponse
getRealtimeLogConfigResponse pResponseStatus_ =
  GetRealtimeLogConfigResponse'
    { _grlcrsRealtimeLogConfig = Nothing,
      _grlcrsResponseStatus = pResponseStatus_
    }

-- | A real-time log configuration.
grlcrsRealtimeLogConfig :: Lens' GetRealtimeLogConfigResponse (Maybe RealtimeLogConfig)
grlcrsRealtimeLogConfig = lens _grlcrsRealtimeLogConfig (\s a -> s {_grlcrsRealtimeLogConfig = a})

-- | -- | The response status code.
grlcrsResponseStatus :: Lens' GetRealtimeLogConfigResponse Int
grlcrsResponseStatus = lens _grlcrsResponseStatus (\s a -> s {_grlcrsResponseStatus = a})

instance NFData GetRealtimeLogConfigResponse
