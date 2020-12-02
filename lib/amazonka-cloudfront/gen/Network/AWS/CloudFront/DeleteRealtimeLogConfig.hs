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
-- Module      : Network.AWS.CloudFront.DeleteRealtimeLogConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a real-time log configuration.
--
--
-- You cannot delete a real-time log configuration if it’s attached to a cache behavior. First update your distributions to remove the real-time log configuration from all cache behaviors, then delete the real-time log configuration.
--
-- To delete a real-time log configuration, you can provide the configuration’s name or its Amazon Resource Name (ARN). You must provide at least one. If you provide both, CloudFront uses the name to identify the real-time log configuration to delete.
module Network.AWS.CloudFront.DeleteRealtimeLogConfig
  ( -- * Creating a Request
    deleteRealtimeLogConfig,
    DeleteRealtimeLogConfig,

    -- * Request Lenses
    drlcARN,
    drlcName,

    -- * Destructuring the Response
    deleteRealtimeLogConfigResponse,
    DeleteRealtimeLogConfigResponse,
  )
where

import Network.AWS.CloudFront.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteRealtimeLogConfig' smart constructor.
data DeleteRealtimeLogConfig = DeleteRealtimeLogConfig'
  { _drlcARN ::
      !(Maybe Text),
    _drlcName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteRealtimeLogConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drlcARN' - The Amazon Resource Name (ARN) of the real-time log configuration to delete.
--
-- * 'drlcName' - The name of the real-time log configuration to delete.
deleteRealtimeLogConfig ::
  DeleteRealtimeLogConfig
deleteRealtimeLogConfig =
  DeleteRealtimeLogConfig' {_drlcARN = Nothing, _drlcName = Nothing}

-- | The Amazon Resource Name (ARN) of the real-time log configuration to delete.
drlcARN :: Lens' DeleteRealtimeLogConfig (Maybe Text)
drlcARN = lens _drlcARN (\s a -> s {_drlcARN = a})

-- | The name of the real-time log configuration to delete.
drlcName :: Lens' DeleteRealtimeLogConfig (Maybe Text)
drlcName = lens _drlcName (\s a -> s {_drlcName = a})

instance AWSRequest DeleteRealtimeLogConfig where
  type Rs DeleteRealtimeLogConfig = DeleteRealtimeLogConfigResponse
  request = postXML cloudFront
  response = receiveNull DeleteRealtimeLogConfigResponse'

instance Hashable DeleteRealtimeLogConfig

instance NFData DeleteRealtimeLogConfig

instance ToElement DeleteRealtimeLogConfig where
  toElement =
    mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}DeleteRealtimeLogConfigRequest"

instance ToHeaders DeleteRealtimeLogConfig where
  toHeaders = const mempty

instance ToPath DeleteRealtimeLogConfig where
  toPath = const "/2020-05-31/delete-realtime-log-config/"

instance ToQuery DeleteRealtimeLogConfig where
  toQuery = const mempty

instance ToXML DeleteRealtimeLogConfig where
  toXML DeleteRealtimeLogConfig' {..} =
    mconcat ["ARN" @= _drlcARN, "Name" @= _drlcName]

-- | /See:/ 'deleteRealtimeLogConfigResponse' smart constructor.
data DeleteRealtimeLogConfigResponse = DeleteRealtimeLogConfigResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteRealtimeLogConfigResponse' with the minimum fields required to make a request.
deleteRealtimeLogConfigResponse ::
  DeleteRealtimeLogConfigResponse
deleteRealtimeLogConfigResponse = DeleteRealtimeLogConfigResponse'

instance NFData DeleteRealtimeLogConfigResponse
