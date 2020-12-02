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
-- Module      : Network.AWS.WAF.DeleteLoggingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes the 'LoggingConfiguration' from the specified web ACL.
module Network.AWS.WAF.DeleteLoggingConfiguration
  ( -- * Creating a Request
    deleteLoggingConfiguration,
    DeleteLoggingConfiguration,

    -- * Request Lenses
    dlcResourceARN,

    -- * Destructuring the Response
    deleteLoggingConfigurationResponse,
    DeleteLoggingConfigurationResponse,

    -- * Response Lenses
    dlcrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAF.Types

-- | /See:/ 'deleteLoggingConfiguration' smart constructor.
newtype DeleteLoggingConfiguration = DeleteLoggingConfiguration'
  { _dlcResourceARN ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteLoggingConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlcResourceARN' - The Amazon Resource Name (ARN) of the web ACL from which you want to delete the 'LoggingConfiguration' .
deleteLoggingConfiguration ::
  -- | 'dlcResourceARN'
  Text ->
  DeleteLoggingConfiguration
deleteLoggingConfiguration pResourceARN_ =
  DeleteLoggingConfiguration' {_dlcResourceARN = pResourceARN_}

-- | The Amazon Resource Name (ARN) of the web ACL from which you want to delete the 'LoggingConfiguration' .
dlcResourceARN :: Lens' DeleteLoggingConfiguration Text
dlcResourceARN = lens _dlcResourceARN (\s a -> s {_dlcResourceARN = a})

instance AWSRequest DeleteLoggingConfiguration where
  type
    Rs DeleteLoggingConfiguration =
      DeleteLoggingConfigurationResponse
  request = postJSON waf
  response =
    receiveEmpty
      ( \s h x ->
          DeleteLoggingConfigurationResponse' <$> (pure (fromEnum s))
      )

instance Hashable DeleteLoggingConfiguration

instance NFData DeleteLoggingConfiguration

instance ToHeaders DeleteLoggingConfiguration where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSWAF_20150824.DeleteLoggingConfiguration" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteLoggingConfiguration where
  toJSON DeleteLoggingConfiguration' {..} =
    object (catMaybes [Just ("ResourceArn" .= _dlcResourceARN)])

instance ToPath DeleteLoggingConfiguration where
  toPath = const "/"

instance ToQuery DeleteLoggingConfiguration where
  toQuery = const mempty

-- | /See:/ 'deleteLoggingConfigurationResponse' smart constructor.
newtype DeleteLoggingConfigurationResponse = DeleteLoggingConfigurationResponse'
  { _dlcrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteLoggingConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlcrsResponseStatus' - -- | The response status code.
deleteLoggingConfigurationResponse ::
  -- | 'dlcrsResponseStatus'
  Int ->
  DeleteLoggingConfigurationResponse
deleteLoggingConfigurationResponse pResponseStatus_ =
  DeleteLoggingConfigurationResponse'
    { _dlcrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
dlcrsResponseStatus :: Lens' DeleteLoggingConfigurationResponse Int
dlcrsResponseStatus = lens _dlcrsResponseStatus (\s a -> s {_dlcrsResponseStatus = a})

instance NFData DeleteLoggingConfigurationResponse
