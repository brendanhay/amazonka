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
-- Module      : Network.AWS.MediaStore.StopAccessLogging
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops access logging on the specified container. When you stop access logging on a container, MediaStore stops sending access logs to Amazon CloudWatch Logs. These access logs are not saved and are not retrievable.
module Network.AWS.MediaStore.StopAccessLogging
  ( -- * Creating a Request
    stopAccessLogging,
    StopAccessLogging,

    -- * Request Lenses
    salContainerName,

    -- * Destructuring the Response
    stopAccessLoggingResponse,
    StopAccessLoggingResponse,

    -- * Response Lenses
    salrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MediaStore.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'stopAccessLogging' smart constructor.
newtype StopAccessLogging = StopAccessLogging'
  { _salContainerName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StopAccessLogging' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'salContainerName' - The name of the container that you want to stop access logging on.
stopAccessLogging ::
  -- | 'salContainerName'
  Text ->
  StopAccessLogging
stopAccessLogging pContainerName_ =
  StopAccessLogging' {_salContainerName = pContainerName_}

-- | The name of the container that you want to stop access logging on.
salContainerName :: Lens' StopAccessLogging Text
salContainerName = lens _salContainerName (\s a -> s {_salContainerName = a})

instance AWSRequest StopAccessLogging where
  type Rs StopAccessLogging = StopAccessLoggingResponse
  request = postJSON mediaStore
  response =
    receiveEmpty
      (\s h x -> StopAccessLoggingResponse' <$> (pure (fromEnum s)))

instance Hashable StopAccessLogging

instance NFData StopAccessLogging

instance ToHeaders StopAccessLogging where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("MediaStore_20170901.StopAccessLogging" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON StopAccessLogging where
  toJSON StopAccessLogging' {..} =
    object (catMaybes [Just ("ContainerName" .= _salContainerName)])

instance ToPath StopAccessLogging where
  toPath = const "/"

instance ToQuery StopAccessLogging where
  toQuery = const mempty

-- | /See:/ 'stopAccessLoggingResponse' smart constructor.
newtype StopAccessLoggingResponse = StopAccessLoggingResponse'
  { _salrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StopAccessLoggingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'salrsResponseStatus' - -- | The response status code.
stopAccessLoggingResponse ::
  -- | 'salrsResponseStatus'
  Int ->
  StopAccessLoggingResponse
stopAccessLoggingResponse pResponseStatus_ =
  StopAccessLoggingResponse'
    { _salrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
salrsResponseStatus :: Lens' StopAccessLoggingResponse Int
salrsResponseStatus = lens _salrsResponseStatus (\s a -> s {_salrsResponseStatus = a})

instance NFData StopAccessLoggingResponse
