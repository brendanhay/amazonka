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
-- Module      : Network.AWS.MediaStore.StartAccessLogging
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts access logging on the specified container. When you enable access logging on a container, MediaStore delivers access logs for objects stored in that container to Amazon CloudWatch Logs.
--
--
module Network.AWS.MediaStore.StartAccessLogging
    (
    -- * Creating a Request
      startAccessLogging
    , StartAccessLogging
    -- * Request Lenses
    , sContainerName

    -- * Destructuring the Response
    , startAccessLoggingResponse
    , StartAccessLoggingResponse
    -- * Response Lenses
    , srsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaStore.Types
import Network.AWS.MediaStore.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startAccessLogging' smart constructor.
newtype StartAccessLogging = StartAccessLogging'
  { _sContainerName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartAccessLogging' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sContainerName' - The name of the container that you want to start access logging on.
startAccessLogging
    :: Text -- ^ 'sContainerName'
    -> StartAccessLogging
startAccessLogging pContainerName_ =
  StartAccessLogging' {_sContainerName = pContainerName_}


-- | The name of the container that you want to start access logging on.
sContainerName :: Lens' StartAccessLogging Text
sContainerName = lens _sContainerName (\ s a -> s{_sContainerName = a})

instance AWSRequest StartAccessLogging where
        type Rs StartAccessLogging =
             StartAccessLoggingResponse
        request = postJSON mediaStore
        response
          = receiveEmpty
              (\ s h x ->
                 StartAccessLoggingResponse' <$> (pure (fromEnum s)))

instance Hashable StartAccessLogging where

instance NFData StartAccessLogging where

instance ToHeaders StartAccessLogging where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("MediaStore_20170901.StartAccessLogging" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartAccessLogging where
        toJSON StartAccessLogging'{..}
          = object
              (catMaybes
                 [Just ("ContainerName" .= _sContainerName)])

instance ToPath StartAccessLogging where
        toPath = const "/"

instance ToQuery StartAccessLogging where
        toQuery = const mempty

-- | /See:/ 'startAccessLoggingResponse' smart constructor.
newtype StartAccessLoggingResponse = StartAccessLoggingResponse'
  { _srsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartAccessLoggingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srsResponseStatus' - -- | The response status code.
startAccessLoggingResponse
    :: Int -- ^ 'srsResponseStatus'
    -> StartAccessLoggingResponse
startAccessLoggingResponse pResponseStatus_ =
  StartAccessLoggingResponse' {_srsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
srsResponseStatus :: Lens' StartAccessLoggingResponse Int
srsResponseStatus = lens _srsResponseStatus (\ s a -> s{_srsResponseStatus = a})

instance NFData StartAccessLoggingResponse where
