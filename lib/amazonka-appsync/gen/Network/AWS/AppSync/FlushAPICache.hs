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
-- Module      : Network.AWS.AppSync.FlushAPICache
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Flushes an @ApiCache@ object.
module Network.AWS.AppSync.FlushAPICache
  ( -- * Creating a Request
    flushAPICache,
    FlushAPICache,

    -- * Request Lenses
    facApiId,

    -- * Destructuring the Response
    flushAPICacheResponse,
    FlushAPICacheResponse,

    -- * Response Lenses
    facrsResponseStatus,
  )
where

import Network.AWS.AppSync.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a @FlushApiCache@ operation.
--
--
--
-- /See:/ 'flushAPICache' smart constructor.
newtype FlushAPICache = FlushAPICache' {_facApiId :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FlushAPICache' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'facApiId' - The API ID.
flushAPICache ::
  -- | 'facApiId'
  Text ->
  FlushAPICache
flushAPICache pApiId_ = FlushAPICache' {_facApiId = pApiId_}

-- | The API ID.
facApiId :: Lens' FlushAPICache Text
facApiId = lens _facApiId (\s a -> s {_facApiId = a})

instance AWSRequest FlushAPICache where
  type Rs FlushAPICache = FlushAPICacheResponse
  request = delete appSync
  response =
    receiveEmpty
      (\s h x -> FlushAPICacheResponse' <$> (pure (fromEnum s)))

instance Hashable FlushAPICache

instance NFData FlushAPICache

instance ToHeaders FlushAPICache where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath FlushAPICache where
  toPath FlushAPICache' {..} =
    mconcat ["/v1/apis/", toBS _facApiId, "/FlushCache"]

instance ToQuery FlushAPICache where
  toQuery = const mempty

-- | Represents the output of a @FlushApiCache@ operation.
--
--
--
-- /See:/ 'flushAPICacheResponse' smart constructor.
newtype FlushAPICacheResponse = FlushAPICacheResponse'
  { _facrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FlushAPICacheResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'facrsResponseStatus' - -- | The response status code.
flushAPICacheResponse ::
  -- | 'facrsResponseStatus'
  Int ->
  FlushAPICacheResponse
flushAPICacheResponse pResponseStatus_ =
  FlushAPICacheResponse' {_facrsResponseStatus = pResponseStatus_}

-- | -- | The response status code.
facrsResponseStatus :: Lens' FlushAPICacheResponse Int
facrsResponseStatus = lens _facrsResponseStatus (\s a -> s {_facrsResponseStatus = a})

instance NFData FlushAPICacheResponse
