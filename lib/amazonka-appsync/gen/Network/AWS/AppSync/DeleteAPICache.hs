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
-- Module      : Network.AWS.AppSync.DeleteAPICache
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an @ApiCache@ object.
module Network.AWS.AppSync.DeleteAPICache
  ( -- * Creating a Request
    deleteAPICache,
    DeleteAPICache,

    -- * Request Lenses
    dacApiId,

    -- * Destructuring the Response
    deleteAPICacheResponse,
    DeleteAPICacheResponse,

    -- * Response Lenses
    dacrsResponseStatus,
  )
where

import Network.AWS.AppSync.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a @DeleteApiCache@ operation.
--
--
--
-- /See:/ 'deleteAPICache' smart constructor.
newtype DeleteAPICache = DeleteAPICache' {_dacApiId :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteAPICache' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dacApiId' - The API ID.
deleteAPICache ::
  -- | 'dacApiId'
  Text ->
  DeleteAPICache
deleteAPICache pApiId_ = DeleteAPICache' {_dacApiId = pApiId_}

-- | The API ID.
dacApiId :: Lens' DeleteAPICache Text
dacApiId = lens _dacApiId (\s a -> s {_dacApiId = a})

instance AWSRequest DeleteAPICache where
  type Rs DeleteAPICache = DeleteAPICacheResponse
  request = delete appSync
  response =
    receiveEmpty
      (\s h x -> DeleteAPICacheResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteAPICache

instance NFData DeleteAPICache

instance ToHeaders DeleteAPICache where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath DeleteAPICache where
  toPath DeleteAPICache' {..} =
    mconcat ["/v1/apis/", toBS _dacApiId, "/ApiCaches"]

instance ToQuery DeleteAPICache where
  toQuery = const mempty

-- | Represents the output of a @DeleteApiCache@ operation.
--
--
--
-- /See:/ 'deleteAPICacheResponse' smart constructor.
newtype DeleteAPICacheResponse = DeleteAPICacheResponse'
  { _dacrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteAPICacheResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dacrsResponseStatus' - -- | The response status code.
deleteAPICacheResponse ::
  -- | 'dacrsResponseStatus'
  Int ->
  DeleteAPICacheResponse
deleteAPICacheResponse pResponseStatus_ =
  DeleteAPICacheResponse' {_dacrsResponseStatus = pResponseStatus_}

-- | -- | The response status code.
dacrsResponseStatus :: Lens' DeleteAPICacheResponse Int
dacrsResponseStatus = lens _dacrsResponseStatus (\s a -> s {_dacrsResponseStatus = a})

instance NFData DeleteAPICacheResponse
