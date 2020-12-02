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
-- Module      : Network.AWS.MediaLive.BatchDelete
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts delete of resources.
module Network.AWS.MediaLive.BatchDelete
  ( -- * Creating a Request
    batchDelete,
    BatchDelete,

    -- * Request Lenses
    bdChannelIds,
    bdInputIds,
    bdMultiplexIds,
    bdInputSecurityGroupIds,

    -- * Destructuring the Response
    batchDeleteResponse,
    BatchDeleteResponse,

    -- * Response Lenses
    bdrsSuccessful,
    bdrsFailed,
    bdrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | A request to delete resources
--
-- /See:/ 'batchDelete' smart constructor.
data BatchDelete = BatchDelete'
  { _bdChannelIds :: !(Maybe [Text]),
    _bdInputIds :: !(Maybe [Text]),
    _bdMultiplexIds :: !(Maybe [Text]),
    _bdInputSecurityGroupIds :: !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchDelete' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdChannelIds' - List of channel IDs
--
-- * 'bdInputIds' - List of input IDs
--
-- * 'bdMultiplexIds' - List of multiplex IDs
--
-- * 'bdInputSecurityGroupIds' - List of input security group IDs
batchDelete ::
  BatchDelete
batchDelete =
  BatchDelete'
    { _bdChannelIds = Nothing,
      _bdInputIds = Nothing,
      _bdMultiplexIds = Nothing,
      _bdInputSecurityGroupIds = Nothing
    }

-- | List of channel IDs
bdChannelIds :: Lens' BatchDelete [Text]
bdChannelIds = lens _bdChannelIds (\s a -> s {_bdChannelIds = a}) . _Default . _Coerce

-- | List of input IDs
bdInputIds :: Lens' BatchDelete [Text]
bdInputIds = lens _bdInputIds (\s a -> s {_bdInputIds = a}) . _Default . _Coerce

-- | List of multiplex IDs
bdMultiplexIds :: Lens' BatchDelete [Text]
bdMultiplexIds = lens _bdMultiplexIds (\s a -> s {_bdMultiplexIds = a}) . _Default . _Coerce

-- | List of input security group IDs
bdInputSecurityGroupIds :: Lens' BatchDelete [Text]
bdInputSecurityGroupIds = lens _bdInputSecurityGroupIds (\s a -> s {_bdInputSecurityGroupIds = a}) . _Default . _Coerce

instance AWSRequest BatchDelete where
  type Rs BatchDelete = BatchDeleteResponse
  request = postJSON mediaLive
  response =
    receiveJSON
      ( \s h x ->
          BatchDeleteResponse'
            <$> (x .?> "successful" .!@ mempty)
            <*> (x .?> "failed" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable BatchDelete

instance NFData BatchDelete

instance ToHeaders BatchDelete where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON BatchDelete where
  toJSON BatchDelete' {..} =
    object
      ( catMaybes
          [ ("channelIds" .=) <$> _bdChannelIds,
            ("inputIds" .=) <$> _bdInputIds,
            ("multiplexIds" .=) <$> _bdMultiplexIds,
            ("inputSecurityGroupIds" .=) <$> _bdInputSecurityGroupIds
          ]
      )

instance ToPath BatchDelete where
  toPath = const "/prod/batch/delete"

instance ToQuery BatchDelete where
  toQuery = const mempty

-- | Placeholder documentation for BatchDeleteResponse
--
-- /See:/ 'batchDeleteResponse' smart constructor.
data BatchDeleteResponse = BatchDeleteResponse'
  { _bdrsSuccessful ::
      !(Maybe [BatchSuccessfulResultModel]),
    _bdrsFailed :: !(Maybe [BatchFailedResultModel]),
    _bdrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchDeleteResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdrsSuccessful' - List of successful operations
--
-- * 'bdrsFailed' - List of failed operations
--
-- * 'bdrsResponseStatus' - -- | The response status code.
batchDeleteResponse ::
  -- | 'bdrsResponseStatus'
  Int ->
  BatchDeleteResponse
batchDeleteResponse pResponseStatus_ =
  BatchDeleteResponse'
    { _bdrsSuccessful = Nothing,
      _bdrsFailed = Nothing,
      _bdrsResponseStatus = pResponseStatus_
    }

-- | List of successful operations
bdrsSuccessful :: Lens' BatchDeleteResponse [BatchSuccessfulResultModel]
bdrsSuccessful = lens _bdrsSuccessful (\s a -> s {_bdrsSuccessful = a}) . _Default . _Coerce

-- | List of failed operations
bdrsFailed :: Lens' BatchDeleteResponse [BatchFailedResultModel]
bdrsFailed = lens _bdrsFailed (\s a -> s {_bdrsFailed = a}) . _Default . _Coerce

-- | -- | The response status code.
bdrsResponseStatus :: Lens' BatchDeleteResponse Int
bdrsResponseStatus = lens _bdrsResponseStatus (\s a -> s {_bdrsResponseStatus = a})

instance NFData BatchDeleteResponse
