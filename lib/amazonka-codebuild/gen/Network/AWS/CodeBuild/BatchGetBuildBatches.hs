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
-- Module      : Network.AWS.CodeBuild.BatchGetBuildBatches
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about one or more batch builds.
module Network.AWS.CodeBuild.BatchGetBuildBatches
  ( -- * Creating a Request
    batchGetBuildBatches,
    BatchGetBuildBatches,

    -- * Request Lenses
    bgbbIds,

    -- * Destructuring the Response
    batchGetBuildBatchesResponse,
    BatchGetBuildBatchesResponse,

    -- * Response Lenses
    bgbbrsBuildBatches,
    bgbbrsBuildBatchesNotFound,
    bgbbrsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'batchGetBuildBatches' smart constructor.
newtype BatchGetBuildBatches = BatchGetBuildBatches'
  { _bgbbIds ::
      [Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchGetBuildBatches' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgbbIds' - An array that contains the batch build identifiers to retrieve.
batchGetBuildBatches ::
  BatchGetBuildBatches
batchGetBuildBatches = BatchGetBuildBatches' {_bgbbIds = mempty}

-- | An array that contains the batch build identifiers to retrieve.
bgbbIds :: Lens' BatchGetBuildBatches [Text]
bgbbIds = lens _bgbbIds (\s a -> s {_bgbbIds = a}) . _Coerce

instance AWSRequest BatchGetBuildBatches where
  type Rs BatchGetBuildBatches = BatchGetBuildBatchesResponse
  request = postJSON codeBuild
  response =
    receiveJSON
      ( \s h x ->
          BatchGetBuildBatchesResponse'
            <$> (x .?> "buildBatches" .!@ mempty)
            <*> (x .?> "buildBatchesNotFound" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable BatchGetBuildBatches

instance NFData BatchGetBuildBatches

instance ToHeaders BatchGetBuildBatches where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("CodeBuild_20161006.BatchGetBuildBatches" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON BatchGetBuildBatches where
  toJSON BatchGetBuildBatches' {..} =
    object (catMaybes [Just ("ids" .= _bgbbIds)])

instance ToPath BatchGetBuildBatches where
  toPath = const "/"

instance ToQuery BatchGetBuildBatches where
  toQuery = const mempty

-- | /See:/ 'batchGetBuildBatchesResponse' smart constructor.
data BatchGetBuildBatchesResponse = BatchGetBuildBatchesResponse'
  { _bgbbrsBuildBatches ::
      !(Maybe [BuildBatch]),
    _bgbbrsBuildBatchesNotFound ::
      !(Maybe [Text]),
    _bgbbrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchGetBuildBatchesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgbbrsBuildBatches' - An array of @BuildBatch@ objects that represent the retrieved batch builds.
--
-- * 'bgbbrsBuildBatchesNotFound' - An array that contains the identifiers of any batch builds that are not found.
--
-- * 'bgbbrsResponseStatus' - -- | The response status code.
batchGetBuildBatchesResponse ::
  -- | 'bgbbrsResponseStatus'
  Int ->
  BatchGetBuildBatchesResponse
batchGetBuildBatchesResponse pResponseStatus_ =
  BatchGetBuildBatchesResponse'
    { _bgbbrsBuildBatches = Nothing,
      _bgbbrsBuildBatchesNotFound = Nothing,
      _bgbbrsResponseStatus = pResponseStatus_
    }

-- | An array of @BuildBatch@ objects that represent the retrieved batch builds.
bgbbrsBuildBatches :: Lens' BatchGetBuildBatchesResponse [BuildBatch]
bgbbrsBuildBatches = lens _bgbbrsBuildBatches (\s a -> s {_bgbbrsBuildBatches = a}) . _Default . _Coerce

-- | An array that contains the identifiers of any batch builds that are not found.
bgbbrsBuildBatchesNotFound :: Lens' BatchGetBuildBatchesResponse [Text]
bgbbrsBuildBatchesNotFound = lens _bgbbrsBuildBatchesNotFound (\s a -> s {_bgbbrsBuildBatchesNotFound = a}) . _Default . _Coerce

-- | -- | The response status code.
bgbbrsResponseStatus :: Lens' BatchGetBuildBatchesResponse Int
bgbbrsResponseStatus = lens _bgbbrsResponseStatus (\s a -> s {_bgbbrsResponseStatus = a})

instance NFData BatchGetBuildBatchesResponse
