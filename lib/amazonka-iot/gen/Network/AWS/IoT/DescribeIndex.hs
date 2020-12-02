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
-- Module      : Network.AWS.IoT.DescribeIndex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a search index.
module Network.AWS.IoT.DescribeIndex
  ( -- * Creating a Request
    describeIndex,
    DescribeIndex,

    -- * Request Lenses
    diIndexName,

    -- * Destructuring the Response
    describeIndexResponse,
    DescribeIndexResponse,

    -- * Response Lenses
    dirsIndexStatus,
    dirsSchema,
    dirsIndexName,
    dirsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeIndex' smart constructor.
newtype DescribeIndex = DescribeIndex' {_diIndexName :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeIndex' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diIndexName' - The index name.
describeIndex ::
  -- | 'diIndexName'
  Text ->
  DescribeIndex
describeIndex pIndexName_ =
  DescribeIndex' {_diIndexName = pIndexName_}

-- | The index name.
diIndexName :: Lens' DescribeIndex Text
diIndexName = lens _diIndexName (\s a -> s {_diIndexName = a})

instance AWSRequest DescribeIndex where
  type Rs DescribeIndex = DescribeIndexResponse
  request = get ioT
  response =
    receiveJSON
      ( \s h x ->
          DescribeIndexResponse'
            <$> (x .?> "indexStatus")
            <*> (x .?> "schema")
            <*> (x .?> "indexName")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeIndex

instance NFData DescribeIndex

instance ToHeaders DescribeIndex where
  toHeaders = const mempty

instance ToPath DescribeIndex where
  toPath DescribeIndex' {..} =
    mconcat ["/indices/", toBS _diIndexName]

instance ToQuery DescribeIndex where
  toQuery = const mempty

-- | /See:/ 'describeIndexResponse' smart constructor.
data DescribeIndexResponse = DescribeIndexResponse'
  { _dirsIndexStatus ::
      !(Maybe IndexStatus),
    _dirsSchema :: !(Maybe Text),
    _dirsIndexName :: !(Maybe Text),
    _dirsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeIndexResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dirsIndexStatus' - The index status.
--
-- * 'dirsSchema' - Contains a value that specifies the type of indexing performed. Valid values are:     * REGISTRY – Your thing index contains only registry data.     * REGISTRY_AND_SHADOW - Your thing index contains registry data and shadow data.     * REGISTRY_AND_CONNECTIVITY_STATUS - Your thing index contains registry data and thing connectivity status data.     * REGISTRY_AND_SHADOW_AND_CONNECTIVITY_STATUS - Your thing index contains registry data, shadow data, and thing connectivity status data.
--
-- * 'dirsIndexName' - The index name.
--
-- * 'dirsResponseStatus' - -- | The response status code.
describeIndexResponse ::
  -- | 'dirsResponseStatus'
  Int ->
  DescribeIndexResponse
describeIndexResponse pResponseStatus_ =
  DescribeIndexResponse'
    { _dirsIndexStatus = Nothing,
      _dirsSchema = Nothing,
      _dirsIndexName = Nothing,
      _dirsResponseStatus = pResponseStatus_
    }

-- | The index status.
dirsIndexStatus :: Lens' DescribeIndexResponse (Maybe IndexStatus)
dirsIndexStatus = lens _dirsIndexStatus (\s a -> s {_dirsIndexStatus = a})

-- | Contains a value that specifies the type of indexing performed. Valid values are:     * REGISTRY – Your thing index contains only registry data.     * REGISTRY_AND_SHADOW - Your thing index contains registry data and shadow data.     * REGISTRY_AND_CONNECTIVITY_STATUS - Your thing index contains registry data and thing connectivity status data.     * REGISTRY_AND_SHADOW_AND_CONNECTIVITY_STATUS - Your thing index contains registry data, shadow data, and thing connectivity status data.
dirsSchema :: Lens' DescribeIndexResponse (Maybe Text)
dirsSchema = lens _dirsSchema (\s a -> s {_dirsSchema = a})

-- | The index name.
dirsIndexName :: Lens' DescribeIndexResponse (Maybe Text)
dirsIndexName = lens _dirsIndexName (\s a -> s {_dirsIndexName = a})

-- | -- | The response status code.
dirsResponseStatus :: Lens' DescribeIndexResponse Int
dirsResponseStatus = lens _dirsResponseStatus (\s a -> s {_dirsResponseStatus = a})

instance NFData DescribeIndexResponse
