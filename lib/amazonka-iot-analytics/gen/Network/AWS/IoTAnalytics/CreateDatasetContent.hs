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
-- Module      : Network.AWS.IoTAnalytics.CreateDatasetContent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates the content of a data set by applying a @queryAction@ (a SQL query) or a @containerAction@ (executing a containerized application).
module Network.AWS.IoTAnalytics.CreateDatasetContent
  ( -- * Creating a Request
    createDatasetContent,
    CreateDatasetContent,

    -- * Request Lenses
    cdcVersionId,
    cdcDatasetName,

    -- * Destructuring the Response
    createDatasetContentResponse,
    CreateDatasetContentResponse,

    -- * Response Lenses
    cdcrsVersionId,
    cdcrsResponseStatus,
  )
where

import Network.AWS.IoTAnalytics.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createDatasetContent' smart constructor.
data CreateDatasetContent = CreateDatasetContent'
  { _cdcVersionId ::
      !(Maybe Text),
    _cdcDatasetName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateDatasetContent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdcVersionId' - The version ID of the dataset content. To specify @versionId@ for a dataset content, the dataset must use a <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer> filter.
--
-- * 'cdcDatasetName' - The name of the dataset.
createDatasetContent ::
  -- | 'cdcDatasetName'
  Text ->
  CreateDatasetContent
createDatasetContent pDatasetName_ =
  CreateDatasetContent'
    { _cdcVersionId = Nothing,
      _cdcDatasetName = pDatasetName_
    }

-- | The version ID of the dataset content. To specify @versionId@ for a dataset content, the dataset must use a <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer> filter.
cdcVersionId :: Lens' CreateDatasetContent (Maybe Text)
cdcVersionId = lens _cdcVersionId (\s a -> s {_cdcVersionId = a})

-- | The name of the dataset.
cdcDatasetName :: Lens' CreateDatasetContent Text
cdcDatasetName = lens _cdcDatasetName (\s a -> s {_cdcDatasetName = a})

instance AWSRequest CreateDatasetContent where
  type Rs CreateDatasetContent = CreateDatasetContentResponse
  request = postJSON ioTAnalytics
  response =
    receiveJSON
      ( \s h x ->
          CreateDatasetContentResponse'
            <$> (x .?> "versionId") <*> (pure (fromEnum s))
      )

instance Hashable CreateDatasetContent

instance NFData CreateDatasetContent

instance ToHeaders CreateDatasetContent where
  toHeaders = const mempty

instance ToJSON CreateDatasetContent where
  toJSON CreateDatasetContent' {..} =
    object (catMaybes [("versionId" .=) <$> _cdcVersionId])

instance ToPath CreateDatasetContent where
  toPath CreateDatasetContent' {..} =
    mconcat ["/datasets/", toBS _cdcDatasetName, "/content"]

instance ToQuery CreateDatasetContent where
  toQuery = const mempty

-- | /See:/ 'createDatasetContentResponse' smart constructor.
data CreateDatasetContentResponse = CreateDatasetContentResponse'
  { _cdcrsVersionId ::
      !(Maybe Text),
    _cdcrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateDatasetContentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdcrsVersionId' - The version ID of the dataset contents that are being created.
--
-- * 'cdcrsResponseStatus' - -- | The response status code.
createDatasetContentResponse ::
  -- | 'cdcrsResponseStatus'
  Int ->
  CreateDatasetContentResponse
createDatasetContentResponse pResponseStatus_ =
  CreateDatasetContentResponse'
    { _cdcrsVersionId = Nothing,
      _cdcrsResponseStatus = pResponseStatus_
    }

-- | The version ID of the dataset contents that are being created.
cdcrsVersionId :: Lens' CreateDatasetContentResponse (Maybe Text)
cdcrsVersionId = lens _cdcrsVersionId (\s a -> s {_cdcrsVersionId = a})

-- | -- | The response status code.
cdcrsResponseStatus :: Lens' CreateDatasetContentResponse Int
cdcrsResponseStatus = lens _cdcrsResponseStatus (\s a -> s {_cdcrsResponseStatus = a})

instance NFData CreateDatasetContentResponse
