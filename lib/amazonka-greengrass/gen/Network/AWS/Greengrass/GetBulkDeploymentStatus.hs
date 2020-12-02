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
-- Module      : Network.AWS.Greengrass.GetBulkDeploymentStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the status of a bulk deployment.
module Network.AWS.Greengrass.GetBulkDeploymentStatus
  ( -- * Creating a Request
    getBulkDeploymentStatus,
    GetBulkDeploymentStatus,

    -- * Request Lenses
    gbdsBulkDeploymentId,

    -- * Destructuring the Response
    getBulkDeploymentStatusResponse,
    GetBulkDeploymentStatusResponse,

    -- * Response Lenses
    gbdsrsCreatedAt,
    gbdsrsErrorDetails,
    gbdsrsBulkDeploymentStatus,
    gbdsrsErrorMessage,
    gbdsrsBulkDeploymentMetrics,
    gbdsrsTags,
    gbdsrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getBulkDeploymentStatus' smart constructor.
newtype GetBulkDeploymentStatus = GetBulkDeploymentStatus'
  { _gbdsBulkDeploymentId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetBulkDeploymentStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbdsBulkDeploymentId' - The ID of the bulk deployment.
getBulkDeploymentStatus ::
  -- | 'gbdsBulkDeploymentId'
  Text ->
  GetBulkDeploymentStatus
getBulkDeploymentStatus pBulkDeploymentId_ =
  GetBulkDeploymentStatus'
    { _gbdsBulkDeploymentId =
        pBulkDeploymentId_
    }

-- | The ID of the bulk deployment.
gbdsBulkDeploymentId :: Lens' GetBulkDeploymentStatus Text
gbdsBulkDeploymentId = lens _gbdsBulkDeploymentId (\s a -> s {_gbdsBulkDeploymentId = a})

instance AWSRequest GetBulkDeploymentStatus where
  type Rs GetBulkDeploymentStatus = GetBulkDeploymentStatusResponse
  request = get greengrass
  response =
    receiveJSON
      ( \s h x ->
          GetBulkDeploymentStatusResponse'
            <$> (x .?> "CreatedAt")
            <*> (x .?> "ErrorDetails" .!@ mempty)
            <*> (x .?> "BulkDeploymentStatus")
            <*> (x .?> "ErrorMessage")
            <*> (x .?> "BulkDeploymentMetrics")
            <*> (x .?> "tags" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable GetBulkDeploymentStatus

instance NFData GetBulkDeploymentStatus

instance ToHeaders GetBulkDeploymentStatus where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath GetBulkDeploymentStatus where
  toPath GetBulkDeploymentStatus' {..} =
    mconcat
      [ "/greengrass/bulk/deployments/",
        toBS _gbdsBulkDeploymentId,
        "/status"
      ]

instance ToQuery GetBulkDeploymentStatus where
  toQuery = const mempty

-- | /See:/ 'getBulkDeploymentStatusResponse' smart constructor.
data GetBulkDeploymentStatusResponse = GetBulkDeploymentStatusResponse'
  { _gbdsrsCreatedAt ::
      !(Maybe Text),
    _gbdsrsErrorDetails ::
      !(Maybe [ErrorDetail]),
    _gbdsrsBulkDeploymentStatus ::
      !( Maybe
           BulkDeploymentStatus
       ),
    _gbdsrsErrorMessage ::
      !(Maybe Text),
    _gbdsrsBulkDeploymentMetrics ::
      !( Maybe
           BulkDeploymentMetrics
       ),
    _gbdsrsTags ::
      !(Maybe (Map Text (Text))),
    _gbdsrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetBulkDeploymentStatusResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbdsrsCreatedAt' - The time, in ISO format, when the deployment was created.
--
-- * 'gbdsrsErrorDetails' - Error details
--
-- * 'gbdsrsBulkDeploymentStatus' - The status of the bulk deployment.
--
-- * 'gbdsrsErrorMessage' - Error message
--
-- * 'gbdsrsBulkDeploymentMetrics' - Relevant metrics on input records processed during bulk deployment.
--
-- * 'gbdsrsTags' - Tag(s) attached to the resource arn.
--
-- * 'gbdsrsResponseStatus' - -- | The response status code.
getBulkDeploymentStatusResponse ::
  -- | 'gbdsrsResponseStatus'
  Int ->
  GetBulkDeploymentStatusResponse
getBulkDeploymentStatusResponse pResponseStatus_ =
  GetBulkDeploymentStatusResponse'
    { _gbdsrsCreatedAt = Nothing,
      _gbdsrsErrorDetails = Nothing,
      _gbdsrsBulkDeploymentStatus = Nothing,
      _gbdsrsErrorMessage = Nothing,
      _gbdsrsBulkDeploymentMetrics = Nothing,
      _gbdsrsTags = Nothing,
      _gbdsrsResponseStatus = pResponseStatus_
    }

-- | The time, in ISO format, when the deployment was created.
gbdsrsCreatedAt :: Lens' GetBulkDeploymentStatusResponse (Maybe Text)
gbdsrsCreatedAt = lens _gbdsrsCreatedAt (\s a -> s {_gbdsrsCreatedAt = a})

-- | Error details
gbdsrsErrorDetails :: Lens' GetBulkDeploymentStatusResponse [ErrorDetail]
gbdsrsErrorDetails = lens _gbdsrsErrorDetails (\s a -> s {_gbdsrsErrorDetails = a}) . _Default . _Coerce

-- | The status of the bulk deployment.
gbdsrsBulkDeploymentStatus :: Lens' GetBulkDeploymentStatusResponse (Maybe BulkDeploymentStatus)
gbdsrsBulkDeploymentStatus = lens _gbdsrsBulkDeploymentStatus (\s a -> s {_gbdsrsBulkDeploymentStatus = a})

-- | Error message
gbdsrsErrorMessage :: Lens' GetBulkDeploymentStatusResponse (Maybe Text)
gbdsrsErrorMessage = lens _gbdsrsErrorMessage (\s a -> s {_gbdsrsErrorMessage = a})

-- | Relevant metrics on input records processed during bulk deployment.
gbdsrsBulkDeploymentMetrics :: Lens' GetBulkDeploymentStatusResponse (Maybe BulkDeploymentMetrics)
gbdsrsBulkDeploymentMetrics = lens _gbdsrsBulkDeploymentMetrics (\s a -> s {_gbdsrsBulkDeploymentMetrics = a})

-- | Tag(s) attached to the resource arn.
gbdsrsTags :: Lens' GetBulkDeploymentStatusResponse (HashMap Text (Text))
gbdsrsTags = lens _gbdsrsTags (\s a -> s {_gbdsrsTags = a}) . _Default . _Map

-- | -- | The response status code.
gbdsrsResponseStatus :: Lens' GetBulkDeploymentStatusResponse Int
gbdsrsResponseStatus = lens _gbdsrsResponseStatus (\s a -> s {_gbdsrsResponseStatus = a})

instance NFData GetBulkDeploymentStatusResponse
