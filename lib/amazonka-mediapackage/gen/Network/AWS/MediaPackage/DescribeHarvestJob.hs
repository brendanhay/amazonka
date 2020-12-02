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
-- Module      : Network.AWS.MediaPackage.DescribeHarvestJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details about an existing HarvestJob.
module Network.AWS.MediaPackage.DescribeHarvestJob
  ( -- * Creating a Request
    describeHarvestJob,
    DescribeHarvestJob,

    -- * Request Lenses
    dhjId,

    -- * Destructuring the Response
    describeHarvestJobResponse,
    DescribeHarvestJobResponse,

    -- * Response Lenses
    dhjrsStatus,
    dhjrsOriginEndpointId,
    dhjrsStartTime,
    dhjrsARN,
    dhjrsCreatedAt,
    dhjrsChannelId,
    dhjrsS3Destination,
    dhjrsEndTime,
    dhjrsId,
    dhjrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MediaPackage.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeHarvestJob' smart constructor.
newtype DescribeHarvestJob = DescribeHarvestJob' {_dhjId :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeHarvestJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dhjId' - The ID of the HarvestJob.
describeHarvestJob ::
  -- | 'dhjId'
  Text ->
  DescribeHarvestJob
describeHarvestJob pId_ = DescribeHarvestJob' {_dhjId = pId_}

-- | The ID of the HarvestJob.
dhjId :: Lens' DescribeHarvestJob Text
dhjId = lens _dhjId (\s a -> s {_dhjId = a})

instance AWSRequest DescribeHarvestJob where
  type Rs DescribeHarvestJob = DescribeHarvestJobResponse
  request = get mediaPackage
  response =
    receiveJSON
      ( \s h x ->
          DescribeHarvestJobResponse'
            <$> (x .?> "status")
            <*> (x .?> "originEndpointId")
            <*> (x .?> "startTime")
            <*> (x .?> "arn")
            <*> (x .?> "createdAt")
            <*> (x .?> "channelId")
            <*> (x .?> "s3Destination")
            <*> (x .?> "endTime")
            <*> (x .?> "id")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeHarvestJob

instance NFData DescribeHarvestJob

instance ToHeaders DescribeHarvestJob where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath DescribeHarvestJob where
  toPath DescribeHarvestJob' {..} =
    mconcat ["/harvest_jobs/", toBS _dhjId]

instance ToQuery DescribeHarvestJob where
  toQuery = const mempty

-- | /See:/ 'describeHarvestJobResponse' smart constructor.
data DescribeHarvestJobResponse = DescribeHarvestJobResponse'
  { _dhjrsStatus ::
      !(Maybe Status),
    _dhjrsOriginEndpointId ::
      !(Maybe Text),
    _dhjrsStartTime :: !(Maybe Text),
    _dhjrsARN :: !(Maybe Text),
    _dhjrsCreatedAt :: !(Maybe Text),
    _dhjrsChannelId :: !(Maybe Text),
    _dhjrsS3Destination ::
      !(Maybe S3Destination),
    _dhjrsEndTime :: !(Maybe Text),
    _dhjrsId :: !(Maybe Text),
    _dhjrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeHarvestJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dhjrsStatus' - The current status of the HarvestJob. Consider setting up a CloudWatch Event to listen for HarvestJobs as they succeed or fail. In the event of failure, the CloudWatch Event will include an explanation of why the HarvestJob failed.
--
-- * 'dhjrsOriginEndpointId' - The ID of the OriginEndpoint that the HarvestJob will harvest from. This cannot be changed after the HarvestJob is submitted.
--
-- * 'dhjrsStartTime' - The start of the time-window which will be harvested.
--
-- * 'dhjrsARN' - The Amazon Resource Name (ARN) assigned to the HarvestJob.
--
-- * 'dhjrsCreatedAt' - The time the HarvestJob was submitted
--
-- * 'dhjrsChannelId' - The ID of the Channel that the HarvestJob will harvest from.
--
-- * 'dhjrsS3Destination' - Undocumented member.
--
-- * 'dhjrsEndTime' - The end of the time-window which will be harvested.
--
-- * 'dhjrsId' - The ID of the HarvestJob. The ID must be unique within the region and it cannot be changed after the HarvestJob is submitted.
--
-- * 'dhjrsResponseStatus' - -- | The response status code.
describeHarvestJobResponse ::
  -- | 'dhjrsResponseStatus'
  Int ->
  DescribeHarvestJobResponse
describeHarvestJobResponse pResponseStatus_ =
  DescribeHarvestJobResponse'
    { _dhjrsStatus = Nothing,
      _dhjrsOriginEndpointId = Nothing,
      _dhjrsStartTime = Nothing,
      _dhjrsARN = Nothing,
      _dhjrsCreatedAt = Nothing,
      _dhjrsChannelId = Nothing,
      _dhjrsS3Destination = Nothing,
      _dhjrsEndTime = Nothing,
      _dhjrsId = Nothing,
      _dhjrsResponseStatus = pResponseStatus_
    }

-- | The current status of the HarvestJob. Consider setting up a CloudWatch Event to listen for HarvestJobs as they succeed or fail. In the event of failure, the CloudWatch Event will include an explanation of why the HarvestJob failed.
dhjrsStatus :: Lens' DescribeHarvestJobResponse (Maybe Status)
dhjrsStatus = lens _dhjrsStatus (\s a -> s {_dhjrsStatus = a})

-- | The ID of the OriginEndpoint that the HarvestJob will harvest from. This cannot be changed after the HarvestJob is submitted.
dhjrsOriginEndpointId :: Lens' DescribeHarvestJobResponse (Maybe Text)
dhjrsOriginEndpointId = lens _dhjrsOriginEndpointId (\s a -> s {_dhjrsOriginEndpointId = a})

-- | The start of the time-window which will be harvested.
dhjrsStartTime :: Lens' DescribeHarvestJobResponse (Maybe Text)
dhjrsStartTime = lens _dhjrsStartTime (\s a -> s {_dhjrsStartTime = a})

-- | The Amazon Resource Name (ARN) assigned to the HarvestJob.
dhjrsARN :: Lens' DescribeHarvestJobResponse (Maybe Text)
dhjrsARN = lens _dhjrsARN (\s a -> s {_dhjrsARN = a})

-- | The time the HarvestJob was submitted
dhjrsCreatedAt :: Lens' DescribeHarvestJobResponse (Maybe Text)
dhjrsCreatedAt = lens _dhjrsCreatedAt (\s a -> s {_dhjrsCreatedAt = a})

-- | The ID of the Channel that the HarvestJob will harvest from.
dhjrsChannelId :: Lens' DescribeHarvestJobResponse (Maybe Text)
dhjrsChannelId = lens _dhjrsChannelId (\s a -> s {_dhjrsChannelId = a})

-- | Undocumented member.
dhjrsS3Destination :: Lens' DescribeHarvestJobResponse (Maybe S3Destination)
dhjrsS3Destination = lens _dhjrsS3Destination (\s a -> s {_dhjrsS3Destination = a})

-- | The end of the time-window which will be harvested.
dhjrsEndTime :: Lens' DescribeHarvestJobResponse (Maybe Text)
dhjrsEndTime = lens _dhjrsEndTime (\s a -> s {_dhjrsEndTime = a})

-- | The ID of the HarvestJob. The ID must be unique within the region and it cannot be changed after the HarvestJob is submitted.
dhjrsId :: Lens' DescribeHarvestJobResponse (Maybe Text)
dhjrsId = lens _dhjrsId (\s a -> s {_dhjrsId = a})

-- | -- | The response status code.
dhjrsResponseStatus :: Lens' DescribeHarvestJobResponse Int
dhjrsResponseStatus = lens _dhjrsResponseStatus (\s a -> s {_dhjrsResponseStatus = a})

instance NFData DescribeHarvestJobResponse
