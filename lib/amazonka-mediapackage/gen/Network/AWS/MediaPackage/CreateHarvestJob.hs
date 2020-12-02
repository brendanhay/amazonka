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
-- Module      : Network.AWS.MediaPackage.CreateHarvestJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new HarvestJob record.
module Network.AWS.MediaPackage.CreateHarvestJob
  ( -- * Creating a Request
    createHarvestJob,
    CreateHarvestJob,

    -- * Request Lenses
    chjS3Destination,
    chjEndTime,
    chjOriginEndpointId,
    chjStartTime,
    chjId,

    -- * Destructuring the Response
    createHarvestJobResponse,
    CreateHarvestJobResponse,

    -- * Response Lenses
    chjrsStatus,
    chjrsOriginEndpointId,
    chjrsStartTime,
    chjrsARN,
    chjrsCreatedAt,
    chjrsChannelId,
    chjrsS3Destination,
    chjrsEndTime,
    chjrsId,
    chjrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MediaPackage.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Configuration parameters used to create a new HarvestJob.
--
-- /See:/ 'createHarvestJob' smart constructor.
data CreateHarvestJob = CreateHarvestJob'
  { _chjS3Destination ::
      !S3Destination,
    _chjEndTime :: !Text,
    _chjOriginEndpointId :: !Text,
    _chjStartTime :: !Text,
    _chjId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateHarvestJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chjS3Destination' - Undocumented member.
--
-- * 'chjEndTime' - The end of the time-window which will be harvested
--
-- * 'chjOriginEndpointId' - The ID of the OriginEndpoint that the HarvestJob will harvest from. This cannot be changed after the HarvestJob is submitted.
--
-- * 'chjStartTime' - The start of the time-window which will be harvested
--
-- * 'chjId' - The ID of the HarvestJob. The ID must be unique within the region and it cannot be changed after the HarvestJob is submitted
createHarvestJob ::
  -- | 'chjS3Destination'
  S3Destination ->
  -- | 'chjEndTime'
  Text ->
  -- | 'chjOriginEndpointId'
  Text ->
  -- | 'chjStartTime'
  Text ->
  -- | 'chjId'
  Text ->
  CreateHarvestJob
createHarvestJob
  pS3Destination_
  pEndTime_
  pOriginEndpointId_
  pStartTime_
  pId_ =
    CreateHarvestJob'
      { _chjS3Destination = pS3Destination_,
        _chjEndTime = pEndTime_,
        _chjOriginEndpointId = pOriginEndpointId_,
        _chjStartTime = pStartTime_,
        _chjId = pId_
      }

-- | Undocumented member.
chjS3Destination :: Lens' CreateHarvestJob S3Destination
chjS3Destination = lens _chjS3Destination (\s a -> s {_chjS3Destination = a})

-- | The end of the time-window which will be harvested
chjEndTime :: Lens' CreateHarvestJob Text
chjEndTime = lens _chjEndTime (\s a -> s {_chjEndTime = a})

-- | The ID of the OriginEndpoint that the HarvestJob will harvest from. This cannot be changed after the HarvestJob is submitted.
chjOriginEndpointId :: Lens' CreateHarvestJob Text
chjOriginEndpointId = lens _chjOriginEndpointId (\s a -> s {_chjOriginEndpointId = a})

-- | The start of the time-window which will be harvested
chjStartTime :: Lens' CreateHarvestJob Text
chjStartTime = lens _chjStartTime (\s a -> s {_chjStartTime = a})

-- | The ID of the HarvestJob. The ID must be unique within the region and it cannot be changed after the HarvestJob is submitted
chjId :: Lens' CreateHarvestJob Text
chjId = lens _chjId (\s a -> s {_chjId = a})

instance AWSRequest CreateHarvestJob where
  type Rs CreateHarvestJob = CreateHarvestJobResponse
  request = postJSON mediaPackage
  response =
    receiveJSON
      ( \s h x ->
          CreateHarvestJobResponse'
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

instance Hashable CreateHarvestJob

instance NFData CreateHarvestJob

instance ToHeaders CreateHarvestJob where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON CreateHarvestJob where
  toJSON CreateHarvestJob' {..} =
    object
      ( catMaybes
          [ Just ("s3Destination" .= _chjS3Destination),
            Just ("endTime" .= _chjEndTime),
            Just ("originEndpointId" .= _chjOriginEndpointId),
            Just ("startTime" .= _chjStartTime),
            Just ("id" .= _chjId)
          ]
      )

instance ToPath CreateHarvestJob where
  toPath = const "/harvest_jobs"

instance ToQuery CreateHarvestJob where
  toQuery = const mempty

-- | /See:/ 'createHarvestJobResponse' smart constructor.
data CreateHarvestJobResponse = CreateHarvestJobResponse'
  { _chjrsStatus ::
      !(Maybe Status),
    _chjrsOriginEndpointId :: !(Maybe Text),
    _chjrsStartTime :: !(Maybe Text),
    _chjrsARN :: !(Maybe Text),
    _chjrsCreatedAt :: !(Maybe Text),
    _chjrsChannelId :: !(Maybe Text),
    _chjrsS3Destination ::
      !(Maybe S3Destination),
    _chjrsEndTime :: !(Maybe Text),
    _chjrsId :: !(Maybe Text),
    _chjrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateHarvestJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chjrsStatus' - The current status of the HarvestJob. Consider setting up a CloudWatch Event to listen for HarvestJobs as they succeed or fail. In the event of failure, the CloudWatch Event will include an explanation of why the HarvestJob failed.
--
-- * 'chjrsOriginEndpointId' - The ID of the OriginEndpoint that the HarvestJob will harvest from. This cannot be changed after the HarvestJob is submitted.
--
-- * 'chjrsStartTime' - The start of the time-window which will be harvested.
--
-- * 'chjrsARN' - The Amazon Resource Name (ARN) assigned to the HarvestJob.
--
-- * 'chjrsCreatedAt' - The time the HarvestJob was submitted
--
-- * 'chjrsChannelId' - The ID of the Channel that the HarvestJob will harvest from.
--
-- * 'chjrsS3Destination' - Undocumented member.
--
-- * 'chjrsEndTime' - The end of the time-window which will be harvested.
--
-- * 'chjrsId' - The ID of the HarvestJob. The ID must be unique within the region and it cannot be changed after the HarvestJob is submitted.
--
-- * 'chjrsResponseStatus' - -- | The response status code.
createHarvestJobResponse ::
  -- | 'chjrsResponseStatus'
  Int ->
  CreateHarvestJobResponse
createHarvestJobResponse pResponseStatus_ =
  CreateHarvestJobResponse'
    { _chjrsStatus = Nothing,
      _chjrsOriginEndpointId = Nothing,
      _chjrsStartTime = Nothing,
      _chjrsARN = Nothing,
      _chjrsCreatedAt = Nothing,
      _chjrsChannelId = Nothing,
      _chjrsS3Destination = Nothing,
      _chjrsEndTime = Nothing,
      _chjrsId = Nothing,
      _chjrsResponseStatus = pResponseStatus_
    }

-- | The current status of the HarvestJob. Consider setting up a CloudWatch Event to listen for HarvestJobs as they succeed or fail. In the event of failure, the CloudWatch Event will include an explanation of why the HarvestJob failed.
chjrsStatus :: Lens' CreateHarvestJobResponse (Maybe Status)
chjrsStatus = lens _chjrsStatus (\s a -> s {_chjrsStatus = a})

-- | The ID of the OriginEndpoint that the HarvestJob will harvest from. This cannot be changed after the HarvestJob is submitted.
chjrsOriginEndpointId :: Lens' CreateHarvestJobResponse (Maybe Text)
chjrsOriginEndpointId = lens _chjrsOriginEndpointId (\s a -> s {_chjrsOriginEndpointId = a})

-- | The start of the time-window which will be harvested.
chjrsStartTime :: Lens' CreateHarvestJobResponse (Maybe Text)
chjrsStartTime = lens _chjrsStartTime (\s a -> s {_chjrsStartTime = a})

-- | The Amazon Resource Name (ARN) assigned to the HarvestJob.
chjrsARN :: Lens' CreateHarvestJobResponse (Maybe Text)
chjrsARN = lens _chjrsARN (\s a -> s {_chjrsARN = a})

-- | The time the HarvestJob was submitted
chjrsCreatedAt :: Lens' CreateHarvestJobResponse (Maybe Text)
chjrsCreatedAt = lens _chjrsCreatedAt (\s a -> s {_chjrsCreatedAt = a})

-- | The ID of the Channel that the HarvestJob will harvest from.
chjrsChannelId :: Lens' CreateHarvestJobResponse (Maybe Text)
chjrsChannelId = lens _chjrsChannelId (\s a -> s {_chjrsChannelId = a})

-- | Undocumented member.
chjrsS3Destination :: Lens' CreateHarvestJobResponse (Maybe S3Destination)
chjrsS3Destination = lens _chjrsS3Destination (\s a -> s {_chjrsS3Destination = a})

-- | The end of the time-window which will be harvested.
chjrsEndTime :: Lens' CreateHarvestJobResponse (Maybe Text)
chjrsEndTime = lens _chjrsEndTime (\s a -> s {_chjrsEndTime = a})

-- | The ID of the HarvestJob. The ID must be unique within the region and it cannot be changed after the HarvestJob is submitted.
chjrsId :: Lens' CreateHarvestJobResponse (Maybe Text)
chjrsId = lens _chjrsId (\s a -> s {_chjrsId = a})

-- | -- | The response status code.
chjrsResponseStatus :: Lens' CreateHarvestJobResponse Int
chjrsResponseStatus = lens _chjrsResponseStatus (\s a -> s {_chjrsResponseStatus = a})

instance NFData CreateHarvestJobResponse
