{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.HarvestJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.HarvestJob where

import Network.AWS.Lens
import Network.AWS.MediaPackage.Types.S3Destination
import Network.AWS.MediaPackage.Types.Status
import Network.AWS.Prelude

-- | A HarvestJob resource configuration
--
-- /See:/ 'harvestJob' smart constructor.
data HarvestJob = HarvestJob'
  { _hjStatus :: !(Maybe Status),
    _hjOriginEndpointId :: !(Maybe Text),
    _hjStartTime :: !(Maybe Text),
    _hjARN :: !(Maybe Text),
    _hjCreatedAt :: !(Maybe Text),
    _hjChannelId :: !(Maybe Text),
    _hjS3Destination :: !(Maybe S3Destination),
    _hjEndTime :: !(Maybe Text),
    _hjId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HarvestJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hjStatus' - The current status of the HarvestJob. Consider setting up a CloudWatch Event to listen for HarvestJobs as they succeed or fail. In the event of failure, the CloudWatch Event will include an explanation of why the HarvestJob failed.
--
-- * 'hjOriginEndpointId' - The ID of the OriginEndpoint that the HarvestJob will harvest from. This cannot be changed after the HarvestJob is submitted.
--
-- * 'hjStartTime' - The start of the time-window which will be harvested.
--
-- * 'hjARN' - The Amazon Resource Name (ARN) assigned to the HarvestJob.
--
-- * 'hjCreatedAt' - The time the HarvestJob was submitted
--
-- * 'hjChannelId' - The ID of the Channel that the HarvestJob will harvest from.
--
-- * 'hjS3Destination' - Undocumented member.
--
-- * 'hjEndTime' - The end of the time-window which will be harvested.
--
-- * 'hjId' - The ID of the HarvestJob. The ID must be unique within the region and it cannot be changed after the HarvestJob is submitted.
harvestJob ::
  HarvestJob
harvestJob =
  HarvestJob'
    { _hjStatus = Nothing,
      _hjOriginEndpointId = Nothing,
      _hjStartTime = Nothing,
      _hjARN = Nothing,
      _hjCreatedAt = Nothing,
      _hjChannelId = Nothing,
      _hjS3Destination = Nothing,
      _hjEndTime = Nothing,
      _hjId = Nothing
    }

-- | The current status of the HarvestJob. Consider setting up a CloudWatch Event to listen for HarvestJobs as they succeed or fail. In the event of failure, the CloudWatch Event will include an explanation of why the HarvestJob failed.
hjStatus :: Lens' HarvestJob (Maybe Status)
hjStatus = lens _hjStatus (\s a -> s {_hjStatus = a})

-- | The ID of the OriginEndpoint that the HarvestJob will harvest from. This cannot be changed after the HarvestJob is submitted.
hjOriginEndpointId :: Lens' HarvestJob (Maybe Text)
hjOriginEndpointId = lens _hjOriginEndpointId (\s a -> s {_hjOriginEndpointId = a})

-- | The start of the time-window which will be harvested.
hjStartTime :: Lens' HarvestJob (Maybe Text)
hjStartTime = lens _hjStartTime (\s a -> s {_hjStartTime = a})

-- | The Amazon Resource Name (ARN) assigned to the HarvestJob.
hjARN :: Lens' HarvestJob (Maybe Text)
hjARN = lens _hjARN (\s a -> s {_hjARN = a})

-- | The time the HarvestJob was submitted
hjCreatedAt :: Lens' HarvestJob (Maybe Text)
hjCreatedAt = lens _hjCreatedAt (\s a -> s {_hjCreatedAt = a})

-- | The ID of the Channel that the HarvestJob will harvest from.
hjChannelId :: Lens' HarvestJob (Maybe Text)
hjChannelId = lens _hjChannelId (\s a -> s {_hjChannelId = a})

-- | Undocumented member.
hjS3Destination :: Lens' HarvestJob (Maybe S3Destination)
hjS3Destination = lens _hjS3Destination (\s a -> s {_hjS3Destination = a})

-- | The end of the time-window which will be harvested.
hjEndTime :: Lens' HarvestJob (Maybe Text)
hjEndTime = lens _hjEndTime (\s a -> s {_hjEndTime = a})

-- | The ID of the HarvestJob. The ID must be unique within the region and it cannot be changed after the HarvestJob is submitted.
hjId :: Lens' HarvestJob (Maybe Text)
hjId = lens _hjId (\s a -> s {_hjId = a})

instance FromJSON HarvestJob where
  parseJSON =
    withObject
      "HarvestJob"
      ( \x ->
          HarvestJob'
            <$> (x .:? "status")
            <*> (x .:? "originEndpointId")
            <*> (x .:? "startTime")
            <*> (x .:? "arn")
            <*> (x .:? "createdAt")
            <*> (x .:? "channelId")
            <*> (x .:? "s3Destination")
            <*> (x .:? "endTime")
            <*> (x .:? "id")
      )

instance Hashable HarvestJob

instance NFData HarvestJob
