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
-- Module      : Network.AWS.GuardDuty.DescribePublishingDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the publishing destination specified by the provided @destinationId@ .
module Network.AWS.GuardDuty.DescribePublishingDestination
  ( -- * Creating a Request
    describePublishingDestination,
    DescribePublishingDestination,

    -- * Request Lenses
    desDetectorId,
    desDestinationId,

    -- * Destructuring the Response
    describePublishingDestinationResponse,
    DescribePublishingDestinationResponse,

    -- * Response Lenses
    desrsResponseStatus,
    desrsDestinationId,
    desrsDestinationType,
    desrsStatus,
    desrsPublishingFailureStartTimestamp,
    desrsDestinationProperties,
  )
where

import Network.AWS.GuardDuty.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describePublishingDestination' smart constructor.
data DescribePublishingDestination = DescribePublishingDestination'
  { _desDetectorId ::
      !Text,
    _desDestinationId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribePublishingDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desDetectorId' - The unique ID of the detector associated with the publishing destination to retrieve.
--
-- * 'desDestinationId' - The ID of the publishing destination to retrieve.
describePublishingDestination ::
  -- | 'desDetectorId'
  Text ->
  -- | 'desDestinationId'
  Text ->
  DescribePublishingDestination
describePublishingDestination pDetectorId_ pDestinationId_ =
  DescribePublishingDestination'
    { _desDetectorId = pDetectorId_,
      _desDestinationId = pDestinationId_
    }

-- | The unique ID of the detector associated with the publishing destination to retrieve.
desDetectorId :: Lens' DescribePublishingDestination Text
desDetectorId = lens _desDetectorId (\s a -> s {_desDetectorId = a})

-- | The ID of the publishing destination to retrieve.
desDestinationId :: Lens' DescribePublishingDestination Text
desDestinationId = lens _desDestinationId (\s a -> s {_desDestinationId = a})

instance AWSRequest DescribePublishingDestination where
  type
    Rs DescribePublishingDestination =
      DescribePublishingDestinationResponse
  request = get guardDuty
  response =
    receiveJSON
      ( \s h x ->
          DescribePublishingDestinationResponse'
            <$> (pure (fromEnum s))
            <*> (x .:> "destinationId")
            <*> (x .:> "destinationType")
            <*> (x .:> "status")
            <*> (x .:> "publishingFailureStartTimestamp")
            <*> (x .:> "destinationProperties")
      )

instance Hashable DescribePublishingDestination

instance NFData DescribePublishingDestination

instance ToHeaders DescribePublishingDestination where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath DescribePublishingDestination where
  toPath DescribePublishingDestination' {..} =
    mconcat
      [ "/detector/",
        toBS _desDetectorId,
        "/publishingDestination/",
        toBS _desDestinationId
      ]

instance ToQuery DescribePublishingDestination where
  toQuery = const mempty

-- | /See:/ 'describePublishingDestinationResponse' smart constructor.
data DescribePublishingDestinationResponse = DescribePublishingDestinationResponse'
  { _desrsResponseStatus ::
      !Int,
    _desrsDestinationId ::
      !Text,
    _desrsDestinationType ::
      !DestinationType,
    _desrsStatus ::
      !PublishingStatus,
    _desrsPublishingFailureStartTimestamp ::
      !Integer,
    _desrsDestinationProperties ::
      !DestinationProperties
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribePublishingDestinationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desrsResponseStatus' - -- | The response status code.
--
-- * 'desrsDestinationId' - The ID of the publishing destination.
--
-- * 'desrsDestinationType' - The type of publishing destination. Currently, only Amazon S3 buckets are supported.
--
-- * 'desrsStatus' - The status of the publishing destination.
--
-- * 'desrsPublishingFailureStartTimestamp' - The time, in epoch millisecond format, at which GuardDuty was first unable to publish findings to the destination.
--
-- * 'desrsDestinationProperties' - A @DestinationProperties@ object that includes the @DestinationArn@ and @KmsKeyArn@ of the publishing destination.
describePublishingDestinationResponse ::
  -- | 'desrsResponseStatus'
  Int ->
  -- | 'desrsDestinationId'
  Text ->
  -- | 'desrsDestinationType'
  DestinationType ->
  -- | 'desrsStatus'
  PublishingStatus ->
  -- | 'desrsPublishingFailureStartTimestamp'
  Integer ->
  -- | 'desrsDestinationProperties'
  DestinationProperties ->
  DescribePublishingDestinationResponse
describePublishingDestinationResponse
  pResponseStatus_
  pDestinationId_
  pDestinationType_
  pStatus_
  pPublishingFailureStartTimestamp_
  pDestinationProperties_ =
    DescribePublishingDestinationResponse'
      { _desrsResponseStatus =
          pResponseStatus_,
        _desrsDestinationId = pDestinationId_,
        _desrsDestinationType = pDestinationType_,
        _desrsStatus = pStatus_,
        _desrsPublishingFailureStartTimestamp =
          pPublishingFailureStartTimestamp_,
        _desrsDestinationProperties = pDestinationProperties_
      }

-- | -- | The response status code.
desrsResponseStatus :: Lens' DescribePublishingDestinationResponse Int
desrsResponseStatus = lens _desrsResponseStatus (\s a -> s {_desrsResponseStatus = a})

-- | The ID of the publishing destination.
desrsDestinationId :: Lens' DescribePublishingDestinationResponse Text
desrsDestinationId = lens _desrsDestinationId (\s a -> s {_desrsDestinationId = a})

-- | The type of publishing destination. Currently, only Amazon S3 buckets are supported.
desrsDestinationType :: Lens' DescribePublishingDestinationResponse DestinationType
desrsDestinationType = lens _desrsDestinationType (\s a -> s {_desrsDestinationType = a})

-- | The status of the publishing destination.
desrsStatus :: Lens' DescribePublishingDestinationResponse PublishingStatus
desrsStatus = lens _desrsStatus (\s a -> s {_desrsStatus = a})

-- | The time, in epoch millisecond format, at which GuardDuty was first unable to publish findings to the destination.
desrsPublishingFailureStartTimestamp :: Lens' DescribePublishingDestinationResponse Integer
desrsPublishingFailureStartTimestamp = lens _desrsPublishingFailureStartTimestamp (\s a -> s {_desrsPublishingFailureStartTimestamp = a})

-- | A @DestinationProperties@ object that includes the @DestinationArn@ and @KmsKeyArn@ of the publishing destination.
desrsDestinationProperties :: Lens' DescribePublishingDestinationResponse DestinationProperties
desrsDestinationProperties = lens _desrsDestinationProperties (\s a -> s {_desrsDestinationProperties = a})

instance NFData DescribePublishingDestinationResponse
