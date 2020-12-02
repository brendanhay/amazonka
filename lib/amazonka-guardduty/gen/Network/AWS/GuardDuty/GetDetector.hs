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
-- Module      : Network.AWS.GuardDuty.GetDetector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves an Amazon GuardDuty detector specified by the detectorId.
module Network.AWS.GuardDuty.GetDetector
  ( -- * Creating a Request
    getDetector,
    GetDetector,

    -- * Request Lenses
    gdDetectorId,

    -- * Destructuring the Response
    getDetectorResponse,
    GetDetectorResponse,

    -- * Response Lenses
    gdrsCreatedAt,
    gdrsFindingPublishingFrequency,
    gdrsDataSources,
    gdrsUpdatedAt,
    gdrsTags,
    gdrsResponseStatus,
    gdrsServiceRole,
    gdrsStatus,
  )
where

import Network.AWS.GuardDuty.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getDetector' smart constructor.
newtype GetDetector = GetDetector' {_gdDetectorId :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetDetector' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdDetectorId' - The unique ID of the detector that you want to get.
getDetector ::
  -- | 'gdDetectorId'
  Text ->
  GetDetector
getDetector pDetectorId_ =
  GetDetector' {_gdDetectorId = pDetectorId_}

-- | The unique ID of the detector that you want to get.
gdDetectorId :: Lens' GetDetector Text
gdDetectorId = lens _gdDetectorId (\s a -> s {_gdDetectorId = a})

instance AWSRequest GetDetector where
  type Rs GetDetector = GetDetectorResponse
  request = get guardDuty
  response =
    receiveJSON
      ( \s h x ->
          GetDetectorResponse'
            <$> (x .?> "createdAt")
            <*> (x .?> "findingPublishingFrequency")
            <*> (x .?> "dataSources")
            <*> (x .?> "updatedAt")
            <*> (x .?> "tags" .!@ mempty)
            <*> (pure (fromEnum s))
            <*> (x .:> "serviceRole")
            <*> (x .:> "status")
      )

instance Hashable GetDetector

instance NFData GetDetector

instance ToHeaders GetDetector where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath GetDetector where
  toPath GetDetector' {..} =
    mconcat ["/detector/", toBS _gdDetectorId]

instance ToQuery GetDetector where
  toQuery = const mempty

-- | /See:/ 'getDetectorResponse' smart constructor.
data GetDetectorResponse = GetDetectorResponse'
  { _gdrsCreatedAt ::
      !(Maybe Text),
    _gdrsFindingPublishingFrequency ::
      !(Maybe FindingPublishingFrequency),
    _gdrsDataSources ::
      !(Maybe DataSourceConfigurationsResult),
    _gdrsUpdatedAt :: !(Maybe Text),
    _gdrsTags :: !(Maybe (Map Text (Text))),
    _gdrsResponseStatus :: !Int,
    _gdrsServiceRole :: !Text,
    _gdrsStatus :: !DetectorStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetDetectorResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdrsCreatedAt' - The timestamp of when the detector was created.
--
-- * 'gdrsFindingPublishingFrequency' - The publishing frequency of the finding.
--
-- * 'gdrsDataSources' - An object that describes which data sources are enabled for the detector.
--
-- * 'gdrsUpdatedAt' - The last-updated timestamp for the detector.
--
-- * 'gdrsTags' - The tags of the detector resource.
--
-- * 'gdrsResponseStatus' - -- | The response status code.
--
-- * 'gdrsServiceRole' - The GuardDuty service role.
--
-- * 'gdrsStatus' - The detector status.
getDetectorResponse ::
  -- | 'gdrsResponseStatus'
  Int ->
  -- | 'gdrsServiceRole'
  Text ->
  -- | 'gdrsStatus'
  DetectorStatus ->
  GetDetectorResponse
getDetectorResponse pResponseStatus_ pServiceRole_ pStatus_ =
  GetDetectorResponse'
    { _gdrsCreatedAt = Nothing,
      _gdrsFindingPublishingFrequency = Nothing,
      _gdrsDataSources = Nothing,
      _gdrsUpdatedAt = Nothing,
      _gdrsTags = Nothing,
      _gdrsResponseStatus = pResponseStatus_,
      _gdrsServiceRole = pServiceRole_,
      _gdrsStatus = pStatus_
    }

-- | The timestamp of when the detector was created.
gdrsCreatedAt :: Lens' GetDetectorResponse (Maybe Text)
gdrsCreatedAt = lens _gdrsCreatedAt (\s a -> s {_gdrsCreatedAt = a})

-- | The publishing frequency of the finding.
gdrsFindingPublishingFrequency :: Lens' GetDetectorResponse (Maybe FindingPublishingFrequency)
gdrsFindingPublishingFrequency = lens _gdrsFindingPublishingFrequency (\s a -> s {_gdrsFindingPublishingFrequency = a})

-- | An object that describes which data sources are enabled for the detector.
gdrsDataSources :: Lens' GetDetectorResponse (Maybe DataSourceConfigurationsResult)
gdrsDataSources = lens _gdrsDataSources (\s a -> s {_gdrsDataSources = a})

-- | The last-updated timestamp for the detector.
gdrsUpdatedAt :: Lens' GetDetectorResponse (Maybe Text)
gdrsUpdatedAt = lens _gdrsUpdatedAt (\s a -> s {_gdrsUpdatedAt = a})

-- | The tags of the detector resource.
gdrsTags :: Lens' GetDetectorResponse (HashMap Text (Text))
gdrsTags = lens _gdrsTags (\s a -> s {_gdrsTags = a}) . _Default . _Map

-- | -- | The response status code.
gdrsResponseStatus :: Lens' GetDetectorResponse Int
gdrsResponseStatus = lens _gdrsResponseStatus (\s a -> s {_gdrsResponseStatus = a})

-- | The GuardDuty service role.
gdrsServiceRole :: Lens' GetDetectorResponse Text
gdrsServiceRole = lens _gdrsServiceRole (\s a -> s {_gdrsServiceRole = a})

-- | The detector status.
gdrsStatus :: Lens' GetDetectorResponse DetectorStatus
gdrsStatus = lens _gdrsStatus (\s a -> s {_gdrsStatus = a})

instance NFData GetDetectorResponse
