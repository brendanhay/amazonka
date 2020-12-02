{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.ExportInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.ExportInfo where

import Network.AWS.Discovery.Types.ExportStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information regarding the export status of discovered data. The value is an array of objects.
--
--
--
-- /See:/ 'exportInfo' smart constructor.
data ExportInfo = ExportInfo'
  { _eiConfigurationsDownloadURL ::
      !(Maybe Text),
    _eiRequestedStartTime :: !(Maybe POSIX),
    _eiRequestedEndTime :: !(Maybe POSIX),
    _eiIsTruncated :: !(Maybe Bool),
    _eiExportId :: !Text,
    _eiExportStatus :: !ExportStatus,
    _eiStatusMessage :: !Text,
    _eiExportRequestTime :: !POSIX
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExportInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eiConfigurationsDownloadURL' - A URL for an Amazon S3 bucket where you can review the exported data. The URL is displayed only if the export succeeded.
--
-- * 'eiRequestedStartTime' - The value of @startTime@ parameter in the @StartExportTask@ request. If no @startTime@ was requested, this result does not appear in @ExportInfo@ .
--
-- * 'eiRequestedEndTime' - The @endTime@ used in the @StartExportTask@ request. If no @endTime@ was requested, this result does not appear in @ExportInfo@ .
--
-- * 'eiIsTruncated' - If true, the export of agent information exceeded the size limit for a single export and the exported data is incomplete for the requested time range. To address this, select a smaller time range for the export by using @startDate@ and @endDate@ .
--
-- * 'eiExportId' - A unique identifier used to query an export.
--
-- * 'eiExportStatus' - The status of the data export job.
--
-- * 'eiStatusMessage' - A status message provided for API callers.
--
-- * 'eiExportRequestTime' - The time that the data export was initiated.
exportInfo ::
  -- | 'eiExportId'
  Text ->
  -- | 'eiExportStatus'
  ExportStatus ->
  -- | 'eiStatusMessage'
  Text ->
  -- | 'eiExportRequestTime'
  UTCTime ->
  ExportInfo
exportInfo
  pExportId_
  pExportStatus_
  pStatusMessage_
  pExportRequestTime_ =
    ExportInfo'
      { _eiConfigurationsDownloadURL = Nothing,
        _eiRequestedStartTime = Nothing,
        _eiRequestedEndTime = Nothing,
        _eiIsTruncated = Nothing,
        _eiExportId = pExportId_,
        _eiExportStatus = pExportStatus_,
        _eiStatusMessage = pStatusMessage_,
        _eiExportRequestTime = _Time # pExportRequestTime_
      }

-- | A URL for an Amazon S3 bucket where you can review the exported data. The URL is displayed only if the export succeeded.
eiConfigurationsDownloadURL :: Lens' ExportInfo (Maybe Text)
eiConfigurationsDownloadURL = lens _eiConfigurationsDownloadURL (\s a -> s {_eiConfigurationsDownloadURL = a})

-- | The value of @startTime@ parameter in the @StartExportTask@ request. If no @startTime@ was requested, this result does not appear in @ExportInfo@ .
eiRequestedStartTime :: Lens' ExportInfo (Maybe UTCTime)
eiRequestedStartTime = lens _eiRequestedStartTime (\s a -> s {_eiRequestedStartTime = a}) . mapping _Time

-- | The @endTime@ used in the @StartExportTask@ request. If no @endTime@ was requested, this result does not appear in @ExportInfo@ .
eiRequestedEndTime :: Lens' ExportInfo (Maybe UTCTime)
eiRequestedEndTime = lens _eiRequestedEndTime (\s a -> s {_eiRequestedEndTime = a}) . mapping _Time

-- | If true, the export of agent information exceeded the size limit for a single export and the exported data is incomplete for the requested time range. To address this, select a smaller time range for the export by using @startDate@ and @endDate@ .
eiIsTruncated :: Lens' ExportInfo (Maybe Bool)
eiIsTruncated = lens _eiIsTruncated (\s a -> s {_eiIsTruncated = a})

-- | A unique identifier used to query an export.
eiExportId :: Lens' ExportInfo Text
eiExportId = lens _eiExportId (\s a -> s {_eiExportId = a})

-- | The status of the data export job.
eiExportStatus :: Lens' ExportInfo ExportStatus
eiExportStatus = lens _eiExportStatus (\s a -> s {_eiExportStatus = a})

-- | A status message provided for API callers.
eiStatusMessage :: Lens' ExportInfo Text
eiStatusMessage = lens _eiStatusMessage (\s a -> s {_eiStatusMessage = a})

-- | The time that the data export was initiated.
eiExportRequestTime :: Lens' ExportInfo UTCTime
eiExportRequestTime = lens _eiExportRequestTime (\s a -> s {_eiExportRequestTime = a}) . _Time

instance FromJSON ExportInfo where
  parseJSON =
    withObject
      "ExportInfo"
      ( \x ->
          ExportInfo'
            <$> (x .:? "configurationsDownloadUrl")
            <*> (x .:? "requestedStartTime")
            <*> (x .:? "requestedEndTime")
            <*> (x .:? "isTruncated")
            <*> (x .: "exportId")
            <*> (x .: "exportStatus")
            <*> (x .: "statusMessage")
            <*> (x .: "exportRequestTime")
      )

instance Hashable ExportInfo

instance NFData ExportInfo
