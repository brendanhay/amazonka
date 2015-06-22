{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.RDS.DownloadDBLogFilePortion
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Downloads all or a portion of the specified log file.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DownloadDBLogFilePortion.html>
module Network.AWS.RDS.DownloadDBLogFilePortion
    (
    -- * Request
      DownloadDBLogFilePortion
    -- ** Request constructor
    , downloadDBLogFilePortion
    -- ** Request lenses
    , ddlfpNumberOfLines
    , ddlfpMarker
    , ddlfpDBInstanceIdentifier
    , ddlfpLogFileName

    -- * Response
    , DownloadDBLogFilePortionResponse
    -- ** Response constructor
    , downloadDBLogFilePortionResponse
    -- ** Response lenses
    , ddlfprLogFileData
    , ddlfprAdditionalDataPending
    , ddlfprMarker
    ) where

import Network.AWS.Pagers
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'downloadDBLogFilePortion' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddlfpNumberOfLines'
--
-- * 'ddlfpMarker'
--
-- * 'ddlfpDBInstanceIdentifier'
--
-- * 'ddlfpLogFileName'
data DownloadDBLogFilePortion = DownloadDBLogFilePortion'{_ddlfpNumberOfLines :: Maybe Int, _ddlfpMarker :: Maybe Text, _ddlfpDBInstanceIdentifier :: Text, _ddlfpLogFileName :: Text} deriving (Eq, Read, Show)

-- | 'DownloadDBLogFilePortion' smart constructor.
downloadDBLogFilePortion :: Text -> Text -> DownloadDBLogFilePortion
downloadDBLogFilePortion pDBInstanceIdentifier pLogFileName = DownloadDBLogFilePortion'{_ddlfpNumberOfLines = Nothing, _ddlfpMarker = Nothing, _ddlfpDBInstanceIdentifier = pDBInstanceIdentifier, _ddlfpLogFileName = pLogFileName};

-- | The number of lines to download.
--
-- If the NumberOfLines parameter is specified, then the block of lines
-- returned can be from the beginning or the end of the log file, depending
-- on the value of the Marker parameter.
--
-- -   If neither Marker or NumberOfLines are specified, the entire log
--     file is returned.
--
-- -   If NumberOfLines is specified and Marker is not specified, then the
--     most recent lines from the end of the log file are returned.
--
-- -   If Marker is specified as \"0\", then the specified number of lines
--     from the beginning of the log file are returned.
--
-- -   You can download the log file in blocks of lines by specifying the
--     size of the block using the NumberOfLines parameter, and by
--     specifying a value of \"0\" for the Marker parameter in your first
--     request. Include the Marker value returned in the response as the
--     Marker value for the next request, continuing until the
--     AdditionalDataPending response element returns false.
--
ddlfpNumberOfLines :: Lens' DownloadDBLogFilePortion (Maybe Int)
ddlfpNumberOfLines = lens _ddlfpNumberOfLines (\ s a -> s{_ddlfpNumberOfLines = a});

-- | The pagination token provided in the previous request or \"0\". If the
-- Marker parameter is specified the response includes only records beyond
-- the marker until the end of the file or up to NumberOfLines.
ddlfpMarker :: Lens' DownloadDBLogFilePortion (Maybe Text)
ddlfpMarker = lens _ddlfpMarker (\ s a -> s{_ddlfpMarker = a});

-- | The customer-assigned name of the DB instance that contains the log
-- files you want to list.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 alphanumeric characters or hyphens
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
ddlfpDBInstanceIdentifier :: Lens' DownloadDBLogFilePortion Text
ddlfpDBInstanceIdentifier = lens _ddlfpDBInstanceIdentifier (\ s a -> s{_ddlfpDBInstanceIdentifier = a});

-- | The name of the log file to be downloaded.
ddlfpLogFileName :: Lens' DownloadDBLogFilePortion Text
ddlfpLogFileName = lens _ddlfpLogFileName (\ s a -> s{_ddlfpLogFileName = a});

instance AWSPager DownloadDBLogFilePortion where
        page rq rs
          | stop (rs ^. ddlfprAdditionalDataPending) = Nothing
          | isNothing (rs ^. ddlfprMarker) = Nothing
          | otherwise =
            Just $ rq & ddlfpMarker .~ rs ^. ddlfprMarker

instance AWSRequest DownloadDBLogFilePortion where
        type Sv DownloadDBLogFilePortion = RDS
        type Rs DownloadDBLogFilePortion =
             DownloadDBLogFilePortionResponse
        request = post
        response
          = receiveXMLWrapper "DownloadDBLogFilePortionResult"
              (\ s h x ->
                 DownloadDBLogFilePortionResponse' <$>
                   (x .@? "LogFileData") <*>
                     (x .@? "AdditionalDataPending")
                     <*> (x .@? "Marker"))

instance ToHeaders DownloadDBLogFilePortion where
        toHeaders = const mempty

instance ToPath DownloadDBLogFilePortion where
        toPath = const "/"

instance ToQuery DownloadDBLogFilePortion where
        toQuery DownloadDBLogFilePortion'{..}
          = mconcat
              ["Action" =:
                 ("DownloadDBLogFilePortion" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "NumberOfLines" =: _ddlfpNumberOfLines,
               "Marker" =: _ddlfpMarker,
               "DBInstanceIdentifier" =: _ddlfpDBInstanceIdentifier,
               "LogFileName" =: _ddlfpLogFileName]

-- | /See:/ 'downloadDBLogFilePortionResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddlfprLogFileData'
--
-- * 'ddlfprAdditionalDataPending'
--
-- * 'ddlfprMarker'
data DownloadDBLogFilePortionResponse = DownloadDBLogFilePortionResponse'{_ddlfprLogFileData :: Maybe Text, _ddlfprAdditionalDataPending :: Maybe Bool, _ddlfprMarker :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DownloadDBLogFilePortionResponse' smart constructor.
downloadDBLogFilePortionResponse :: DownloadDBLogFilePortionResponse
downloadDBLogFilePortionResponse = DownloadDBLogFilePortionResponse'{_ddlfprLogFileData = Nothing, _ddlfprAdditionalDataPending = Nothing, _ddlfprMarker = Nothing};

-- | Entries from the specified log file.
ddlfprLogFileData :: Lens' DownloadDBLogFilePortionResponse (Maybe Text)
ddlfprLogFileData = lens _ddlfprLogFileData (\ s a -> s{_ddlfprLogFileData = a});

-- | Boolean value that if true, indicates there is more data to be
-- downloaded.
ddlfprAdditionalDataPending :: Lens' DownloadDBLogFilePortionResponse (Maybe Bool)
ddlfprAdditionalDataPending = lens _ddlfprAdditionalDataPending (\ s a -> s{_ddlfprAdditionalDataPending = a});

-- | A pagination token that can be used in a subsequent
-- DownloadDBLogFilePortion request.
ddlfprMarker :: Lens' DownloadDBLogFilePortionResponse (Maybe Text)
ddlfprMarker = lens _ddlfprMarker (\ s a -> s{_ddlfprMarker = a});
