{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DownloadDBLogFilePortion
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Downloads all or a portion of the specified log file.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DownloadDBLogFilePortion.html>
module Network.AWS.RDS.DownloadDBLogFilePortion
    (
    -- * Request
      DownloadDBLogFilePortion
    -- ** Request constructor
    , downloadDBLogFilePortion
    -- ** Request lenses
    , ddlfprqNumberOfLines
    , ddlfprqMarker
    , ddlfprqDBInstanceIdentifier
    , ddlfprqLogFileName

    -- * Response
    , DownloadDBLogFilePortionResponse
    -- ** Response constructor
    , downloadDBLogFilePortionResponse
    -- ** Response lenses
    , ddlfprsLogFileData
    , ddlfprsAdditionalDataPending
    , ddlfprsMarker
    , ddlfprsStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'downloadDBLogFilePortion' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddlfprqNumberOfLines'
--
-- * 'ddlfprqMarker'
--
-- * 'ddlfprqDBInstanceIdentifier'
--
-- * 'ddlfprqLogFileName'
data DownloadDBLogFilePortion = DownloadDBLogFilePortion'
    { _ddlfprqNumberOfLines        :: !(Maybe Int)
    , _ddlfprqMarker               :: !(Maybe Text)
    , _ddlfprqDBInstanceIdentifier :: !Text
    , _ddlfprqLogFileName          :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DownloadDBLogFilePortion' smart constructor.
downloadDBLogFilePortion :: Text -> Text -> DownloadDBLogFilePortion
downloadDBLogFilePortion pDBInstanceIdentifier_ pLogFileName_ =
    DownloadDBLogFilePortion'
    { _ddlfprqNumberOfLines = Nothing
    , _ddlfprqMarker = Nothing
    , _ddlfprqDBInstanceIdentifier = pDBInstanceIdentifier_
    , _ddlfprqLogFileName = pLogFileName_
    }

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
ddlfprqNumberOfLines :: Lens' DownloadDBLogFilePortion (Maybe Int)
ddlfprqNumberOfLines = lens _ddlfprqNumberOfLines (\ s a -> s{_ddlfprqNumberOfLines = a});

-- | The pagination token provided in the previous request or \"0\". If the
-- Marker parameter is specified the response includes only records beyond
-- the marker until the end of the file or up to NumberOfLines.
ddlfprqMarker :: Lens' DownloadDBLogFilePortion (Maybe Text)
ddlfprqMarker = lens _ddlfprqMarker (\ s a -> s{_ddlfprqMarker = a});

-- | The customer-assigned name of the DB instance that contains the log
-- files you want to list.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 alphanumeric characters or hyphens
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
ddlfprqDBInstanceIdentifier :: Lens' DownloadDBLogFilePortion Text
ddlfprqDBInstanceIdentifier = lens _ddlfprqDBInstanceIdentifier (\ s a -> s{_ddlfprqDBInstanceIdentifier = a});

-- | The name of the log file to be downloaded.
ddlfprqLogFileName :: Lens' DownloadDBLogFilePortion Text
ddlfprqLogFileName = lens _ddlfprqLogFileName (\ s a -> s{_ddlfprqLogFileName = a});

instance AWSPager DownloadDBLogFilePortion where
        page rq rs
          | stop (rs ^. ddlfprsAdditionalDataPending) = Nothing
          | isNothing (rs ^. ddlfprsMarker) = Nothing
          | otherwise =
            Just $ rq & ddlfprqMarker .~ rs ^. ddlfprsMarker

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
                     <*> (x .@? "Marker")
                     <*> (pure (fromEnum s)))

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
               "NumberOfLines" =: _ddlfprqNumberOfLines,
               "Marker" =: _ddlfprqMarker,
               "DBInstanceIdentifier" =:
                 _ddlfprqDBInstanceIdentifier,
               "LogFileName" =: _ddlfprqLogFileName]

-- | This data type is used as a response element to
-- DownloadDBLogFilePortion.
--
-- /See:/ 'downloadDBLogFilePortionResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddlfprsLogFileData'
--
-- * 'ddlfprsAdditionalDataPending'
--
-- * 'ddlfprsMarker'
--
-- * 'ddlfprsStatus'
data DownloadDBLogFilePortionResponse = DownloadDBLogFilePortionResponse'
    { _ddlfprsLogFileData           :: !(Maybe Text)
    , _ddlfprsAdditionalDataPending :: !(Maybe Bool)
    , _ddlfprsMarker                :: !(Maybe Text)
    , _ddlfprsStatus                :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DownloadDBLogFilePortionResponse' smart constructor.
downloadDBLogFilePortionResponse :: Int -> DownloadDBLogFilePortionResponse
downloadDBLogFilePortionResponse pStatus_ =
    DownloadDBLogFilePortionResponse'
    { _ddlfprsLogFileData = Nothing
    , _ddlfprsAdditionalDataPending = Nothing
    , _ddlfprsMarker = Nothing
    , _ddlfprsStatus = pStatus_
    }

-- | Entries from the specified log file.
ddlfprsLogFileData :: Lens' DownloadDBLogFilePortionResponse (Maybe Text)
ddlfprsLogFileData = lens _ddlfprsLogFileData (\ s a -> s{_ddlfprsLogFileData = a});

-- | Boolean value that if true, indicates there is more data to be
-- downloaded.
ddlfprsAdditionalDataPending :: Lens' DownloadDBLogFilePortionResponse (Maybe Bool)
ddlfprsAdditionalDataPending = lens _ddlfprsAdditionalDataPending (\ s a -> s{_ddlfprsAdditionalDataPending = a});

-- | A pagination token that can be used in a subsequent
-- DownloadDBLogFilePortion request.
ddlfprsMarker :: Lens' DownloadDBLogFilePortionResponse (Maybe Text)
ddlfprsMarker = lens _ddlfprsMarker (\ s a -> s{_ddlfprsMarker = a});

-- | FIXME: Undocumented member.
ddlfprsStatus :: Lens' DownloadDBLogFilePortionResponse Int
ddlfprsStatus = lens _ddlfprsStatus (\ s a -> s{_ddlfprsStatus = a});
