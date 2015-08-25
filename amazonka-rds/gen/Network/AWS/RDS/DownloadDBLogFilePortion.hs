{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DownloadDBLogFilePortion
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Downloads all or a portion of the specified log file, up to 1 MB in
-- size.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DownloadDBLogFilePortion.html AWS API Reference> for DownloadDBLogFilePortion.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DownloadDBLogFilePortion
    (
    -- * Creating a Request
      downloadDBLogFilePortion
    , DownloadDBLogFilePortion
    -- * Request Lenses
    , ddlfpNumberOfLines
    , ddlfpMarker
    , ddlfpDBInstanceIdentifier
    , ddlfpLogFileName

    -- * Destructuring the Response
    , downloadDBLogFilePortionResponse
    , DownloadDBLogFilePortionResponse
    -- * Response Lenses
    , ddlfprsLogFileData
    , ddlfprsAdditionalDataPending
    , ddlfprsMarker
    , ddlfprsStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.RDS.Types.Product
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'downloadDBLogFilePortion' smart constructor.
data DownloadDBLogFilePortion = DownloadDBLogFilePortion'
    { _ddlfpNumberOfLines        :: !(Maybe Int)
    , _ddlfpMarker               :: !(Maybe Text)
    , _ddlfpDBInstanceIdentifier :: !Text
    , _ddlfpLogFileName          :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DownloadDBLogFilePortion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddlfpNumberOfLines'
--
-- * 'ddlfpMarker'
--
-- * 'ddlfpDBInstanceIdentifier'
--
-- * 'ddlfpLogFileName'
downloadDBLogFilePortion
    :: Text -- ^ 'ddlfpDBInstanceIdentifier'
    -> Text -- ^ 'ddlfpLogFileName'
    -> DownloadDBLogFilePortion
downloadDBLogFilePortion pDBInstanceIdentifier_ pLogFileName_ =
    DownloadDBLogFilePortion'
    { _ddlfpNumberOfLines = Nothing
    , _ddlfpMarker = Nothing
    , _ddlfpDBInstanceIdentifier = pDBInstanceIdentifier_
    , _ddlfpLogFileName = pLogFileName_
    }

-- | The number of lines to download. If the number of lines specified
-- results in a file over 1 MB in size, the file will be truncated at 1 MB
-- in size.
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
          | stop (rs ^. ddlfprsAdditionalDataPending) = Nothing
          | isNothing (rs ^. ddlfprsMarker) = Nothing
          | otherwise =
            Just $ rq & ddlfpMarker .~ rs ^. ddlfprsMarker

instance AWSRequest DownloadDBLogFilePortion where
        type Rs DownloadDBLogFilePortion =
             DownloadDBLogFilePortionResponse
        request = postQuery rDS
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
               "NumberOfLines" =: _ddlfpNumberOfLines,
               "Marker" =: _ddlfpMarker,
               "DBInstanceIdentifier" =: _ddlfpDBInstanceIdentifier,
               "LogFileName" =: _ddlfpLogFileName]

-- | This data type is used as a response element to
-- DownloadDBLogFilePortion.
--
-- /See:/ 'downloadDBLogFilePortionResponse' smart constructor.
data DownloadDBLogFilePortionResponse = DownloadDBLogFilePortionResponse'
    { _ddlfprsLogFileData           :: !(Maybe Text)
    , _ddlfprsAdditionalDataPending :: !(Maybe Bool)
    , _ddlfprsMarker                :: !(Maybe Text)
    , _ddlfprsStatus                :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DownloadDBLogFilePortionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddlfprsLogFileData'
--
-- * 'ddlfprsAdditionalDataPending'
--
-- * 'ddlfprsMarker'
--
-- * 'ddlfprsStatus'
downloadDBLogFilePortionResponse
    :: Int -- ^ 'ddlfprsStatus'
    -> DownloadDBLogFilePortionResponse
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

-- | The response status code.
ddlfprsStatus :: Lens' DownloadDBLogFilePortionResponse Int
ddlfprsStatus = lens _ddlfprsStatus (\ s a -> s{_ddlfprsStatus = a});
