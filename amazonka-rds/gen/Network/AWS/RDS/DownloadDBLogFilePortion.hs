{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.DownloadDBLogFilePortion
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

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
    , ddblfpDBInstanceIdentifier
    , ddblfpLogFileName
    , ddblfpMarker
    , ddblfpNumberOfLines

    -- * Response
    , DownloadDBLogFilePortionResponse
    -- ** Response constructor
    , downloadDBLogFilePortionResponse
    -- ** Response lenses
    , ddblfprAdditionalDataPending
    , ddblfprLogFileData
    , ddblfprMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import qualified GHC.Exts

data DownloadDBLogFilePortion = DownloadDBLogFilePortion
    { _ddblfpDBInstanceIdentifier :: Text
    , _ddblfpLogFileName          :: Text
    , _ddblfpMarker               :: Maybe Text
    , _ddblfpNumberOfLines        :: Maybe Int
    } deriving (Eq, Ord, Show, Generic)

-- | 'DownloadDBLogFilePortion' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddblfpDBInstanceIdentifier' @::@ 'Text'
--
-- * 'ddblfpLogFileName' @::@ 'Text'
--
-- * 'ddblfpMarker' @::@ 'Maybe' 'Text'
--
-- * 'ddblfpNumberOfLines' @::@ 'Maybe' 'Int'
--
downloadDBLogFilePortion :: Text -- ^ 'ddblfpDBInstanceIdentifier'
                         -> Text -- ^ 'ddblfpLogFileName'
                         -> DownloadDBLogFilePortion
downloadDBLogFilePortion p1 p2 = DownloadDBLogFilePortion
    { _ddblfpDBInstanceIdentifier = p1
    , _ddblfpLogFileName          = p2
    , _ddblfpMarker               = Nothing
    , _ddblfpNumberOfLines        = Nothing
    }

-- | The customer-assigned name of the DB instance that contains the log files
-- you want to list. Constraints: Must contain from 1 to 63 alphanumeric
-- characters or hyphens First character must be a letter Cannot end with a
-- hyphen or contain two consecutive hyphens.
ddblfpDBInstanceIdentifier :: Lens' DownloadDBLogFilePortion Text
ddblfpDBInstanceIdentifier =
    lens _ddblfpDBInstanceIdentifier
        (\s a -> s { _ddblfpDBInstanceIdentifier = a })

-- | The name of the log file to be downloaded.
ddblfpLogFileName :: Lens' DownloadDBLogFilePortion Text
ddblfpLogFileName =
    lens _ddblfpLogFileName (\s a -> s { _ddblfpLogFileName = a })

-- | The pagination token provided in the previous request or "0". If the
-- Marker parameter is specified the response includes only records beyond
-- the marker until the end of the file or up to NumberOfLines.
ddblfpMarker :: Lens' DownloadDBLogFilePortion (Maybe Text)
ddblfpMarker = lens _ddblfpMarker (\s a -> s { _ddblfpMarker = a })

-- | The number of lines to download. If the NumberOfLines parameter is
-- specified, then the block of lines returned can be from the beginning or
-- the end of the log file, depending on the value of the Marker parameter.
-- If neither Marker or NumberOfLines are specified, the entire log file is
-- returned. If NumberOfLines is specified and Marker is not specified, then
-- the most recent lines from the end of the log file are returned. If
-- Marker is specified as "0", then the specified number of lines from the
-- beginning of the log file are returned. You can download the log file in
-- blocks of lines by specifying the size of the block using the
-- NumberOfLines parameter, and by specifying a value of "0" for the Marker
-- parameter in your first request. Include the Marker value returned in the
-- response as the Marker value for the next request, continuing until the
-- AdditionalDataPending response element returns false.
ddblfpNumberOfLines :: Lens' DownloadDBLogFilePortion (Maybe Int)
ddblfpNumberOfLines =
    lens _ddblfpNumberOfLines (\s a -> s { _ddblfpNumberOfLines = a })

data DownloadDBLogFilePortionResponse = DownloadDBLogFilePortionResponse
    { _ddblfprAdditionalDataPending :: Maybe Bool
    , _ddblfprLogFileData           :: Maybe Text
    , _ddblfprMarker                :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DownloadDBLogFilePortionResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddblfprAdditionalDataPending' @::@ 'Maybe' 'Bool'
--
-- * 'ddblfprLogFileData' @::@ 'Maybe' 'Text'
--
-- * 'ddblfprMarker' @::@ 'Maybe' 'Text'
--
downloadDBLogFilePortionResponse :: DownloadDBLogFilePortionResponse
downloadDBLogFilePortionResponse = DownloadDBLogFilePortionResponse
    { _ddblfprLogFileData           = Nothing
    , _ddblfprMarker                = Nothing
    , _ddblfprAdditionalDataPending = Nothing
    }

-- | Boolean value that if true, indicates there is more data to be
-- downloaded.
ddblfprAdditionalDataPending :: Lens' DownloadDBLogFilePortionResponse (Maybe Bool)
ddblfprAdditionalDataPending =
    lens _ddblfprAdditionalDataPending
        (\s a -> s { _ddblfprAdditionalDataPending = a })

-- | Entries from the specified log file.
ddblfprLogFileData :: Lens' DownloadDBLogFilePortionResponse (Maybe Text)
ddblfprLogFileData =
    lens _ddblfprLogFileData (\s a -> s { _ddblfprLogFileData = a })

-- | A pagination token that can be used in a subsequent
-- DownloadDBLogFilePortion request.
ddblfprMarker :: Lens' DownloadDBLogFilePortionResponse (Maybe Text)
ddblfprMarker = lens _ddblfprMarker (\s a -> s { _ddblfprMarker = a })

instance ToPath DownloadDBLogFilePortion where
    toPath = const "/"

instance ToQuery DownloadDBLogFilePortion

instance ToHeaders DownloadDBLogFilePortion

instance AWSRequest DownloadDBLogFilePortion where
    type Sv DownloadDBLogFilePortion = RDS
    type Rs DownloadDBLogFilePortion = DownloadDBLogFilePortionResponse

    request  = post "DownloadDBLogFilePortion"
    response = xmlResponse

instance FromXML DownloadDBLogFilePortionResponse where
    parseXML = withElement "DownloadDBLogFilePortionResult" $ \x ->
        DownloadDBLogFilePortionResponse
            <$> x .@? "AdditionalDataPending"
            <*> x .@? "LogFileData"
            <*> x .@? "Marker"

instance AWSPager DownloadDBLogFilePortion where
    next rq rs
        | not (more (rs ^. ddblfprAdditionalDataPending)) = Nothing
        | otherwise = Just $ rq
            & ddblfpMarker .~ rs ^. ddblfprMarker
