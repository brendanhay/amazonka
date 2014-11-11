{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
module Network.AWS.RDS.DownloadDBLogFilePortion
    (
    -- * Request
      DownloadDBLogFilePortionMessage
    -- ** Request constructor
    , downloadDBLogFilePortionMessage
    -- ** Request lenses
    , ddblfpmDBInstanceIdentifier
    , ddblfpmLogFileName
    , ddblfpmMarker
    , ddblfpmNumberOfLines

    -- * Response
    , DownloadDBLogFilePortionDetails
    -- ** Response constructor
    , downloadDBLogFilePortionDetails
    -- ** Response lenses
    , ddblfpdAdditionalDataPending
    , ddblfpdLogFileData
    , ddblfpdMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types

data DownloadDBLogFilePortionMessage = DownloadDBLogFilePortionMessage
    { _ddblfpmDBInstanceIdentifier :: Text
    , _ddblfpmLogFileName          :: Text
    , _ddblfpmMarker               :: Maybe Text
    , _ddblfpmNumberOfLines        :: Maybe Int
    } deriving (Eq, Ord, Show, Generic)

-- | 'DownloadDBLogFilePortionMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddblfpmDBInstanceIdentifier' @::@ 'Text'
--
-- * 'ddblfpmLogFileName' @::@ 'Text'
--
-- * 'ddblfpmMarker' @::@ 'Maybe' 'Text'
--
-- * 'ddblfpmNumberOfLines' @::@ 'Maybe' 'Int'
--
downloadDBLogFilePortionMessage :: Text -- ^ 'ddblfpmDBInstanceIdentifier'
                                -> Text -- ^ 'ddblfpmLogFileName'
                                -> DownloadDBLogFilePortionMessage
downloadDBLogFilePortionMessage p1 p2 = DownloadDBLogFilePortionMessage
    { _ddblfpmDBInstanceIdentifier = p1
    , _ddblfpmLogFileName          = p2
    , _ddblfpmMarker               = Nothing
    , _ddblfpmNumberOfLines        = Nothing
    }

-- | The customer-assigned name of the DB instance that contains the log files
-- you want to list. Constraints: Must contain from 1 to 63 alphanumeric
-- characters or hyphens First character must be a letter Cannot end with a
-- hyphen or contain two consecutive hyphens.
ddblfpmDBInstanceIdentifier :: Lens' DownloadDBLogFilePortionMessage Text
ddblfpmDBInstanceIdentifier =
    lens _ddblfpmDBInstanceIdentifier
        (\s a -> s { _ddblfpmDBInstanceIdentifier = a })

-- | The name of the log file to be downloaded.
ddblfpmLogFileName :: Lens' DownloadDBLogFilePortionMessage Text
ddblfpmLogFileName =
    lens _ddblfpmLogFileName (\s a -> s { _ddblfpmLogFileName = a })

-- | The pagination token provided in the previous request or "0". If the
-- Marker parameter is specified the response includes only records beyond
-- the marker until the end of the file or up to NumberOfLines.
ddblfpmMarker :: Lens' DownloadDBLogFilePortionMessage (Maybe Text)
ddblfpmMarker = lens _ddblfpmMarker (\s a -> s { _ddblfpmMarker = a })

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
ddblfpmNumberOfLines :: Lens' DownloadDBLogFilePortionMessage (Maybe Int)
ddblfpmNumberOfLines =
    lens _ddblfpmNumberOfLines (\s a -> s { _ddblfpmNumberOfLines = a })
instance ToQuery DownloadDBLogFilePortionMessage

instance ToPath DownloadDBLogFilePortionMessage where
    toPath = const "/"

data DownloadDBLogFilePortionDetails = DownloadDBLogFilePortionDetails
    { _ddblfpdAdditionalDataPending :: Maybe Bool
    , _ddblfpdLogFileData           :: Maybe Text
    , _ddblfpdMarker                :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DownloadDBLogFilePortionDetails' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddblfpdAdditionalDataPending' @::@ 'Maybe' 'Bool'
--
-- * 'ddblfpdLogFileData' @::@ 'Maybe' 'Text'
--
-- * 'ddblfpdMarker' @::@ 'Maybe' 'Text'
--
downloadDBLogFilePortionDetails :: DownloadDBLogFilePortionDetails
downloadDBLogFilePortionDetails = DownloadDBLogFilePortionDetails
    { _ddblfpdLogFileData           = Nothing
    , _ddblfpdMarker                = Nothing
    , _ddblfpdAdditionalDataPending = Nothing
    }

-- | Boolean value that if true, indicates there is more data to be
-- downloaded.
ddblfpdAdditionalDataPending :: Lens' DownloadDBLogFilePortionDetails (Maybe Bool)
ddblfpdAdditionalDataPending =
    lens _ddblfpdAdditionalDataPending
        (\s a -> s { _ddblfpdAdditionalDataPending = a })

-- | Entries from the specified log file.
ddblfpdLogFileData :: Lens' DownloadDBLogFilePortionDetails (Maybe Text)
ddblfpdLogFileData =
    lens _ddblfpdLogFileData (\s a -> s { _ddblfpdLogFileData = a })

-- | A pagination token that can be used in a subsequent
-- DownloadDBLogFilePortion request.
ddblfpdMarker :: Lens' DownloadDBLogFilePortionDetails (Maybe Text)
ddblfpdMarker = lens _ddblfpdMarker (\s a -> s { _ddblfpdMarker = a })
instance FromXML DownloadDBLogFilePortionDetails where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DownloadDBLogFilePortionDetails"

instance AWSRequest DownloadDBLogFilePortionMessage where
    type Sv DownloadDBLogFilePortionMessage = RDS
    type Rs DownloadDBLogFilePortionMessage = DownloadDBLogFilePortionDetails

    request  = post "DownloadDBLogFilePortion"
    response = xmlResponse $ \h x -> DownloadDBLogFilePortionDetails
        <$> x %| "AdditionalDataPending"
        <*> x %| "LogFileData"
        <*> x %| "Marker"
