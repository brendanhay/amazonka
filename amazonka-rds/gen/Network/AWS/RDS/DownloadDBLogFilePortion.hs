{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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

-- | Downloads the last line of the specified log file.
-- https://rds.amazonaws.com/ ?DBInstanceIdentifier=rra-mysql &MaxRecords=100
-- &Version=2013-05-15 &Action=DescribeDBLogFiles &SignatureVersion=4
-- &SignatureMethod=HmacSHA256 &Timestamp=20130327T173621Z
-- &X-Amz-Algorithm=AWS4-HMAC-SHA256 &X-Amz-Date=20130327T173621Z
-- &X-Amz-SignedHeaders=Host &X-Amz-Expires=20130327T173621Z
-- &X-Amz-Credential= &X-Amz-Signature= 1364403600000
-- error/mysql-error-running.log 0 1364338800000
-- error/mysql-error-running.log.0 0 1364342400000
-- error/mysql-error-running.log.1 0 1364371200000
-- error/mysql-error-running.log.9 0 1364405700000 error/mysql-error.log 0
-- d70fb3b3-9704-11e2-a0db-871552e0ef19.
module Network.AWS.RDS.DownloadDBLogFilePortion
    (
    -- * Request
      DownloadDBLogFilePortion
    -- ** Request constructor
    , mkDownloadDBLogFilePortion
    -- ** Request lenses
    , ddblfpDBInstanceIdentifier
    , ddblfpLogFileName
    , ddblfpMarker
    , ddblfpNumberOfLines

    -- * Response
    , DownloadDBLogFilePortionResponse
    -- ** Response constructor
    , mkDownloadDBLogFilePortionResponse
    -- ** Response lenses
    , ddblfprAdditionalDataPending
    , ddblfprLogFileData
    , ddblfprMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import Network.AWS.Prelude

-- | 
data DownloadDBLogFilePortion = DownloadDBLogFilePortion
    { _ddblfpDBInstanceIdentifier :: !Text
    , _ddblfpLogFileName :: !Text
    , _ddblfpMarker :: !(Maybe Text)
    , _ddblfpNumberOfLines :: !(Maybe Integer)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DownloadDBLogFilePortion' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DBInstanceIdentifier ::@ @Text@
--
-- * @LogFileName ::@ @Text@
--
-- * @Marker ::@ @Maybe Text@
--
-- * @NumberOfLines ::@ @Maybe Integer@
--
mkDownloadDBLogFilePortion :: Text -- ^ 'ddblfpDBInstanceIdentifier'
                           -> Text -- ^ 'ddblfpLogFileName'
                           -> DownloadDBLogFilePortion
mkDownloadDBLogFilePortion p1 p2 = DownloadDBLogFilePortion
    { _ddblfpDBInstanceIdentifier = p1
    , _ddblfpLogFileName = p2
    , _ddblfpMarker = Nothing
    , _ddblfpNumberOfLines = Nothing
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

-- | The pagination token provided in the previous request. If this parameter is
-- specified the response includes only records beyond the marker, up to
-- MaxRecords.
ddblfpMarker :: Lens' DownloadDBLogFilePortion (Maybe Text)
ddblfpMarker = lens _ddblfpMarker (\s a -> s { _ddblfpMarker = a })

-- | The number of lines remaining to be downloaded.
ddblfpNumberOfLines :: Lens' DownloadDBLogFilePortion (Maybe Integer)
ddblfpNumberOfLines =
    lens _ddblfpNumberOfLines (\s a -> s { _ddblfpNumberOfLines = a })

instance ToQuery DownloadDBLogFilePortion where
    toQuery = genericQuery def

-- | This data type is used as a response element to DownloadDBLogFilePortion.
data DownloadDBLogFilePortionResponse = DownloadDBLogFilePortionResponse
    { _ddblfprAdditionalDataPending :: !Bool
    , _ddblfprLogFileData :: !ByteString
    , _ddblfprMarker :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DownloadDBLogFilePortionResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AdditionalDataPending ::@ @Bool@
--
-- * @LogFileData ::@ @ByteString@
--
-- * @Marker ::@ @Maybe Text@
--
mkDownloadDBLogFilePortionResponse :: Bool -- ^ 'ddblfprAdditionalDataPending'
                                   -> ByteString -- ^ 'ddblfprLogFileData'
                                   -> DownloadDBLogFilePortionResponse
mkDownloadDBLogFilePortionResponse p1 p2 = DownloadDBLogFilePortionResponse
    { _ddblfprAdditionalDataPending = p1
    , _ddblfprLogFileData = p2
    , _ddblfprMarker = Nothing
    }

-- | Boolean value that if true, indicates there is more data to be downloaded.
ddblfprAdditionalDataPending :: Lens' DownloadDBLogFilePortionResponse Bool
ddblfprAdditionalDataPending =
    lens _ddblfprAdditionalDataPending
         (\s a -> s { _ddblfprAdditionalDataPending = a })

-- | Entries from the specified log file.
ddblfprLogFileData :: Lens' DownloadDBLogFilePortionResponse ByteString
ddblfprLogFileData =
    lens _ddblfprLogFileData (\s a -> s { _ddblfprLogFileData = a })

-- | An optional pagination token provided by a previous
-- DownloadDBLogFilePortion request.
ddblfprMarker :: Lens' DownloadDBLogFilePortionResponse (Maybe Text)
ddblfprMarker = lens _ddblfprMarker (\s a -> s { _ddblfprMarker = a })

instance FromXML DownloadDBLogFilePortionResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DownloadDBLogFilePortion where
    type Sv DownloadDBLogFilePortion = RDS
    type Rs DownloadDBLogFilePortion = DownloadDBLogFilePortionResponse

    request = post "DownloadDBLogFilePortion"
    response _ = xmlResponse

instance AWSPager DownloadDBLogFilePortion where
    next rq rs
        | not (rs ^. ddblfprAdditionalDataPending) = Nothing
        | otherwise = Just $
            rq & ddblfpMarker .~ rs ^. ddblfprMarker
