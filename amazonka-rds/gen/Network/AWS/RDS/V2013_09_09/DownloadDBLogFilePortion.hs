{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.V2013_09_09.DownloadDBLogFilePortion
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
module Network.AWS.RDS.V2013_09_09.DownloadDBLogFilePortion
    (
    -- * Request
      DownloadDBLogFilePortion
    -- ** Request constructor
    , mkDownloadDBLogFilePortionMessage
    -- ** Request lenses
    , ddblfpmDBInstanceIdentifier
    , ddblfpmLogFileName
    , ddblfpmMarker
    , ddblfpmNumberOfLines

    -- * Response
    , DownloadDBLogFilePortionResponse
    -- ** Response lenses
    , ddblfpdAdditionalDataPending
    , ddblfpdLogFileData
    , ddblfpdMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DownloadDBLogFilePortion' request.
mkDownloadDBLogFilePortionMessage :: Text -- ^ 'ddblfpmDBInstanceIdentifier'
                                  -> Text -- ^ 'ddblfpmLogFileName'
                                  -> DownloadDBLogFilePortion
mkDownloadDBLogFilePortionMessage p1 p2 = DownloadDBLogFilePortion
    { _ddblfpmDBInstanceIdentifier = p1
    , _ddblfpmLogFileName = p2
    , _ddblfpmMarker = Nothing
    , _ddblfpmNumberOfLines = Nothing
    }
{-# INLINE mkDownloadDBLogFilePortionMessage #-}

data DownloadDBLogFilePortion = DownloadDBLogFilePortion
    { _ddblfpmDBInstanceIdentifier :: Text
      -- ^ The customer-assigned name of the DB instance that contains the
      -- log files you want to list. Constraints: Must contain from 1 to
      -- 63 alphanumeric characters or hyphens First character must be a
      -- letter Cannot end with a hyphen or contain two consecutive
      -- hyphens.
    , _ddblfpmLogFileName :: Text
      -- ^ The name of the log file to be downloaded.
    , _ddblfpmMarker :: Maybe Text
      -- ^ The pagination token provided in the previous request. If this
      -- parameter is specified the response includes only records beyond
      -- the marker, up to MaxRecords.
    , _ddblfpmNumberOfLines :: Maybe Integer
      -- ^ The number of lines remaining to be downloaded.
    } deriving (Show, Generic)

-- | The customer-assigned name of the DB instance that contains the log files
-- you want to list. Constraints: Must contain from 1 to 63 alphanumeric
-- characters or hyphens First character must be a letter Cannot end with a
-- hyphen or contain two consecutive hyphens.
ddblfpmDBInstanceIdentifier :: Lens' DownloadDBLogFilePortion (Text)
ddblfpmDBInstanceIdentifier = lens _ddblfpmDBInstanceIdentifier (\s a -> s { _ddblfpmDBInstanceIdentifier = a })
{-# INLINE ddblfpmDBInstanceIdentifier #-}

-- | The name of the log file to be downloaded.
ddblfpmLogFileName :: Lens' DownloadDBLogFilePortion (Text)
ddblfpmLogFileName = lens _ddblfpmLogFileName (\s a -> s { _ddblfpmLogFileName = a })
{-# INLINE ddblfpmLogFileName #-}

-- | The pagination token provided in the previous request. If this parameter is
-- specified the response includes only records beyond the marker, up to
-- MaxRecords.
ddblfpmMarker :: Lens' DownloadDBLogFilePortion (Maybe Text)
ddblfpmMarker = lens _ddblfpmMarker (\s a -> s { _ddblfpmMarker = a })
{-# INLINE ddblfpmMarker #-}

-- | The number of lines remaining to be downloaded.
ddblfpmNumberOfLines :: Lens' DownloadDBLogFilePortion (Maybe Integer)
ddblfpmNumberOfLines = lens _ddblfpmNumberOfLines (\s a -> s { _ddblfpmNumberOfLines = a })
{-# INLINE ddblfpmNumberOfLines #-}

instance ToQuery DownloadDBLogFilePortion where
    toQuery = genericQuery def

data DownloadDBLogFilePortionResponse = DownloadDBLogFilePortionResponse
    { _ddblfpdAdditionalDataPending :: Bool
      -- ^ Boolean value that if true, indicates there is more data to be
      -- downloaded.
    , _ddblfpdLogFileData :: ByteString
      -- ^ Entries from the specified log file.
    , _ddblfpdMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous
      -- DownloadDBLogFilePortion request.
    } deriving (Show, Generic)

-- | Boolean value that if true, indicates there is more data to be downloaded.
ddblfpdAdditionalDataPending :: Lens' DownloadDBLogFilePortionResponse (Bool)
ddblfpdAdditionalDataPending = lens _ddblfpdAdditionalDataPending (\s a -> s { _ddblfpdAdditionalDataPending = a })
{-# INLINE ddblfpdAdditionalDataPending #-}

-- | Entries from the specified log file.
ddblfpdLogFileData :: Lens' DownloadDBLogFilePortionResponse (ByteString)
ddblfpdLogFileData = lens _ddblfpdLogFileData (\s a -> s { _ddblfpdLogFileData = a })
{-# INLINE ddblfpdLogFileData #-}

-- | An optional pagination token provided by a previous
-- DownloadDBLogFilePortion request.
ddblfpdMarker :: Lens' DownloadDBLogFilePortionResponse (Maybe Text)
ddblfpdMarker = lens _ddblfpdMarker (\s a -> s { _ddblfpdMarker = a })
{-# INLINE ddblfpdMarker #-}

instance FromXML DownloadDBLogFilePortionResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DownloadDBLogFilePortion where
    type Sv DownloadDBLogFilePortion = RDS
    type Rs DownloadDBLogFilePortion = DownloadDBLogFilePortionResponse

    request = post "DownloadDBLogFilePortion"
    response _ = xmlResponse

instance AWSPager DownloadDBLogFilePortion where
    next rq rs
        | not (_ddblfpdAdditionalDataPending rs) = Nothing
        | otherwise = Just $ rq
            { _ddblfpmMarker = _ddblfpdMarker rs
            }
