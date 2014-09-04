{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.V2013_09_09.DescribeDBLogFiles
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of DB log files for the DB instance.
-- https://rds.amazonaws.com/ ?DBInstanceIdentifier=rrak-mysql &MaxRecords=100
-- &Version=2013-02-12 &Action=DescribeDBLogFiles &SignatureVersion=4
-- &SignatureMethod=HmacSHA256 &Timestamp=20130327T173621Z
-- &X-Amz-Algorithm=AWS4-HMAC-SHA256 &X-Amz-Date=20130327T173621Z
-- &X-Amz-SignedHeaders=Host &X-Amz-Expires=20130327T173621Z
-- &X-Amz-Credential= &X-Amz-Signature= 1364403600000
-- error/mysql-error-running.log 0 1364338800000
-- error/mysql-error-running.log.0 0 1364342400000
-- error/mysql-error-running.log.1 0 1364346000000
-- error/mysql-error-running.log.2 0 1364349600000
-- error/mysql-error-running.log.3 0 1364405700000 error/mysql-error.log 0
-- d70fb3b3-9704-11e2-a0db-871552e0ef19.
module Network.AWS.RDS.V2013_09_09.DescribeDBLogFiles
    (
    -- * Request
      DescribeDBLogFiles
    -- ** Request constructor
    , describeDBLogFiles
    -- ** Request lenses
    , ddblfmDBInstanceIdentifier
    , ddblfmMaxRecords
    , ddblfmFileLastWritten
    , ddblfmFileSize
    , ddblfmFilenameContains
    , ddblfmMarker

    -- * Response
    , DescribeDBLogFilesResponse
    -- ** Response lenses
    , ddblfrDescribeDBLogFiles
    , ddblfrMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeDBLogFiles' request.
describeDBLogFiles :: Text -- ^ 'ddblfmDBInstanceIdentifier'
                   -> DescribeDBLogFiles
describeDBLogFiles p1 = DescribeDBLogFiles
    { _ddblfmDBInstanceIdentifier = p1
    , _ddblfmMaxRecords = Nothing
    , _ddblfmFileLastWritten = Nothing
    , _ddblfmFileSize = Nothing
    , _ddblfmFilenameContains = Nothing
    , _ddblfmMarker = Nothing
    }
{-# INLINE describeDBLogFiles #-}

data DescribeDBLogFiles = DescribeDBLogFiles
    { _ddblfmDBInstanceIdentifier :: Text
      -- ^ The customer-assigned name of the DB instance that contains the
      -- log files you want to list. Constraints: Must contain from 1 to
      -- 63 alphanumeric characters or hyphens First character must be a
      -- letter Cannot end with a hyphen or contain two consecutive
      -- hyphens.
    , _ddblfmMaxRecords :: Maybe Integer
      -- ^ The maximum number of records to include in the response. If more
      -- records exist than the specified MaxRecords value, a pagination
      -- token called a marker is included in the response so that the
      -- remaining results can be retrieved.
    , _ddblfmFileLastWritten :: Maybe Integer
      -- ^ Filters the available log files for files written since the
      -- specified date, in POSIX timestamp format.
    , _ddblfmFileSize :: Maybe Integer
      -- ^ Filters the available log files for files larger than the
      -- specified size.
    , _ddblfmFilenameContains :: Maybe Text
      -- ^ Filters the available log files for log file names that contain
      -- the specified string.
    , _ddblfmMarker :: Maybe Text
      -- ^ The pagination token provided in the previous request. If this
      -- parameter is specified the response includes only records beyond
      -- the marker, up to MaxRecords.
    } deriving (Show, Generic)

-- | The customer-assigned name of the DB instance that contains the log files
-- you want to list. Constraints: Must contain from 1 to 63 alphanumeric
-- characters or hyphens First character must be a letter Cannot end with a
-- hyphen or contain two consecutive hyphens.
ddblfmDBInstanceIdentifier :: Lens' DescribeDBLogFiles (Text)
ddblfmDBInstanceIdentifier f x =
    f (_ddblfmDBInstanceIdentifier x)
        <&> \y -> x { _ddblfmDBInstanceIdentifier = y }
{-# INLINE ddblfmDBInstanceIdentifier #-}

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a pagination token called a
-- marker is included in the response so that the remaining results can be
-- retrieved.
ddblfmMaxRecords :: Lens' DescribeDBLogFiles (Maybe Integer)
ddblfmMaxRecords f x =
    f (_ddblfmMaxRecords x)
        <&> \y -> x { _ddblfmMaxRecords = y }
{-# INLINE ddblfmMaxRecords #-}

-- | Filters the available log files for files written since the specified date,
-- in POSIX timestamp format.
ddblfmFileLastWritten :: Lens' DescribeDBLogFiles (Maybe Integer)
ddblfmFileLastWritten f x =
    f (_ddblfmFileLastWritten x)
        <&> \y -> x { _ddblfmFileLastWritten = y }
{-# INLINE ddblfmFileLastWritten #-}

-- | Filters the available log files for files larger than the specified size.
ddblfmFileSize :: Lens' DescribeDBLogFiles (Maybe Integer)
ddblfmFileSize f x =
    f (_ddblfmFileSize x)
        <&> \y -> x { _ddblfmFileSize = y }
{-# INLINE ddblfmFileSize #-}

-- | Filters the available log files for log file names that contain the
-- specified string.
ddblfmFilenameContains :: Lens' DescribeDBLogFiles (Maybe Text)
ddblfmFilenameContains f x =
    f (_ddblfmFilenameContains x)
        <&> \y -> x { _ddblfmFilenameContains = y }
{-# INLINE ddblfmFilenameContains #-}

-- | The pagination token provided in the previous request. If this parameter is
-- specified the response includes only records beyond the marker, up to
-- MaxRecords.
ddblfmMarker :: Lens' DescribeDBLogFiles (Maybe Text)
ddblfmMarker f x =
    f (_ddblfmMarker x)
        <&> \y -> x { _ddblfmMarker = y }
{-# INLINE ddblfmMarker #-}

instance ToQuery DescribeDBLogFiles where
    toQuery = genericQuery def

data DescribeDBLogFilesResponse = DescribeDBLogFilesResponse
    { _ddblfrDescribeDBLogFiles :: [DescribeDBLogFilesDetails]
      -- ^ The DB log files returned.
    , _ddblfrMarker :: Maybe Text
      -- ^ An optional paging token.
    } deriving (Show, Generic)

-- | The DB log files returned.
ddblfrDescribeDBLogFiles :: Lens' DescribeDBLogFilesResponse ([DescribeDBLogFilesDetails])
ddblfrDescribeDBLogFiles f x =
    f (_ddblfrDescribeDBLogFiles x)
        <&> \y -> x { _ddblfrDescribeDBLogFiles = y }
{-# INLINE ddblfrDescribeDBLogFiles #-}

-- | An optional paging token.
ddblfrMarker :: Lens' DescribeDBLogFilesResponse (Maybe Text)
ddblfrMarker f x =
    f (_ddblfrMarker x)
        <&> \y -> x { _ddblfrMarker = y }
{-# INLINE ddblfrMarker #-}

instance FromXML DescribeDBLogFilesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeDBLogFiles where
    type Sv DescribeDBLogFiles = RDS
    type Rs DescribeDBLogFiles = DescribeDBLogFilesResponse

    request = post "DescribeDBLogFiles"
    response _ = xmlResponse

instance AWSPager DescribeDBLogFiles where
    next rq rs = (\x -> rq { _ddblfmMarker = Just x })
        <$> (_ddblfrMarker rs)
