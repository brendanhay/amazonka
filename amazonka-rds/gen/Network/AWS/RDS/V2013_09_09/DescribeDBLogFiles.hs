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
ddblfmDBInstanceIdentifier
    :: Functor f
    => (Text
    -> f (Text))
    -> DescribeDBLogFiles
    -> f DescribeDBLogFiles
ddblfmDBInstanceIdentifier f x =
    (\y -> x { _ddblfmDBInstanceIdentifier = y })
       <$> f (_ddblfmDBInstanceIdentifier x)
{-# INLINE ddblfmDBInstanceIdentifier #-}

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a pagination token called a
-- marker is included in the response so that the remaining results can be
-- retrieved.
ddblfmMaxRecords
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DescribeDBLogFiles
    -> f DescribeDBLogFiles
ddblfmMaxRecords f x =
    (\y -> x { _ddblfmMaxRecords = y })
       <$> f (_ddblfmMaxRecords x)
{-# INLINE ddblfmMaxRecords #-}

-- | Filters the available log files for files written since the specified date,
-- in POSIX timestamp format.
ddblfmFileLastWritten
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DescribeDBLogFiles
    -> f DescribeDBLogFiles
ddblfmFileLastWritten f x =
    (\y -> x { _ddblfmFileLastWritten = y })
       <$> f (_ddblfmFileLastWritten x)
{-# INLINE ddblfmFileLastWritten #-}

-- | Filters the available log files for files larger than the specified size.
ddblfmFileSize
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DescribeDBLogFiles
    -> f DescribeDBLogFiles
ddblfmFileSize f x =
    (\y -> x { _ddblfmFileSize = y })
       <$> f (_ddblfmFileSize x)
{-# INLINE ddblfmFileSize #-}

-- | Filters the available log files for log file names that contain the
-- specified string.
ddblfmFilenameContains
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeDBLogFiles
    -> f DescribeDBLogFiles
ddblfmFilenameContains f x =
    (\y -> x { _ddblfmFilenameContains = y })
       <$> f (_ddblfmFilenameContains x)
{-# INLINE ddblfmFilenameContains #-}

-- | The pagination token provided in the previous request. If this parameter is
-- specified the response includes only records beyond the marker, up to
-- MaxRecords.
ddblfmMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeDBLogFiles
    -> f DescribeDBLogFiles
ddblfmMarker f x =
    (\y -> x { _ddblfmMarker = y })
       <$> f (_ddblfmMarker x)
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
ddblfrDescribeDBLogFiles
    :: Functor f
    => ([DescribeDBLogFilesDetails]
    -> f ([DescribeDBLogFilesDetails]))
    -> DescribeDBLogFilesResponse
    -> f DescribeDBLogFilesResponse
ddblfrDescribeDBLogFiles f x =
    (\y -> x { _ddblfrDescribeDBLogFiles = y })
       <$> f (_ddblfrDescribeDBLogFiles x)
{-# INLINE ddblfrDescribeDBLogFiles #-}

-- | An optional paging token.
ddblfrMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeDBLogFilesResponse
    -> f DescribeDBLogFilesResponse
ddblfrMarker f x =
    (\y -> x { _ddblfrMarker = y })
       <$> f (_ddblfrMarker x)
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
