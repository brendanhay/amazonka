{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.DescribeDBLogFiles
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
module Network.AWS.RDS.DescribeDBLogFiles
    (
    -- * Request
      DescribeDBLogFiles
    -- ** Request constructor
    , describeDBLogFiles
    -- ** Request lenses
    , ddblfDBInstanceIdentifier
    , ddblfFilenameContains
    , ddblfFileLastWritten
    , ddblfFileSize
    , ddblfMaxRecords
    , ddblfMarker

    -- * Response
    , DescribeDBLogFilesResponse
    -- ** Response constructor
    , describeDBLogFilesResponse
    -- ** Response lenses
    , ddblfrDescribeDBLogFiles
    , ddblfrMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import Network.AWS.Prelude

-- | 
data DescribeDBLogFiles = DescribeDBLogFiles
    { _ddblfDBInstanceIdentifier :: Text
    , _ddblfFilenameContains :: Maybe Text
    , _ddblfFileLastWritten :: Maybe Integer
    , _ddblfFileSize :: Maybe Integer
    , _ddblfMaxRecords :: Maybe Integer
    , _ddblfMarker :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeDBLogFiles' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DBInstanceIdentifier ::@ @Text@
--
-- * @FilenameContains ::@ @Maybe Text@
--
-- * @FileLastWritten ::@ @Maybe Integer@
--
-- * @FileSize ::@ @Maybe Integer@
--
-- * @MaxRecords ::@ @Maybe Integer@
--
-- * @Marker ::@ @Maybe Text@
--
describeDBLogFiles :: Text -- ^ 'ddblfDBInstanceIdentifier'
                     -> DescribeDBLogFiles
describeDBLogFiles p1 = DescribeDBLogFiles
    { _ddblfDBInstanceIdentifier = p1
    , _ddblfFilenameContains = Nothing
    , _ddblfFileLastWritten = Nothing
    , _ddblfFileSize = Nothing
    , _ddblfMaxRecords = Nothing
    , _ddblfMarker = Nothing
    }

-- | The customer-assigned name of the DB instance that contains the log files
-- you want to list. Constraints: Must contain from 1 to 63 alphanumeric
-- characters or hyphens First character must be a letter Cannot end with a
-- hyphen or contain two consecutive hyphens.
ddblfDBInstanceIdentifier :: Lens' DescribeDBLogFiles Text
ddblfDBInstanceIdentifier =
    lens _ddblfDBInstanceIdentifier
         (\s a -> s { _ddblfDBInstanceIdentifier = a })

-- | Filters the available log files for log file names that contain the
-- specified string.
ddblfFilenameContains :: Lens' DescribeDBLogFiles (Maybe Text)
ddblfFilenameContains =
    lens _ddblfFilenameContains (\s a -> s { _ddblfFilenameContains = a })

-- | Filters the available log files for files written since the specified date,
-- in POSIX timestamp format.
ddblfFileLastWritten :: Lens' DescribeDBLogFiles (Maybe Integer)
ddblfFileLastWritten =
    lens _ddblfFileLastWritten (\s a -> s { _ddblfFileLastWritten = a })

-- | Filters the available log files for files larger than the specified size.
ddblfFileSize :: Lens' DescribeDBLogFiles (Maybe Integer)
ddblfFileSize = lens _ddblfFileSize (\s a -> s { _ddblfFileSize = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a pagination token called a
-- marker is included in the response so that the remaining results can be
-- retrieved.
ddblfMaxRecords :: Lens' DescribeDBLogFiles (Maybe Integer)
ddblfMaxRecords = lens _ddblfMaxRecords (\s a -> s { _ddblfMaxRecords = a })

-- | The pagination token provided in the previous request. If this parameter is
-- specified the response includes only records beyond the marker, up to
-- MaxRecords.
ddblfMarker :: Lens' DescribeDBLogFiles (Maybe Text)
ddblfMarker = lens _ddblfMarker (\s a -> s { _ddblfMarker = a })

instance ToQuery DescribeDBLogFiles where
    toQuery = genericQuery def

-- | The response from a call to DescribeDBLogFiles.
data DescribeDBLogFilesResponse = DescribeDBLogFilesResponse
    { _ddblfrDescribeDBLogFiles :: [DescribeDBLogFilesDetails]
    , _ddblfrMarker :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeDBLogFilesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DescribeDBLogFiles ::@ @[DescribeDBLogFilesDetails]@
--
-- * @Marker ::@ @Maybe Text@
--
describeDBLogFilesResponse :: DescribeDBLogFilesResponse
describeDBLogFilesResponse = DescribeDBLogFilesResponse
    { _ddblfrDescribeDBLogFiles = mempty
    , _ddblfrMarker = Nothing
    }

-- | The DB log files returned.
ddblfrDescribeDBLogFiles :: Lens' DescribeDBLogFilesResponse [DescribeDBLogFilesDetails]
ddblfrDescribeDBLogFiles =
    lens _ddblfrDescribeDBLogFiles
         (\s a -> s { _ddblfrDescribeDBLogFiles = a })

-- | An optional paging token.
ddblfrMarker :: Lens' DescribeDBLogFilesResponse (Maybe Text)
ddblfrMarker = lens _ddblfrMarker (\s a -> s { _ddblfrMarker = a })

instance FromXML DescribeDBLogFilesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeDBLogFiles where
    type Sv DescribeDBLogFiles = RDS
    type Rs DescribeDBLogFiles = DescribeDBLogFilesResponse

    request = post "DescribeDBLogFiles"
    response _ = xmlResponse

instance AWSPager DescribeDBLogFiles where
    next rq rs = (\x -> rq & ddblfMarker ?~ x)
        <$> (rs ^. ddblfrMarker)
