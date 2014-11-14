{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

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
module Network.AWS.RDS.DescribeDBLogFiles
    (
    -- * Request
      DescribeDBLogFiles
    -- ** Request constructor
    , describeDBLogFiles
    -- ** Request lenses
    , ddblfDBInstanceIdentifier
    , ddblfFileLastWritten
    , ddblfFileSize
    , ddblfFilenameContains
    , ddblfFilters
    , ddblfMarker
    , ddblfMaxRecords

    -- * Response
    , DescribeDBLogFilesResponse
    -- ** Response constructor
    , describeDBLogFilesResponse
    -- ** Response lenses
    , ddblfrDescribeDBLogFiles
    , ddblfrMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import qualified GHC.Exts

data DescribeDBLogFiles = DescribeDBLogFiles
    { _ddblfDBInstanceIdentifier :: Text
    , _ddblfFileLastWritten      :: Maybe Integer
    , _ddblfFileSize             :: Maybe Integer
    , _ddblfFilenameContains     :: Maybe Text
    , _ddblfFilters              :: [Filter]
    , _ddblfMarker               :: Maybe Text
    , _ddblfMaxRecords           :: Maybe Int
    } deriving (Eq, Show, Generic)

-- | 'DescribeDBLogFiles' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddblfDBInstanceIdentifier' @::@ 'Text'
--
-- * 'ddblfFileLastWritten' @::@ 'Maybe' 'Integer'
--
-- * 'ddblfFileSize' @::@ 'Maybe' 'Integer'
--
-- * 'ddblfFilenameContains' @::@ 'Maybe' 'Text'
--
-- * 'ddblfFilters' @::@ ['Filter']
--
-- * 'ddblfMarker' @::@ 'Maybe' 'Text'
--
-- * 'ddblfMaxRecords' @::@ 'Maybe' 'Int'
--
describeDBLogFiles :: Text -- ^ 'ddblfDBInstanceIdentifier'
                   -> DescribeDBLogFiles
describeDBLogFiles p1 = DescribeDBLogFiles
    { _ddblfDBInstanceIdentifier = p1
    , _ddblfFilenameContains     = Nothing
    , _ddblfFileLastWritten      = Nothing
    , _ddblfFileSize             = Nothing
    , _ddblfFilters              = mempty
    , _ddblfMaxRecords           = Nothing
    , _ddblfMarker               = Nothing
    }

-- | The customer-assigned name of the DB instance that contains the log files
-- you want to list. Constraints: Must contain from 1 to 63 alphanumeric
-- characters or hyphens First character must be a letter Cannot end with a
-- hyphen or contain two consecutive hyphens.
ddblfDBInstanceIdentifier :: Lens' DescribeDBLogFiles Text
ddblfDBInstanceIdentifier =
    lens _ddblfDBInstanceIdentifier
        (\s a -> s { _ddblfDBInstanceIdentifier = a })

-- | Filters the available log files for files written since the specified
-- date, in POSIX timestamp format.
ddblfFileLastWritten :: Lens' DescribeDBLogFiles (Maybe Integer)
ddblfFileLastWritten =
    lens _ddblfFileLastWritten (\s a -> s { _ddblfFileLastWritten = a })

-- | Filters the available log files for files larger than the specified size.
ddblfFileSize :: Lens' DescribeDBLogFiles (Maybe Integer)
ddblfFileSize = lens _ddblfFileSize (\s a -> s { _ddblfFileSize = a })

-- | Filters the available log files for log file names that contain the
-- specified string.
ddblfFilenameContains :: Lens' DescribeDBLogFiles (Maybe Text)
ddblfFilenameContains =
    lens _ddblfFilenameContains (\s a -> s { _ddblfFilenameContains = a })

-- | This parameter is not currently supported.
ddblfFilters :: Lens' DescribeDBLogFiles [Filter]
ddblfFilters = lens _ddblfFilters (\s a -> s { _ddblfFilters = a })

-- | The pagination token provided in the previous request. If this parameter
-- is specified the response includes only records beyond the marker, up to
-- MaxRecords.
ddblfMarker :: Lens' DescribeDBLogFiles (Maybe Text)
ddblfMarker = lens _ddblfMarker (\s a -> s { _ddblfMarker = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a pagination token called a
-- marker is included in the response so that the remaining results can be
-- retrieved.
ddblfMaxRecords :: Lens' DescribeDBLogFiles (Maybe Int)
ddblfMaxRecords = lens _ddblfMaxRecords (\s a -> s { _ddblfMaxRecords = a })

instance ToQuery DescribeDBLogFiles

instance ToPath DescribeDBLogFiles where
    toPath = const "/"

data DescribeDBLogFilesResponse = DescribeDBLogFilesResponse
    { _ddblfrDescribeDBLogFiles :: [DescribeDBLogFilesDetails]
    , _ddblfrMarker             :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'DescribeDBLogFilesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddblfrDescribeDBLogFiles' @::@ ['DescribeDBLogFilesDetails']
--
-- * 'ddblfrMarker' @::@ 'Maybe' 'Text'
--
describeDBLogFilesResponse :: DescribeDBLogFilesResponse
describeDBLogFilesResponse = DescribeDBLogFilesResponse
    { _ddblfrDescribeDBLogFiles = mempty
    , _ddblfrMarker             = Nothing
    }

-- | The DB log files returned.
ddblfrDescribeDBLogFiles :: Lens' DescribeDBLogFilesResponse [DescribeDBLogFilesDetails]
ddblfrDescribeDBLogFiles =
    lens _ddblfrDescribeDBLogFiles
        (\s a -> s { _ddblfrDescribeDBLogFiles = a })

-- | A pagination token that can be used in a subsequent DescribeDBLogFiles
-- request.
ddblfrMarker :: Lens' DescribeDBLogFilesResponse (Maybe Text)
ddblfrMarker = lens _ddblfrMarker (\s a -> s { _ddblfrMarker = a })

instance AWSRequest DescribeDBLogFiles where
    type Sv DescribeDBLogFiles = RDS
    type Rs DescribeDBLogFiles = DescribeDBLogFilesResponse

    request  = post "DescribeDBLogFiles"
    response = xmlResponse $ \h x -> DescribeDBLogFilesResponse
        <$> x %| "DescribeDBLogFiles"
        <*> x %| "Marker"
