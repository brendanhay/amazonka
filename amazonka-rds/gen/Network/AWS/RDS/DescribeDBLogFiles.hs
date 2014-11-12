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
      DescribeDBLogFilesMessage
    -- ** Request constructor
    , describeDBLogFilesMessage
    -- ** Request lenses
    , ddblfmDBInstanceIdentifier
    , ddblfmFileLastWritten
    , ddblfmFileSize
    , ddblfmFilenameContains
    , ddblfmFilters
    , ddblfmMarker
    , ddblfmMaxRecords

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

data DescribeDBLogFilesMessage = DescribeDBLogFilesMessage
    { _ddblfmDBInstanceIdentifier :: Text
    , _ddblfmFileLastWritten      :: Maybe Integer
    , _ddblfmFileSize             :: Maybe Integer
    , _ddblfmFilenameContains     :: Maybe Text
    , _ddblfmFilters              :: [Filter]
    , _ddblfmMarker               :: Maybe Text
    , _ddblfmMaxRecords           :: Maybe Int
    } (Eq, Show, Generic)

-- | 'DescribeDBLogFilesMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddblfmDBInstanceIdentifier' @::@ 'Text'
--
-- * 'ddblfmFileLastWritten' @::@ 'Maybe' 'Integer'
--
-- * 'ddblfmFileSize' @::@ 'Maybe' 'Integer'
--
-- * 'ddblfmFilenameContains' @::@ 'Maybe' 'Text'
--
-- * 'ddblfmFilters' @::@ ['Filter']
--
-- * 'ddblfmMarker' @::@ 'Maybe' 'Text'
--
-- * 'ddblfmMaxRecords' @::@ 'Maybe' 'Int'
--
describeDBLogFilesMessage :: Text -- ^ 'ddblfmDBInstanceIdentifier'
                          -> DescribeDBLogFilesMessage
describeDBLogFilesMessage p1 = DescribeDBLogFilesMessage
    { _ddblfmDBInstanceIdentifier = p1
    , _ddblfmFilenameContains     = Nothing
    , _ddblfmFileLastWritten      = Nothing
    , _ddblfmFileSize             = Nothing
    , _ddblfmFilters              = mempty
    , _ddblfmMaxRecords           = Nothing
    , _ddblfmMarker               = Nothing
    }

-- | The customer-assigned name of the DB instance that contains the log files
-- you want to list. Constraints: Must contain from 1 to 63 alphanumeric
-- characters or hyphens First character must be a letter Cannot end with a
-- hyphen or contain two consecutive hyphens.
ddblfmDBInstanceIdentifier :: Lens' DescribeDBLogFilesMessage Text
ddblfmDBInstanceIdentifier =
    lens _ddblfmDBInstanceIdentifier
        (\s a -> s { _ddblfmDBInstanceIdentifier = a })

-- | Filters the available log files for files written since the specified
-- date, in POSIX timestamp format.
ddblfmFileLastWritten :: Lens' DescribeDBLogFilesMessage (Maybe Integer)
ddblfmFileLastWritten =
    lens _ddblfmFileLastWritten (\s a -> s { _ddblfmFileLastWritten = a })

-- | Filters the available log files for files larger than the specified size.
ddblfmFileSize :: Lens' DescribeDBLogFilesMessage (Maybe Integer)
ddblfmFileSize = lens _ddblfmFileSize (\s a -> s { _ddblfmFileSize = a })

-- | Filters the available log files for log file names that contain the
-- specified string.
ddblfmFilenameContains :: Lens' DescribeDBLogFilesMessage (Maybe Text)
ddblfmFilenameContains =
    lens _ddblfmFilenameContains (\s a -> s { _ddblfmFilenameContains = a })

-- | This parameter is not currently supported.
ddblfmFilters :: Lens' DescribeDBLogFilesMessage [Filter]
ddblfmFilters = lens _ddblfmFilters (\s a -> s { _ddblfmFilters = a })

-- | The pagination token provided in the previous request. If this parameter
-- is specified the response includes only records beyond the marker, up to
-- MaxRecords.
ddblfmMarker :: Lens' DescribeDBLogFilesMessage (Maybe Text)
ddblfmMarker = lens _ddblfmMarker (\s a -> s { _ddblfmMarker = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a pagination token called a
-- marker is included in the response so that the remaining results can be
-- retrieved.
ddblfmMaxRecords :: Lens' DescribeDBLogFilesMessage (Maybe Int)
ddblfmMaxRecords = lens _ddblfmMaxRecords (\s a -> s { _ddblfmMaxRecords = a })
instance ToQuery DescribeDBLogFilesMessage

instance ToPath DescribeDBLogFilesMessage where
    toPath = const "/"

data DescribeDBLogFilesResponse = DescribeDBLogFilesResponse
    { _ddblfrDescribeDBLogFiles :: [DescribeDBLogFilesDetails]
    , _ddblfrMarker             :: Maybe Text
    } (Eq, Show, Generic)

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

instance FromXML DescribeDBLogFilesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DescribeDBLogFilesResponse"

instance AWSRequest DescribeDBLogFilesMessage where
    type Sv DescribeDBLogFilesMessage = RDS
    type Rs DescribeDBLogFilesMessage = DescribeDBLogFilesResponse

    request  = post "DescribeDBLogFiles"
    response = xmlResponse $ \h x -> DescribeDBLogFilesResponse
        <$> x %| "DescribeDBLogFiles"
        <*> x %| "Marker"
