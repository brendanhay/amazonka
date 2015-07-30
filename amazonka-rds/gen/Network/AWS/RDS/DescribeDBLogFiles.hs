{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeDBLogFiles
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of DB log files for the DB instance.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DescribeDBLogFiles.html>
module Network.AWS.RDS.DescribeDBLogFiles
    (
    -- * Request
      DescribeDBLogFiles
    -- ** Request constructor
    , describeDBLogFiles
    -- ** Request lenses
    , ddlfFilenameContains
    , ddlfFileSize
    , ddlfFileLastWritten
    , ddlfFilters
    , ddlfMaxRecords
    , ddlfMarker
    , ddlfDBInstanceIdentifier

    -- * Response
    , DescribeDBLogFilesResponse
    -- ** Response constructor
    , describeDBLogFilesResponse
    -- ** Response lenses
    , ddlfrsDescribeDBLogFiles
    , ddlfrsMarker
    , ddlfrsStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'describeDBLogFiles' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddlfFilenameContains'
--
-- * 'ddlfFileSize'
--
-- * 'ddlfFileLastWritten'
--
-- * 'ddlfFilters'
--
-- * 'ddlfMaxRecords'
--
-- * 'ddlfMarker'
--
-- * 'ddlfDBInstanceIdentifier'
data DescribeDBLogFiles = DescribeDBLogFiles'
    { _ddlfFilenameContains     :: !(Maybe Text)
    , _ddlfFileSize             :: !(Maybe Integer)
    , _ddlfFileLastWritten      :: !(Maybe Integer)
    , _ddlfFilters              :: !(Maybe [Filter])
    , _ddlfMaxRecords           :: !(Maybe Int)
    , _ddlfMarker               :: !(Maybe Text)
    , _ddlfDBInstanceIdentifier :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeDBLogFiles' smart constructor.
describeDBLogFiles :: Text -> DescribeDBLogFiles
describeDBLogFiles pDBInstanceIdentifier_ =
    DescribeDBLogFiles'
    { _ddlfFilenameContains = Nothing
    , _ddlfFileSize = Nothing
    , _ddlfFileLastWritten = Nothing
    , _ddlfFilters = Nothing
    , _ddlfMaxRecords = Nothing
    , _ddlfMarker = Nothing
    , _ddlfDBInstanceIdentifier = pDBInstanceIdentifier_
    }

-- | Filters the available log files for log file names that contain the
-- specified string.
ddlfFilenameContains :: Lens' DescribeDBLogFiles (Maybe Text)
ddlfFilenameContains = lens _ddlfFilenameContains (\ s a -> s{_ddlfFilenameContains = a});

-- | Filters the available log files for files larger than the specified
-- size.
ddlfFileSize :: Lens' DescribeDBLogFiles (Maybe Integer)
ddlfFileSize = lens _ddlfFileSize (\ s a -> s{_ddlfFileSize = a});

-- | Filters the available log files for files written since the specified
-- date, in POSIX timestamp format.
ddlfFileLastWritten :: Lens' DescribeDBLogFiles (Maybe Integer)
ddlfFileLastWritten = lens _ddlfFileLastWritten (\ s a -> s{_ddlfFileLastWritten = a});

-- | This parameter is not currently supported.
ddlfFilters :: Lens' DescribeDBLogFiles [Filter]
ddlfFilters = lens _ddlfFilters (\ s a -> s{_ddlfFilters = a}) . _Default . _Coerce;

-- | The maximum number of records to include in the response. If more
-- records exist than the specified MaxRecords value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
ddlfMaxRecords :: Lens' DescribeDBLogFiles (Maybe Int)
ddlfMaxRecords = lens _ddlfMaxRecords (\ s a -> s{_ddlfMaxRecords = a});

-- | The pagination token provided in the previous request. If this parameter
-- is specified the response includes only records beyond the marker, up to
-- MaxRecords.
ddlfMarker :: Lens' DescribeDBLogFiles (Maybe Text)
ddlfMarker = lens _ddlfMarker (\ s a -> s{_ddlfMarker = a});

-- | The customer-assigned name of the DB instance that contains the log
-- files you want to list.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 alphanumeric characters or hyphens
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
ddlfDBInstanceIdentifier :: Lens' DescribeDBLogFiles Text
ddlfDBInstanceIdentifier = lens _ddlfDBInstanceIdentifier (\ s a -> s{_ddlfDBInstanceIdentifier = a});

instance AWSPager DescribeDBLogFiles where
        page rq rs
          | stop (rs ^. ddlfrsMarker) = Nothing
          | stop (rs ^. ddlfrsDescribeDBLogFiles) = Nothing
          | otherwise =
            Just $ rq & ddlfMarker .~ rs ^. ddlfrsMarker

instance AWSRequest DescribeDBLogFiles where
        type Sv DescribeDBLogFiles = RDS
        type Rs DescribeDBLogFiles =
             DescribeDBLogFilesResponse
        request = postQuery
        response
          = receiveXMLWrapper "DescribeDBLogFilesResult"
              (\ s h x ->
                 DescribeDBLogFilesResponse' <$>
                   (x .@? "DescribeDBLogFiles" .!@ mempty >>=
                      may (parseXMLList "DescribeDBLogFilesDetails"))
                     <*> (x .@? "Marker")
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeDBLogFiles where
        toHeaders = const mempty

instance ToPath DescribeDBLogFiles where
        toPath = const "/"

instance ToQuery DescribeDBLogFiles where
        toQuery DescribeDBLogFiles'{..}
          = mconcat
              ["Action" =: ("DescribeDBLogFiles" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "FilenameContains" =: _ddlfFilenameContains,
               "FileSize" =: _ddlfFileSize,
               "FileLastWritten" =: _ddlfFileLastWritten,
               "Filters" =:
                 toQuery (toQueryList "Filter" <$> _ddlfFilters),
               "MaxRecords" =: _ddlfMaxRecords,
               "Marker" =: _ddlfMarker,
               "DBInstanceIdentifier" =: _ddlfDBInstanceIdentifier]

-- | The response from a call to DescribeDBLogFiles.
--
-- /See:/ 'describeDBLogFilesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddlfrsDescribeDBLogFiles'
--
-- * 'ddlfrsMarker'
--
-- * 'ddlfrsStatus'
data DescribeDBLogFilesResponse = DescribeDBLogFilesResponse'
    { _ddlfrsDescribeDBLogFiles :: !(Maybe [DescribeDBLogFilesDetails])
    , _ddlfrsMarker             :: !(Maybe Text)
    , _ddlfrsStatus             :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeDBLogFilesResponse' smart constructor.
describeDBLogFilesResponse :: Int -> DescribeDBLogFilesResponse
describeDBLogFilesResponse pStatus_ =
    DescribeDBLogFilesResponse'
    { _ddlfrsDescribeDBLogFiles = Nothing
    , _ddlfrsMarker = Nothing
    , _ddlfrsStatus = pStatus_
    }

-- | The DB log files returned.
ddlfrsDescribeDBLogFiles :: Lens' DescribeDBLogFilesResponse [DescribeDBLogFilesDetails]
ddlfrsDescribeDBLogFiles = lens _ddlfrsDescribeDBLogFiles (\ s a -> s{_ddlfrsDescribeDBLogFiles = a}) . _Default . _Coerce;

-- | A pagination token that can be used in a subsequent DescribeDBLogFiles
-- request.
ddlfrsMarker :: Lens' DescribeDBLogFilesResponse (Maybe Text)
ddlfrsMarker = lens _ddlfrsMarker (\ s a -> s{_ddlfrsMarker = a});

-- | FIXME: Undocumented member.
ddlfrsStatus :: Lens' DescribeDBLogFilesResponse Int
ddlfrsStatus = lens _ddlfrsStatus (\ s a -> s{_ddlfrsStatus = a});
