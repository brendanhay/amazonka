{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeDBLogFiles
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of DB log files for the DB instance.
--
--
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBLogFiles
    (
    -- * Creating a Request
      describeDBLogFiles
    , DescribeDBLogFiles
    -- * Request Lenses
    , ddlfFilenameContains
    , ddlfFilters
    , ddlfFileSize
    , ddlfFileLastWritten
    , ddlfMarker
    , ddlfMaxRecords
    , ddlfDBInstanceIdentifier

    -- * Destructuring the Response
    , describeDBLogFilesResponse
    , DescribeDBLogFilesResponse
    -- * Response Lenses
    , ddlfrsDescribeDBLogFiles
    , ddlfrsMarker
    , ddlfrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.RDS.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'describeDBLogFiles' smart constructor.
data DescribeDBLogFiles = DescribeDBLogFiles'
  { _ddlfFilenameContains     :: !(Maybe Text)
  , _ddlfFilters              :: !(Maybe [Filter])
  , _ddlfFileSize             :: !(Maybe Integer)
  , _ddlfFileLastWritten      :: !(Maybe Integer)
  , _ddlfMarker               :: !(Maybe Text)
  , _ddlfMaxRecords           :: !(Maybe Int)
  , _ddlfDBInstanceIdentifier :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDBLogFiles' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddlfFilenameContains' - Filters the available log files for log file names that contain the specified string.
--
-- * 'ddlfFilters' - This parameter is not currently supported.
--
-- * 'ddlfFileSize' - Filters the available log files for files larger than the specified size.
--
-- * 'ddlfFileLastWritten' - Filters the available log files for files written since the specified date, in POSIX timestamp format with milliseconds.
--
-- * 'ddlfMarker' - The pagination token provided in the previous request. If this parameter is specified the response includes only records beyond the marker, up to MaxRecords.
--
-- * 'ddlfMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified MaxRecords value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
--
-- * 'ddlfDBInstanceIdentifier' - The customer-assigned name of the DB instance that contains the log files you want to list. Constraints:     * Must match the identifier of an existing DBInstance.
describeDBLogFiles
    :: Text -- ^ 'ddlfDBInstanceIdentifier'
    -> DescribeDBLogFiles
describeDBLogFiles pDBInstanceIdentifier_ =
  DescribeDBLogFiles'
    { _ddlfFilenameContains = Nothing
    , _ddlfFilters = Nothing
    , _ddlfFileSize = Nothing
    , _ddlfFileLastWritten = Nothing
    , _ddlfMarker = Nothing
    , _ddlfMaxRecords = Nothing
    , _ddlfDBInstanceIdentifier = pDBInstanceIdentifier_
    }


-- | Filters the available log files for log file names that contain the specified string.
ddlfFilenameContains :: Lens' DescribeDBLogFiles (Maybe Text)
ddlfFilenameContains = lens _ddlfFilenameContains (\ s a -> s{_ddlfFilenameContains = a})

-- | This parameter is not currently supported.
ddlfFilters :: Lens' DescribeDBLogFiles [Filter]
ddlfFilters = lens _ddlfFilters (\ s a -> s{_ddlfFilters = a}) . _Default . _Coerce

-- | Filters the available log files for files larger than the specified size.
ddlfFileSize :: Lens' DescribeDBLogFiles (Maybe Integer)
ddlfFileSize = lens _ddlfFileSize (\ s a -> s{_ddlfFileSize = a})

-- | Filters the available log files for files written since the specified date, in POSIX timestamp format with milliseconds.
ddlfFileLastWritten :: Lens' DescribeDBLogFiles (Maybe Integer)
ddlfFileLastWritten = lens _ddlfFileLastWritten (\ s a -> s{_ddlfFileLastWritten = a})

-- | The pagination token provided in the previous request. If this parameter is specified the response includes only records beyond the marker, up to MaxRecords.
ddlfMarker :: Lens' DescribeDBLogFiles (Maybe Text)
ddlfMarker = lens _ddlfMarker (\ s a -> s{_ddlfMarker = a})

-- | The maximum number of records to include in the response. If more records exist than the specified MaxRecords value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
ddlfMaxRecords :: Lens' DescribeDBLogFiles (Maybe Int)
ddlfMaxRecords = lens _ddlfMaxRecords (\ s a -> s{_ddlfMaxRecords = a})

-- | The customer-assigned name of the DB instance that contains the log files you want to list. Constraints:     * Must match the identifier of an existing DBInstance.
ddlfDBInstanceIdentifier :: Lens' DescribeDBLogFiles Text
ddlfDBInstanceIdentifier = lens _ddlfDBInstanceIdentifier (\ s a -> s{_ddlfDBInstanceIdentifier = a})

instance AWSPager DescribeDBLogFiles where
        page rq rs
          | stop (rs ^. ddlfrsMarker) = Nothing
          | stop (rs ^. ddlfrsDescribeDBLogFiles) = Nothing
          | otherwise =
            Just $ rq & ddlfMarker .~ rs ^. ddlfrsMarker

instance AWSRequest DescribeDBLogFiles where
        type Rs DescribeDBLogFiles =
             DescribeDBLogFilesResponse
        request = postQuery rds
        response
          = receiveXMLWrapper "DescribeDBLogFilesResult"
              (\ s h x ->
                 DescribeDBLogFilesResponse' <$>
                   (x .@? "DescribeDBLogFiles" .!@ mempty >>=
                      may (parseXMLList "DescribeDBLogFilesDetails"))
                     <*> (x .@? "Marker")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeDBLogFiles where

instance NFData DescribeDBLogFiles where

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
               "Filters" =:
                 toQuery (toQueryList "Filter" <$> _ddlfFilters),
               "FileSize" =: _ddlfFileSize,
               "FileLastWritten" =: _ddlfFileLastWritten,
               "Marker" =: _ddlfMarker,
               "MaxRecords" =: _ddlfMaxRecords,
               "DBInstanceIdentifier" =: _ddlfDBInstanceIdentifier]

-- | The response from a call to 'DescribeDBLogFiles' .
--
--
--
-- /See:/ 'describeDBLogFilesResponse' smart constructor.
data DescribeDBLogFilesResponse = DescribeDBLogFilesResponse'
  { _ddlfrsDescribeDBLogFiles :: !(Maybe [DescribeDBLogFilesDetails])
  , _ddlfrsMarker             :: !(Maybe Text)
  , _ddlfrsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDBLogFilesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddlfrsDescribeDBLogFiles' - The DB log files returned.
--
-- * 'ddlfrsMarker' - A pagination token that can be used in a subsequent DescribeDBLogFiles request.
--
-- * 'ddlfrsResponseStatus' - -- | The response status code.
describeDBLogFilesResponse
    :: Int -- ^ 'ddlfrsResponseStatus'
    -> DescribeDBLogFilesResponse
describeDBLogFilesResponse pResponseStatus_ =
  DescribeDBLogFilesResponse'
    { _ddlfrsDescribeDBLogFiles = Nothing
    , _ddlfrsMarker = Nothing
    , _ddlfrsResponseStatus = pResponseStatus_
    }


-- | The DB log files returned.
ddlfrsDescribeDBLogFiles :: Lens' DescribeDBLogFilesResponse [DescribeDBLogFilesDetails]
ddlfrsDescribeDBLogFiles = lens _ddlfrsDescribeDBLogFiles (\ s a -> s{_ddlfrsDescribeDBLogFiles = a}) . _Default . _Coerce

-- | A pagination token that can be used in a subsequent DescribeDBLogFiles request.
ddlfrsMarker :: Lens' DescribeDBLogFilesResponse (Maybe Text)
ddlfrsMarker = lens _ddlfrsMarker (\ s a -> s{_ddlfrsMarker = a})

-- | -- | The response status code.
ddlfrsResponseStatus :: Lens' DescribeDBLogFilesResponse Int
ddlfrsResponseStatus = lens _ddlfrsResponseStatus (\ s a -> s{_ddlfrsResponseStatus = a})

instance NFData DescribeDBLogFilesResponse where
