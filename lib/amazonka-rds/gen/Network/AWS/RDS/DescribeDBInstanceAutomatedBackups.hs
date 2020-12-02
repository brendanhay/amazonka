{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeDBInstanceAutomatedBackups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays backups for both current and deleted instances. For example, use this operation to find details about automated backups for previously deleted instances. Current instances with retention periods greater than zero (0) are returned for both the @DescribeDBInstanceAutomatedBackups@ and @DescribeDBInstances@ operations.
--
--
-- All parameters are optional.
--
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBInstanceAutomatedBackups
  ( -- * Creating a Request
    describeDBInstanceAutomatedBackups,
    DescribeDBInstanceAutomatedBackups,

    -- * Request Lenses
    ddbiabFilters,
    ddbiabDBInstanceIdentifier,
    ddbiabMarker,
    ddbiabMaxRecords,
    ddbiabDBiResourceId,

    -- * Destructuring the Response
    describeDBInstanceAutomatedBackupsResponse,
    DescribeDBInstanceAutomatedBackupsResponse,

    -- * Response Lenses
    ddiabrsDBInstanceAutomatedBackups,
    ddiabrsMarker,
    ddiabrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.Request
import Network.AWS.Response

-- | Parameter input for DescribeDBInstanceAutomatedBackups.
--
--
--
-- /See:/ 'describeDBInstanceAutomatedBackups' smart constructor.
data DescribeDBInstanceAutomatedBackups = DescribeDBInstanceAutomatedBackups'
  { _ddbiabFilters ::
      !(Maybe [Filter]),
    _ddbiabDBInstanceIdentifier ::
      !(Maybe Text),
    _ddbiabMarker ::
      !(Maybe Text),
    _ddbiabMaxRecords ::
      !(Maybe Int),
    _ddbiabDBiResourceId ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeDBInstanceAutomatedBackups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddbiabFilters' - A filter that specifies which resources to return based on status. Supported filters are the following:     * @status@      * @active@ - automated backups for current instances     * @retained@ - automated backups for deleted instances     * @creating@ - automated backups that are waiting for the first automated snapshot to be available     * @db-instance-id@ - Accepts DB instance identifiers and Amazon Resource Names (ARNs) for DB instances. The results list includes only information about the DB instance automated backupss identified by these ARNs.     * @dbi-resource-id@ - Accepts DB instance resource identifiers and DB Amazon Resource Names (ARNs) for DB instances. The results list includes only information about the DB instance resources identified by these ARNs. Returns all resources by default. The status for each resource is specified in the response.
--
-- * 'ddbiabDBInstanceIdentifier' - (Optional) The user-supplied instance identifier. If this parameter is specified, it must match the identifier of an existing DB instance. It returns information from the specific DB instance' automated backup. This parameter isn't case-sensitive.
--
-- * 'ddbiabMarker' - The pagination token provided in the previous request. If this parameter is specified the response includes only records beyond the marker, up to @MaxRecords@ .
--
-- * 'ddbiabMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
--
-- * 'ddbiabDBiResourceId' - The resource ID of the DB instance that is the source of the automated backup. This parameter isn't case-sensitive.
describeDBInstanceAutomatedBackups ::
  DescribeDBInstanceAutomatedBackups
describeDBInstanceAutomatedBackups =
  DescribeDBInstanceAutomatedBackups'
    { _ddbiabFilters = Nothing,
      _ddbiabDBInstanceIdentifier = Nothing,
      _ddbiabMarker = Nothing,
      _ddbiabMaxRecords = Nothing,
      _ddbiabDBiResourceId = Nothing
    }

-- | A filter that specifies which resources to return based on status. Supported filters are the following:     * @status@      * @active@ - automated backups for current instances     * @retained@ - automated backups for deleted instances     * @creating@ - automated backups that are waiting for the first automated snapshot to be available     * @db-instance-id@ - Accepts DB instance identifiers and Amazon Resource Names (ARNs) for DB instances. The results list includes only information about the DB instance automated backupss identified by these ARNs.     * @dbi-resource-id@ - Accepts DB instance resource identifiers and DB Amazon Resource Names (ARNs) for DB instances. The results list includes only information about the DB instance resources identified by these ARNs. Returns all resources by default. The status for each resource is specified in the response.
ddbiabFilters :: Lens' DescribeDBInstanceAutomatedBackups [Filter]
ddbiabFilters = lens _ddbiabFilters (\s a -> s {_ddbiabFilters = a}) . _Default . _Coerce

-- | (Optional) The user-supplied instance identifier. If this parameter is specified, it must match the identifier of an existing DB instance. It returns information from the specific DB instance' automated backup. This parameter isn't case-sensitive.
ddbiabDBInstanceIdentifier :: Lens' DescribeDBInstanceAutomatedBackups (Maybe Text)
ddbiabDBInstanceIdentifier = lens _ddbiabDBInstanceIdentifier (\s a -> s {_ddbiabDBInstanceIdentifier = a})

-- | The pagination token provided in the previous request. If this parameter is specified the response includes only records beyond the marker, up to @MaxRecords@ .
ddbiabMarker :: Lens' DescribeDBInstanceAutomatedBackups (Maybe Text)
ddbiabMarker = lens _ddbiabMarker (\s a -> s {_ddbiabMarker = a})

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
ddbiabMaxRecords :: Lens' DescribeDBInstanceAutomatedBackups (Maybe Int)
ddbiabMaxRecords = lens _ddbiabMaxRecords (\s a -> s {_ddbiabMaxRecords = a})

-- | The resource ID of the DB instance that is the source of the automated backup. This parameter isn't case-sensitive.
ddbiabDBiResourceId :: Lens' DescribeDBInstanceAutomatedBackups (Maybe Text)
ddbiabDBiResourceId = lens _ddbiabDBiResourceId (\s a -> s {_ddbiabDBiResourceId = a})

instance AWSPager DescribeDBInstanceAutomatedBackups where
  page rq rs
    | stop (rs ^. ddiabrsMarker) = Nothing
    | stop (rs ^. ddiabrsDBInstanceAutomatedBackups) = Nothing
    | otherwise = Just $ rq & ddbiabMarker .~ rs ^. ddiabrsMarker

instance AWSRequest DescribeDBInstanceAutomatedBackups where
  type
    Rs DescribeDBInstanceAutomatedBackups =
      DescribeDBInstanceAutomatedBackupsResponse
  request = postQuery rds
  response =
    receiveXMLWrapper
      "DescribeDBInstanceAutomatedBackupsResult"
      ( \s h x ->
          DescribeDBInstanceAutomatedBackupsResponse'
            <$> ( x .@? "DBInstanceAutomatedBackups" .!@ mempty
                    >>= may (parseXMLList "DBInstanceAutomatedBackup")
                )
            <*> (x .@? "Marker")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeDBInstanceAutomatedBackups

instance NFData DescribeDBInstanceAutomatedBackups

instance ToHeaders DescribeDBInstanceAutomatedBackups where
  toHeaders = const mempty

instance ToPath DescribeDBInstanceAutomatedBackups where
  toPath = const "/"

instance ToQuery DescribeDBInstanceAutomatedBackups where
  toQuery DescribeDBInstanceAutomatedBackups' {..} =
    mconcat
      [ "Action" =: ("DescribeDBInstanceAutomatedBackups" :: ByteString),
        "Version" =: ("2014-10-31" :: ByteString),
        "Filters" =: toQuery (toQueryList "Filter" <$> _ddbiabFilters),
        "DBInstanceIdentifier" =: _ddbiabDBInstanceIdentifier,
        "Marker" =: _ddbiabMarker,
        "MaxRecords" =: _ddbiabMaxRecords,
        "DbiResourceId" =: _ddbiabDBiResourceId
      ]

-- | Contains the result of a successful invocation of the @DescribeDBInstanceAutomatedBackups@ action.
--
--
--
-- /See:/ 'describeDBInstanceAutomatedBackupsResponse' smart constructor.
data DescribeDBInstanceAutomatedBackupsResponse = DescribeDBInstanceAutomatedBackupsResponse'
  { _ddiabrsDBInstanceAutomatedBackups ::
      !( Maybe
           [DBInstanceAutomatedBackup]
       ),
    _ddiabrsMarker ::
      !( Maybe
           Text
       ),
    _ddiabrsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DescribeDBInstanceAutomatedBackupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddiabrsDBInstanceAutomatedBackups' - A list of @DBInstanceAutomatedBackup@ instances.
--
-- * 'ddiabrsMarker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'ddiabrsResponseStatus' - -- | The response status code.
describeDBInstanceAutomatedBackupsResponse ::
  -- | 'ddiabrsResponseStatus'
  Int ->
  DescribeDBInstanceAutomatedBackupsResponse
describeDBInstanceAutomatedBackupsResponse pResponseStatus_ =
  DescribeDBInstanceAutomatedBackupsResponse'
    { _ddiabrsDBInstanceAutomatedBackups =
        Nothing,
      _ddiabrsMarker = Nothing,
      _ddiabrsResponseStatus = pResponseStatus_
    }

-- | A list of @DBInstanceAutomatedBackup@ instances.
ddiabrsDBInstanceAutomatedBackups :: Lens' DescribeDBInstanceAutomatedBackupsResponse [DBInstanceAutomatedBackup]
ddiabrsDBInstanceAutomatedBackups = lens _ddiabrsDBInstanceAutomatedBackups (\s a -> s {_ddiabrsDBInstanceAutomatedBackups = a}) . _Default . _Coerce

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
ddiabrsMarker :: Lens' DescribeDBInstanceAutomatedBackupsResponse (Maybe Text)
ddiabrsMarker = lens _ddiabrsMarker (\s a -> s {_ddiabrsMarker = a})

-- | -- | The response status code.
ddiabrsResponseStatus :: Lens' DescribeDBInstanceAutomatedBackupsResponse Int
ddiabrsResponseStatus = lens _ddiabrsResponseStatus (\s a -> s {_ddiabrsResponseStatus = a})

instance NFData DescribeDBInstanceAutomatedBackupsResponse
