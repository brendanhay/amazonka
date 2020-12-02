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
-- Module      : Network.AWS.RDS.DescribeInstallationMedia
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the available installation media for a DB engine that requires an on-premises customer provided license, such as Microsoft SQL Server.
--
--
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeInstallationMedia
  ( -- * Creating a Request
    describeInstallationMedia,
    DescribeInstallationMedia,

    -- * Request Lenses
    dimInstallationMediaId,
    dimFilters,
    dimMarker,
    dimMaxRecords,

    -- * Destructuring the Response
    describeInstallationMediaResponse,
    DescribeInstallationMediaResponse,

    -- * Response Lenses
    dimrsMarker,
    dimrsInstallationMedia,
    dimrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeInstallationMedia' smart constructor.
data DescribeInstallationMedia = DescribeInstallationMedia'
  { _dimInstallationMediaId ::
      !(Maybe Text),
    _dimFilters :: !(Maybe [Filter]),
    _dimMarker :: !(Maybe Text),
    _dimMaxRecords :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeInstallationMedia' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dimInstallationMediaId' - The installation medium ID.
--
-- * 'dimFilters' - A filter that specifies one or more installation media to describe. Supported filters include the following:     * @custom-availability-zone-id@ - Accepts custom Availability Zone (AZ) identifiers. The results list includes information about only the custom AZs identified by these identifiers.     * @engine@ - Accepts database engines. The results list includes information about only the database engines identified by these identifiers. For more information about the valid engines for installation media, see 'ImportInstallationMedia' .
--
-- * 'dimMarker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'dimMaxRecords' - An optional pagination token provided by a previous DescribeInstallationMedia request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
describeInstallationMedia ::
  DescribeInstallationMedia
describeInstallationMedia =
  DescribeInstallationMedia'
    { _dimInstallationMediaId = Nothing,
      _dimFilters = Nothing,
      _dimMarker = Nothing,
      _dimMaxRecords = Nothing
    }

-- | The installation medium ID.
dimInstallationMediaId :: Lens' DescribeInstallationMedia (Maybe Text)
dimInstallationMediaId = lens _dimInstallationMediaId (\s a -> s {_dimInstallationMediaId = a})

-- | A filter that specifies one or more installation media to describe. Supported filters include the following:     * @custom-availability-zone-id@ - Accepts custom Availability Zone (AZ) identifiers. The results list includes information about only the custom AZs identified by these identifiers.     * @engine@ - Accepts database engines. The results list includes information about only the database engines identified by these identifiers. For more information about the valid engines for installation media, see 'ImportInstallationMedia' .
dimFilters :: Lens' DescribeInstallationMedia [Filter]
dimFilters = lens _dimFilters (\s a -> s {_dimFilters = a}) . _Default . _Coerce

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
dimMarker :: Lens' DescribeInstallationMedia (Maybe Text)
dimMarker = lens _dimMarker (\s a -> s {_dimMarker = a})

-- | An optional pagination token provided by a previous DescribeInstallationMedia request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
dimMaxRecords :: Lens' DescribeInstallationMedia (Maybe Int)
dimMaxRecords = lens _dimMaxRecords (\s a -> s {_dimMaxRecords = a})

instance AWSPager DescribeInstallationMedia where
  page rq rs
    | stop (rs ^. dimrsMarker) = Nothing
    | stop (rs ^. dimrsInstallationMedia) = Nothing
    | otherwise = Just $ rq & dimMarker .~ rs ^. dimrsMarker

instance AWSRequest DescribeInstallationMedia where
  type
    Rs DescribeInstallationMedia =
      DescribeInstallationMediaResponse
  request = postQuery rds
  response =
    receiveXMLWrapper
      "DescribeInstallationMediaResult"
      ( \s h x ->
          DescribeInstallationMediaResponse'
            <$> (x .@? "Marker")
            <*> ( x .@? "InstallationMedia" .!@ mempty
                    >>= may (parseXMLList "InstallationMedia")
                )
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeInstallationMedia

instance NFData DescribeInstallationMedia

instance ToHeaders DescribeInstallationMedia where
  toHeaders = const mempty

instance ToPath DescribeInstallationMedia where
  toPath = const "/"

instance ToQuery DescribeInstallationMedia where
  toQuery DescribeInstallationMedia' {..} =
    mconcat
      [ "Action" =: ("DescribeInstallationMedia" :: ByteString),
        "Version" =: ("2014-10-31" :: ByteString),
        "InstallationMediaId" =: _dimInstallationMediaId,
        "Filters" =: toQuery (toQueryList "Filter" <$> _dimFilters),
        "Marker" =: _dimMarker,
        "MaxRecords" =: _dimMaxRecords
      ]

-- | /See:/ 'describeInstallationMediaResponse' smart constructor.
data DescribeInstallationMediaResponse = DescribeInstallationMediaResponse'
  { _dimrsMarker ::
      !(Maybe Text),
    _dimrsInstallationMedia ::
      !( Maybe
           [InstallationMedia]
       ),
    _dimrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeInstallationMediaResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dimrsMarker' - An optional pagination token provided by a previous 'DescribeInstallationMedia' request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'dimrsInstallationMedia' - The list of 'InstallationMedia' objects for the AWS account.
--
-- * 'dimrsResponseStatus' - -- | The response status code.
describeInstallationMediaResponse ::
  -- | 'dimrsResponseStatus'
  Int ->
  DescribeInstallationMediaResponse
describeInstallationMediaResponse pResponseStatus_ =
  DescribeInstallationMediaResponse'
    { _dimrsMarker = Nothing,
      _dimrsInstallationMedia = Nothing,
      _dimrsResponseStatus = pResponseStatus_
    }

-- | An optional pagination token provided by a previous 'DescribeInstallationMedia' request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
dimrsMarker :: Lens' DescribeInstallationMediaResponse (Maybe Text)
dimrsMarker = lens _dimrsMarker (\s a -> s {_dimrsMarker = a})

-- | The list of 'InstallationMedia' objects for the AWS account.
dimrsInstallationMedia :: Lens' DescribeInstallationMediaResponse [InstallationMedia]
dimrsInstallationMedia = lens _dimrsInstallationMedia (\s a -> s {_dimrsInstallationMedia = a}) . _Default . _Coerce

-- | -- | The response status code.
dimrsResponseStatus :: Lens' DescribeInstallationMediaResponse Int
dimrsResponseStatus = lens _dimrsResponseStatus (\s a -> s {_dimrsResponseStatus = a})

instance NFData DescribeInstallationMediaResponse
