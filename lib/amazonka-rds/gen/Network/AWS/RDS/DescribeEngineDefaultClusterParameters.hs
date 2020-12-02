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
-- Module      : Network.AWS.RDS.DescribeEngineDefaultClusterParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the default engine and system parameter information for the cluster database engine.
--
--
-- For more information on Amazon Aurora, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?> in the /Amazon Aurora User Guide./
--
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeEngineDefaultClusterParameters
  ( -- * Creating a Request
    describeEngineDefaultClusterParameters,
    DescribeEngineDefaultClusterParameters,

    -- * Request Lenses
    dedcpFilters,
    dedcpMarker,
    dedcpMaxRecords,
    dedcpDBParameterGroupFamily,

    -- * Destructuring the Response
    describeEngineDefaultClusterParametersResponse,
    DescribeEngineDefaultClusterParametersResponse,

    -- * Response Lenses
    dedcprsEngineDefaults,
    dedcprsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'describeEngineDefaultClusterParameters' smart constructor.
data DescribeEngineDefaultClusterParameters = DescribeEngineDefaultClusterParameters'
  { _dedcpFilters ::
      !( Maybe
           [Filter]
       ),
    _dedcpMarker ::
      !(Maybe Text),
    _dedcpMaxRecords ::
      !(Maybe Int),
    _dedcpDBParameterGroupFamily ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeEngineDefaultClusterParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dedcpFilters' - This parameter isn't currently supported.
--
-- * 'dedcpMarker' - An optional pagination token provided by a previous @DescribeEngineDefaultClusterParameters@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'dedcpMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results.  Default: 100 Constraints: Minimum 20, maximum 100.
--
-- * 'dedcpDBParameterGroupFamily' - The name of the DB cluster parameter group family to return engine parameter information for.
describeEngineDefaultClusterParameters ::
  -- | 'dedcpDBParameterGroupFamily'
  Text ->
  DescribeEngineDefaultClusterParameters
describeEngineDefaultClusterParameters pDBParameterGroupFamily_ =
  DescribeEngineDefaultClusterParameters'
    { _dedcpFilters = Nothing,
      _dedcpMarker = Nothing,
      _dedcpMaxRecords = Nothing,
      _dedcpDBParameterGroupFamily = pDBParameterGroupFamily_
    }

-- | This parameter isn't currently supported.
dedcpFilters :: Lens' DescribeEngineDefaultClusterParameters [Filter]
dedcpFilters = lens _dedcpFilters (\s a -> s {_dedcpFilters = a}) . _Default . _Coerce

-- | An optional pagination token provided by a previous @DescribeEngineDefaultClusterParameters@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
dedcpMarker :: Lens' DescribeEngineDefaultClusterParameters (Maybe Text)
dedcpMarker = lens _dedcpMarker (\s a -> s {_dedcpMarker = a})

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results.  Default: 100 Constraints: Minimum 20, maximum 100.
dedcpMaxRecords :: Lens' DescribeEngineDefaultClusterParameters (Maybe Int)
dedcpMaxRecords = lens _dedcpMaxRecords (\s a -> s {_dedcpMaxRecords = a})

-- | The name of the DB cluster parameter group family to return engine parameter information for.
dedcpDBParameterGroupFamily :: Lens' DescribeEngineDefaultClusterParameters Text
dedcpDBParameterGroupFamily = lens _dedcpDBParameterGroupFamily (\s a -> s {_dedcpDBParameterGroupFamily = a})

instance AWSPager DescribeEngineDefaultClusterParameters where
  page rq rs
    | stop (rs ^? dedcprsEngineDefaults . _Just . edMarker . _Just) =
      Nothing
    | stop (rs ^? dedcprsEngineDefaults . _Just . edParameters) =
      Nothing
    | otherwise =
      Just $
        rq
          & dedcpMarker
          .~ rs ^? dedcprsEngineDefaults . _Just . edMarker . _Just

instance AWSRequest DescribeEngineDefaultClusterParameters where
  type
    Rs DescribeEngineDefaultClusterParameters =
      DescribeEngineDefaultClusterParametersResponse
  request = postQuery rds
  response =
    receiveXMLWrapper
      "DescribeEngineDefaultClusterParametersResult"
      ( \s h x ->
          DescribeEngineDefaultClusterParametersResponse'
            <$> (x .@? "EngineDefaults") <*> (pure (fromEnum s))
      )

instance Hashable DescribeEngineDefaultClusterParameters

instance NFData DescribeEngineDefaultClusterParameters

instance ToHeaders DescribeEngineDefaultClusterParameters where
  toHeaders = const mempty

instance ToPath DescribeEngineDefaultClusterParameters where
  toPath = const "/"

instance ToQuery DescribeEngineDefaultClusterParameters where
  toQuery DescribeEngineDefaultClusterParameters' {..} =
    mconcat
      [ "Action"
          =: ("DescribeEngineDefaultClusterParameters" :: ByteString),
        "Version" =: ("2014-10-31" :: ByteString),
        "Filters" =: toQuery (toQueryList "Filter" <$> _dedcpFilters),
        "Marker" =: _dedcpMarker,
        "MaxRecords" =: _dedcpMaxRecords,
        "DBParameterGroupFamily" =: _dedcpDBParameterGroupFamily
      ]

-- | /See:/ 'describeEngineDefaultClusterParametersResponse' smart constructor.
data DescribeEngineDefaultClusterParametersResponse = DescribeEngineDefaultClusterParametersResponse'
  { _dedcprsEngineDefaults ::
      !( Maybe
           EngineDefaults
       ),
    _dedcprsResponseStatus ::
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

-- | Creates a value of 'DescribeEngineDefaultClusterParametersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dedcprsEngineDefaults' - Undocumented member.
--
-- * 'dedcprsResponseStatus' - -- | The response status code.
describeEngineDefaultClusterParametersResponse ::
  -- | 'dedcprsResponseStatus'
  Int ->
  DescribeEngineDefaultClusterParametersResponse
describeEngineDefaultClusterParametersResponse pResponseStatus_ =
  DescribeEngineDefaultClusterParametersResponse'
    { _dedcprsEngineDefaults =
        Nothing,
      _dedcprsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
dedcprsEngineDefaults :: Lens' DescribeEngineDefaultClusterParametersResponse (Maybe EngineDefaults)
dedcprsEngineDefaults = lens _dedcprsEngineDefaults (\s a -> s {_dedcprsEngineDefaults = a})

-- | -- | The response status code.
dedcprsResponseStatus :: Lens' DescribeEngineDefaultClusterParametersResponse Int
dedcprsResponseStatus = lens _dedcprsResponseStatus (\s a -> s {_dedcprsResponseStatus = a})

instance NFData DescribeEngineDefaultClusterParametersResponse
