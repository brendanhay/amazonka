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
-- Module      : Network.AWS.RDS.DescribeEngineDefaultParameters
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the default engine and system parameter information for the specified database engine.
--
--
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeEngineDefaultParameters
    (
    -- * Creating a Request
      describeEngineDefaultParameters
    , DescribeEngineDefaultParameters
    -- * Request Lenses
    , dedpFilters
    , dedpMarker
    , dedpMaxRecords
    , dedpDBParameterGroupFamily

    -- * Destructuring the Response
    , describeEngineDefaultParametersResponse
    , DescribeEngineDefaultParametersResponse
    -- * Response Lenses
    , dedprsResponseStatus
    , dedprsEngineDefaults
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
-- /See:/ 'describeEngineDefaultParameters' smart constructor.
data DescribeEngineDefaultParameters = DescribeEngineDefaultParameters'
  { _dedpFilters                :: !(Maybe [Filter])
  , _dedpMarker                 :: !(Maybe Text)
  , _dedpMaxRecords             :: !(Maybe Int)
  , _dedpDBParameterGroupFamily :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEngineDefaultParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dedpFilters' - This parameter is not currently supported.
--
-- * 'dedpMarker' - An optional pagination token provided by a previous @DescribeEngineDefaultParameters@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'dedpMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
--
-- * 'dedpDBParameterGroupFamily' - The name of the DB parameter group family.
describeEngineDefaultParameters
    :: Text -- ^ 'dedpDBParameterGroupFamily'
    -> DescribeEngineDefaultParameters
describeEngineDefaultParameters pDBParameterGroupFamily_ =
  DescribeEngineDefaultParameters'
    { _dedpFilters = Nothing
    , _dedpMarker = Nothing
    , _dedpMaxRecords = Nothing
    , _dedpDBParameterGroupFamily = pDBParameterGroupFamily_
    }


-- | This parameter is not currently supported.
dedpFilters :: Lens' DescribeEngineDefaultParameters [Filter]
dedpFilters = lens _dedpFilters (\ s a -> s{_dedpFilters = a}) . _Default . _Coerce

-- | An optional pagination token provided by a previous @DescribeEngineDefaultParameters@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
dedpMarker :: Lens' DescribeEngineDefaultParameters (Maybe Text)
dedpMarker = lens _dedpMarker (\ s a -> s{_dedpMarker = a})

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
dedpMaxRecords :: Lens' DescribeEngineDefaultParameters (Maybe Int)
dedpMaxRecords = lens _dedpMaxRecords (\ s a -> s{_dedpMaxRecords = a})

-- | The name of the DB parameter group family.
dedpDBParameterGroupFamily :: Lens' DescribeEngineDefaultParameters Text
dedpDBParameterGroupFamily = lens _dedpDBParameterGroupFamily (\ s a -> s{_dedpDBParameterGroupFamily = a})

instance AWSPager DescribeEngineDefaultParameters
         where
        page rq rs
          | stop
              (rs ^? dedprsEngineDefaults . edMarker . _Just)
            = Nothing
          | stop (rs ^. dedprsEngineDefaults . edParameters) =
            Nothing
          | otherwise =
            Just $ rq &
              dedpMarker .~
                rs ^? dedprsEngineDefaults . edMarker . _Just

instance AWSRequest DescribeEngineDefaultParameters
         where
        type Rs DescribeEngineDefaultParameters =
             DescribeEngineDefaultParametersResponse
        request = postQuery rds
        response
          = receiveXMLWrapper
              "DescribeEngineDefaultParametersResult"
              (\ s h x ->
                 DescribeEngineDefaultParametersResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "EngineDefaults"))

instance Hashable DescribeEngineDefaultParameters
         where

instance NFData DescribeEngineDefaultParameters where

instance ToHeaders DescribeEngineDefaultParameters
         where
        toHeaders = const mempty

instance ToPath DescribeEngineDefaultParameters where
        toPath = const "/"

instance ToQuery DescribeEngineDefaultParameters
         where
        toQuery DescribeEngineDefaultParameters'{..}
          = mconcat
              ["Action" =:
                 ("DescribeEngineDefaultParameters" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "Filters" =:
                 toQuery (toQueryList "Filter" <$> _dedpFilters),
               "Marker" =: _dedpMarker,
               "MaxRecords" =: _dedpMaxRecords,
               "DBParameterGroupFamily" =:
                 _dedpDBParameterGroupFamily]

-- | /See:/ 'describeEngineDefaultParametersResponse' smart constructor.
data DescribeEngineDefaultParametersResponse = DescribeEngineDefaultParametersResponse'
  { _dedprsResponseStatus :: !Int
  , _dedprsEngineDefaults :: !EngineDefaults
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEngineDefaultParametersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dedprsResponseStatus' - -- | The response status code.
--
-- * 'dedprsEngineDefaults' - Undocumented member.
describeEngineDefaultParametersResponse
    :: Int -- ^ 'dedprsResponseStatus'
    -> EngineDefaults -- ^ 'dedprsEngineDefaults'
    -> DescribeEngineDefaultParametersResponse
describeEngineDefaultParametersResponse pResponseStatus_ pEngineDefaults_ =
  DescribeEngineDefaultParametersResponse'
    { _dedprsResponseStatus = pResponseStatus_
    , _dedprsEngineDefaults = pEngineDefaults_
    }


-- | -- | The response status code.
dedprsResponseStatus :: Lens' DescribeEngineDefaultParametersResponse Int
dedprsResponseStatus = lens _dedprsResponseStatus (\ s a -> s{_dedprsResponseStatus = a})

-- | Undocumented member.
dedprsEngineDefaults :: Lens' DescribeEngineDefaultParametersResponse EngineDefaults
dedprsEngineDefaults = lens _dedprsEngineDefaults (\ s a -> s{_dedprsEngineDefaults = a})

instance NFData
           DescribeEngineDefaultParametersResponse
         where
