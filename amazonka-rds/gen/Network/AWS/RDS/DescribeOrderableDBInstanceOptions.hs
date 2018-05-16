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
-- Module      : Network.AWS.RDS.DescribeOrderableDBInstanceOptions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of orderable DB instance options for the specified engine.
--
--
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeOrderableDBInstanceOptions
    (
    -- * Creating a Request
      describeOrderableDBInstanceOptions
    , DescribeOrderableDBInstanceOptions
    -- * Request Lenses
    , dodioEngineVersion
    , dodioFilters
    , dodioDBInstanceClass
    , dodioLicenseModel
    , dodioMarker
    , dodioMaxRecords
    , dodioVPC
    , dodioEngine

    -- * Destructuring the Response
    , describeOrderableDBInstanceOptionsResponse
    , DescribeOrderableDBInstanceOptionsResponse
    -- * Response Lenses
    , dodiorsOrderableDBInstanceOptions
    , dodiorsMarker
    , dodiorsResponseStatus
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
-- /See:/ 'describeOrderableDBInstanceOptions' smart constructor.
data DescribeOrderableDBInstanceOptions = DescribeOrderableDBInstanceOptions'
  { _dodioEngineVersion   :: !(Maybe Text)
  , _dodioFilters         :: !(Maybe [Filter])
  , _dodioDBInstanceClass :: !(Maybe Text)
  , _dodioLicenseModel    :: !(Maybe Text)
  , _dodioMarker          :: !(Maybe Text)
  , _dodioMaxRecords      :: !(Maybe Int)
  , _dodioVPC             :: !(Maybe Bool)
  , _dodioEngine          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeOrderableDBInstanceOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dodioEngineVersion' - The engine version filter value. Specify this parameter to show only the available offerings matching the specified engine version.
--
-- * 'dodioFilters' - This parameter is not currently supported.
--
-- * 'dodioDBInstanceClass' - The DB instance class filter value. Specify this parameter to show only the available offerings matching the specified DB instance class.
--
-- * 'dodioLicenseModel' - The license model filter value. Specify this parameter to show only the available offerings matching the specified license model.
--
-- * 'dodioMarker' - An optional pagination token provided by a previous DescribeOrderableDBInstanceOptions request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'dodioMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
--
-- * 'dodioVPC' - The VPC filter value. Specify this parameter to show only the available VPC or non-VPC offerings.
--
-- * 'dodioEngine' - The name of the engine to retrieve DB instance options for.
describeOrderableDBInstanceOptions
    :: Text -- ^ 'dodioEngine'
    -> DescribeOrderableDBInstanceOptions
describeOrderableDBInstanceOptions pEngine_ =
  DescribeOrderableDBInstanceOptions'
    { _dodioEngineVersion = Nothing
    , _dodioFilters = Nothing
    , _dodioDBInstanceClass = Nothing
    , _dodioLicenseModel = Nothing
    , _dodioMarker = Nothing
    , _dodioMaxRecords = Nothing
    , _dodioVPC = Nothing
    , _dodioEngine = pEngine_
    }


-- | The engine version filter value. Specify this parameter to show only the available offerings matching the specified engine version.
dodioEngineVersion :: Lens' DescribeOrderableDBInstanceOptions (Maybe Text)
dodioEngineVersion = lens _dodioEngineVersion (\ s a -> s{_dodioEngineVersion = a})

-- | This parameter is not currently supported.
dodioFilters :: Lens' DescribeOrderableDBInstanceOptions [Filter]
dodioFilters = lens _dodioFilters (\ s a -> s{_dodioFilters = a}) . _Default . _Coerce

-- | The DB instance class filter value. Specify this parameter to show only the available offerings matching the specified DB instance class.
dodioDBInstanceClass :: Lens' DescribeOrderableDBInstanceOptions (Maybe Text)
dodioDBInstanceClass = lens _dodioDBInstanceClass (\ s a -> s{_dodioDBInstanceClass = a})

-- | The license model filter value. Specify this parameter to show only the available offerings matching the specified license model.
dodioLicenseModel :: Lens' DescribeOrderableDBInstanceOptions (Maybe Text)
dodioLicenseModel = lens _dodioLicenseModel (\ s a -> s{_dodioLicenseModel = a})

-- | An optional pagination token provided by a previous DescribeOrderableDBInstanceOptions request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
dodioMarker :: Lens' DescribeOrderableDBInstanceOptions (Maybe Text)
dodioMarker = lens _dodioMarker (\ s a -> s{_dodioMarker = a})

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
dodioMaxRecords :: Lens' DescribeOrderableDBInstanceOptions (Maybe Int)
dodioMaxRecords = lens _dodioMaxRecords (\ s a -> s{_dodioMaxRecords = a})

-- | The VPC filter value. Specify this parameter to show only the available VPC or non-VPC offerings.
dodioVPC :: Lens' DescribeOrderableDBInstanceOptions (Maybe Bool)
dodioVPC = lens _dodioVPC (\ s a -> s{_dodioVPC = a})

-- | The name of the engine to retrieve DB instance options for.
dodioEngine :: Lens' DescribeOrderableDBInstanceOptions Text
dodioEngine = lens _dodioEngine (\ s a -> s{_dodioEngine = a})

instance AWSPager DescribeOrderableDBInstanceOptions
         where
        page rq rs
          | stop (rs ^. dodiorsMarker) = Nothing
          | stop (rs ^. dodiorsOrderableDBInstanceOptions) =
            Nothing
          | otherwise =
            Just $ rq & dodioMarker .~ rs ^. dodiorsMarker

instance AWSRequest
           DescribeOrderableDBInstanceOptions
         where
        type Rs DescribeOrderableDBInstanceOptions =
             DescribeOrderableDBInstanceOptionsResponse
        request = postQuery rds
        response
          = receiveXMLWrapper
              "DescribeOrderableDBInstanceOptionsResult"
              (\ s h x ->
                 DescribeOrderableDBInstanceOptionsResponse' <$>
                   (x .@? "OrderableDBInstanceOptions" .!@ mempty >>=
                      may (parseXMLList "OrderableDBInstanceOption"))
                     <*> (x .@? "Marker")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeOrderableDBInstanceOptions
         where

instance NFData DescribeOrderableDBInstanceOptions
         where

instance ToHeaders DescribeOrderableDBInstanceOptions
         where
        toHeaders = const mempty

instance ToPath DescribeOrderableDBInstanceOptions
         where
        toPath = const "/"

instance ToQuery DescribeOrderableDBInstanceOptions
         where
        toQuery DescribeOrderableDBInstanceOptions'{..}
          = mconcat
              ["Action" =:
                 ("DescribeOrderableDBInstanceOptions" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "EngineVersion" =: _dodioEngineVersion,
               "Filters" =:
                 toQuery (toQueryList "Filter" <$> _dodioFilters),
               "DBInstanceClass" =: _dodioDBInstanceClass,
               "LicenseModel" =: _dodioLicenseModel,
               "Marker" =: _dodioMarker,
               "MaxRecords" =: _dodioMaxRecords, "Vpc" =: _dodioVPC,
               "Engine" =: _dodioEngine]

-- | Contains the result of a successful invocation of the 'DescribeOrderableDBInstanceOptions' action.
--
--
--
-- /See:/ 'describeOrderableDBInstanceOptionsResponse' smart constructor.
data DescribeOrderableDBInstanceOptionsResponse = DescribeOrderableDBInstanceOptionsResponse'
  { _dodiorsOrderableDBInstanceOptions :: !(Maybe [OrderableDBInstanceOption])
  , _dodiorsMarker                     :: !(Maybe Text)
  , _dodiorsResponseStatus             :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeOrderableDBInstanceOptionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dodiorsOrderableDBInstanceOptions' - An 'OrderableDBInstanceOption' structure containing information about orderable options for the DB instance.
--
-- * 'dodiorsMarker' - An optional pagination token provided by a previous OrderableDBInstanceOptions request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'dodiorsResponseStatus' - -- | The response status code.
describeOrderableDBInstanceOptionsResponse
    :: Int -- ^ 'dodiorsResponseStatus'
    -> DescribeOrderableDBInstanceOptionsResponse
describeOrderableDBInstanceOptionsResponse pResponseStatus_ =
  DescribeOrderableDBInstanceOptionsResponse'
    { _dodiorsOrderableDBInstanceOptions = Nothing
    , _dodiorsMarker = Nothing
    , _dodiorsResponseStatus = pResponseStatus_
    }


-- | An 'OrderableDBInstanceOption' structure containing information about orderable options for the DB instance.
dodiorsOrderableDBInstanceOptions :: Lens' DescribeOrderableDBInstanceOptionsResponse [OrderableDBInstanceOption]
dodiorsOrderableDBInstanceOptions = lens _dodiorsOrderableDBInstanceOptions (\ s a -> s{_dodiorsOrderableDBInstanceOptions = a}) . _Default . _Coerce

-- | An optional pagination token provided by a previous OrderableDBInstanceOptions request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
dodiorsMarker :: Lens' DescribeOrderableDBInstanceOptionsResponse (Maybe Text)
dodiorsMarker = lens _dodiorsMarker (\ s a -> s{_dodiorsMarker = a})

-- | -- | The response status code.
dodiorsResponseStatus :: Lens' DescribeOrderableDBInstanceOptionsResponse Int
dodiorsResponseStatus = lens _dodiorsResponseStatus (\ s a -> s{_dodiorsResponseStatus = a})

instance NFData
           DescribeOrderableDBInstanceOptionsResponse
         where
