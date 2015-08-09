{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeOrderableDBInstanceOptions
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of orderable DB instance options for the specified
-- engine.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DescribeOrderableDBInstanceOptions.html AWS API Reference> for DescribeOrderableDBInstanceOptions.
module Network.AWS.RDS.DescribeOrderableDBInstanceOptions
    (
    -- * Creating a Request
      DescribeOrderableDBInstanceOptions
    , describeOrderableDBInstanceOptions
    -- * Request Lenses
    , dodioEngineVersion
    , dodioFilters
    , dodioDBInstanceClass
    , dodioLicenseModel
    , dodioMaxRecords
    , dodioMarker
    , dodioVPC
    , dodioEngine

    -- * Destructuring the Response
    , DescribeOrderableDBInstanceOptionsResponse
    , describeOrderableDBInstanceOptionsResponse
    -- * Response Lenses
    , dodiorsOrderableDBInstanceOptions
    , dodiorsMarker
    , dodiorsStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.RDS.Types.Product
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'describeOrderableDBInstanceOptions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dodioEngineVersion'
--
-- * 'dodioFilters'
--
-- * 'dodioDBInstanceClass'
--
-- * 'dodioLicenseModel'
--
-- * 'dodioMaxRecords'
--
-- * 'dodioMarker'
--
-- * 'dodioVPC'
--
-- * 'dodioEngine'
data DescribeOrderableDBInstanceOptions = DescribeOrderableDBInstanceOptions'
    { _dodioEngineVersion   :: !(Maybe Text)
    , _dodioFilters         :: !(Maybe [Filter])
    , _dodioDBInstanceClass :: !(Maybe Text)
    , _dodioLicenseModel    :: !(Maybe Text)
    , _dodioMaxRecords      :: !(Maybe Int)
    , _dodioMarker          :: !(Maybe Text)
    , _dodioVPC             :: !(Maybe Bool)
    , _dodioEngine          :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeOrderableDBInstanceOptions' smart constructor.
describeOrderableDBInstanceOptions :: Text -> DescribeOrderableDBInstanceOptions
describeOrderableDBInstanceOptions pEngine_ =
    DescribeOrderableDBInstanceOptions'
    { _dodioEngineVersion = Nothing
    , _dodioFilters = Nothing
    , _dodioDBInstanceClass = Nothing
    , _dodioLicenseModel = Nothing
    , _dodioMaxRecords = Nothing
    , _dodioMarker = Nothing
    , _dodioVPC = Nothing
    , _dodioEngine = pEngine_
    }

-- | The engine version filter value. Specify this parameter to show only the
-- available offerings matching the specified engine version.
dodioEngineVersion :: Lens' DescribeOrderableDBInstanceOptions (Maybe Text)
dodioEngineVersion = lens _dodioEngineVersion (\ s a -> s{_dodioEngineVersion = a});

-- | This parameter is not currently supported.
dodioFilters :: Lens' DescribeOrderableDBInstanceOptions [Filter]
dodioFilters = lens _dodioFilters (\ s a -> s{_dodioFilters = a}) . _Default . _Coerce;

-- | The DB instance class filter value. Specify this parameter to show only
-- the available offerings matching the specified DB instance class.
dodioDBInstanceClass :: Lens' DescribeOrderableDBInstanceOptions (Maybe Text)
dodioDBInstanceClass = lens _dodioDBInstanceClass (\ s a -> s{_dodioDBInstanceClass = a});

-- | The license model filter value. Specify this parameter to show only the
-- available offerings matching the specified license model.
dodioLicenseModel :: Lens' DescribeOrderableDBInstanceOptions (Maybe Text)
dodioLicenseModel = lens _dodioLicenseModel (\ s a -> s{_dodioLicenseModel = a});

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
dodioMaxRecords :: Lens' DescribeOrderableDBInstanceOptions (Maybe Int)
dodioMaxRecords = lens _dodioMaxRecords (\ s a -> s{_dodioMaxRecords = a});

-- | An optional pagination token provided by a previous
-- DescribeOrderableDBInstanceOptions request. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@ .
dodioMarker :: Lens' DescribeOrderableDBInstanceOptions (Maybe Text)
dodioMarker = lens _dodioMarker (\ s a -> s{_dodioMarker = a});

-- | The VPC filter value. Specify this parameter to show only the available
-- VPC or non-VPC offerings.
dodioVPC :: Lens' DescribeOrderableDBInstanceOptions (Maybe Bool)
dodioVPC = lens _dodioVPC (\ s a -> s{_dodioVPC = a});

-- | The name of the engine to retrieve DB instance options for.
dodioEngine :: Lens' DescribeOrderableDBInstanceOptions Text
dodioEngine = lens _dodioEngine (\ s a -> s{_dodioEngine = a});

instance AWSPager DescribeOrderableDBInstanceOptions
         where
        page rq rs
          | stop (rs ^. dodiorsMarker) = Nothing
          | stop (rs ^. dodiorsOrderableDBInstanceOptions) =
            Nothing
          | otherwise =
            Just $ rq & dodioMarker .~ rs ^. dodiorsMarker

instance AWSRequest
         DescribeOrderableDBInstanceOptions where
        type Sv DescribeOrderableDBInstanceOptions = RDS
        type Rs DescribeOrderableDBInstanceOptions =
             DescribeOrderableDBInstanceOptionsResponse
        request = postQuery
        response
          = receiveXMLWrapper
              "DescribeOrderableDBInstanceOptionsResult"
              (\ s h x ->
                 DescribeOrderableDBInstanceOptionsResponse' <$>
                   (x .@? "OrderableDBInstanceOptions" .!@ mempty >>=
                      may (parseXMLList "OrderableDBInstanceOption"))
                     <*> (x .@? "Marker")
                     <*> (pure (fromEnum s)))

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
               "MaxRecords" =: _dodioMaxRecords,
               "Marker" =: _dodioMarker, "Vpc" =: _dodioVPC,
               "Engine" =: _dodioEngine]

-- | Contains the result of a successful invocation of the
-- DescribeOrderableDBInstanceOptions action.
--
-- /See:/ 'describeOrderableDBInstanceOptionsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dodiorsOrderableDBInstanceOptions'
--
-- * 'dodiorsMarker'
--
-- * 'dodiorsStatus'
data DescribeOrderableDBInstanceOptionsResponse = DescribeOrderableDBInstanceOptionsResponse'
    { _dodiorsOrderableDBInstanceOptions :: !(Maybe [OrderableDBInstanceOption])
    , _dodiorsMarker                     :: !(Maybe Text)
    , _dodiorsStatus                     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeOrderableDBInstanceOptionsResponse' smart constructor.
describeOrderableDBInstanceOptionsResponse :: Int -> DescribeOrderableDBInstanceOptionsResponse
describeOrderableDBInstanceOptionsResponse pStatus_ =
    DescribeOrderableDBInstanceOptionsResponse'
    { _dodiorsOrderableDBInstanceOptions = Nothing
    , _dodiorsMarker = Nothing
    , _dodiorsStatus = pStatus_
    }

-- | An OrderableDBInstanceOption structure containing information about
-- orderable options for the DB instance.
dodiorsOrderableDBInstanceOptions :: Lens' DescribeOrderableDBInstanceOptionsResponse [OrderableDBInstanceOption]
dodiorsOrderableDBInstanceOptions = lens _dodiorsOrderableDBInstanceOptions (\ s a -> s{_dodiorsOrderableDBInstanceOptions = a}) . _Default . _Coerce;

-- | An optional pagination token provided by a previous
-- OrderableDBInstanceOptions request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@ .
dodiorsMarker :: Lens' DescribeOrderableDBInstanceOptionsResponse (Maybe Text)
dodiorsMarker = lens _dodiorsMarker (\ s a -> s{_dodiorsMarker = a});

-- | Undocumented member.
dodiorsStatus :: Lens' DescribeOrderableDBInstanceOptionsResponse Int
dodiorsStatus = lens _dodiorsStatus (\ s a -> s{_dodiorsStatus = a});
