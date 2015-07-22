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
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of orderable DB instance options for the specified
-- engine.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DescribeOrderableDBInstanceOptions.html>
module Network.AWS.RDS.DescribeOrderableDBInstanceOptions
    (
    -- * Request
      DescribeOrderableDBInstanceOptions
    -- ** Request constructor
    , describeOrderableDBInstanceOptions
    -- ** Request lenses
    , dodiorqEngineVersion
    , dodiorqFilters
    , dodiorqDBInstanceClass
    , dodiorqLicenseModel
    , dodiorqMaxRecords
    , dodiorqMarker
    , dodiorqVPC
    , dodiorqEngine

    -- * Response
    , DescribeOrderableDBInstanceOptionsResponse
    -- ** Response constructor
    , describeOrderableDBInstanceOptionsResponse
    -- ** Response lenses
    , dodiorsOrderableDBInstanceOptions
    , dodiorsMarker
    , dodiorsStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'describeOrderableDBInstanceOptions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dodiorqEngineVersion'
--
-- * 'dodiorqFilters'
--
-- * 'dodiorqDBInstanceClass'
--
-- * 'dodiorqLicenseModel'
--
-- * 'dodiorqMaxRecords'
--
-- * 'dodiorqMarker'
--
-- * 'dodiorqVPC'
--
-- * 'dodiorqEngine'
data DescribeOrderableDBInstanceOptions = DescribeOrderableDBInstanceOptions'
    { _dodiorqEngineVersion   :: !(Maybe Text)
    , _dodiorqFilters         :: !(Maybe [Filter])
    , _dodiorqDBInstanceClass :: !(Maybe Text)
    , _dodiorqLicenseModel    :: !(Maybe Text)
    , _dodiorqMaxRecords      :: !(Maybe Int)
    , _dodiorqMarker          :: !(Maybe Text)
    , _dodiorqVPC             :: !(Maybe Bool)
    , _dodiorqEngine          :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeOrderableDBInstanceOptions' smart constructor.
describeOrderableDBInstanceOptions :: Text -> DescribeOrderableDBInstanceOptions
describeOrderableDBInstanceOptions pEngine =
    DescribeOrderableDBInstanceOptions'
    { _dodiorqEngineVersion = Nothing
    , _dodiorqFilters = Nothing
    , _dodiorqDBInstanceClass = Nothing
    , _dodiorqLicenseModel = Nothing
    , _dodiorqMaxRecords = Nothing
    , _dodiorqMarker = Nothing
    , _dodiorqVPC = Nothing
    , _dodiorqEngine = pEngine
    }

-- | The engine version filter value. Specify this parameter to show only the
-- available offerings matching the specified engine version.
dodiorqEngineVersion :: Lens' DescribeOrderableDBInstanceOptions (Maybe Text)
dodiorqEngineVersion = lens _dodiorqEngineVersion (\ s a -> s{_dodiorqEngineVersion = a});

-- | This parameter is not currently supported.
dodiorqFilters :: Lens' DescribeOrderableDBInstanceOptions [Filter]
dodiorqFilters = lens _dodiorqFilters (\ s a -> s{_dodiorqFilters = a}) . _Default;

-- | The DB instance class filter value. Specify this parameter to show only
-- the available offerings matching the specified DB instance class.
dodiorqDBInstanceClass :: Lens' DescribeOrderableDBInstanceOptions (Maybe Text)
dodiorqDBInstanceClass = lens _dodiorqDBInstanceClass (\ s a -> s{_dodiorqDBInstanceClass = a});

-- | The license model filter value. Specify this parameter to show only the
-- available offerings matching the specified license model.
dodiorqLicenseModel :: Lens' DescribeOrderableDBInstanceOptions (Maybe Text)
dodiorqLicenseModel = lens _dodiorqLicenseModel (\ s a -> s{_dodiorqLicenseModel = a});

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20, maximum 100
dodiorqMaxRecords :: Lens' DescribeOrderableDBInstanceOptions (Maybe Int)
dodiorqMaxRecords = lens _dodiorqMaxRecords (\ s a -> s{_dodiorqMaxRecords = a});

-- | An optional pagination token provided by a previous
-- DescribeOrderableDBInstanceOptions request. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@ .
dodiorqMarker :: Lens' DescribeOrderableDBInstanceOptions (Maybe Text)
dodiorqMarker = lens _dodiorqMarker (\ s a -> s{_dodiorqMarker = a});

-- | The VPC filter value. Specify this parameter to show only the available
-- VPC or non-VPC offerings.
dodiorqVPC :: Lens' DescribeOrderableDBInstanceOptions (Maybe Bool)
dodiorqVPC = lens _dodiorqVPC (\ s a -> s{_dodiorqVPC = a});

-- | The name of the engine to retrieve DB instance options for.
dodiorqEngine :: Lens' DescribeOrderableDBInstanceOptions Text
dodiorqEngine = lens _dodiorqEngine (\ s a -> s{_dodiorqEngine = a});

instance AWSPager DescribeOrderableDBInstanceOptions
         where
        page rq rs
          | stop (rs ^. dodiorsMarker) = Nothing
          | stop (rs ^. dodiorsOrderableDBInstanceOptions) =
            Nothing
          | otherwise =
            Just $ rq & dodiorqMarker .~ rs ^. dodiorsMarker

instance AWSRequest
         DescribeOrderableDBInstanceOptions where
        type Sv DescribeOrderableDBInstanceOptions = RDS
        type Rs DescribeOrderableDBInstanceOptions =
             DescribeOrderableDBInstanceOptionsResponse
        request = post
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
               "EngineVersion" =: _dodiorqEngineVersion,
               "Filters" =:
                 toQuery (toQueryList "Filter" <$> _dodiorqFilters),
               "DBInstanceClass" =: _dodiorqDBInstanceClass,
               "LicenseModel" =: _dodiorqLicenseModel,
               "MaxRecords" =: _dodiorqMaxRecords,
               "Marker" =: _dodiorqMarker, "Vpc" =: _dodiorqVPC,
               "Engine" =: _dodiorqEngine]

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
describeOrderableDBInstanceOptionsResponse pStatus =
    DescribeOrderableDBInstanceOptionsResponse'
    { _dodiorsOrderableDBInstanceOptions = Nothing
    , _dodiorsMarker = Nothing
    , _dodiorsStatus = pStatus
    }

-- | An OrderableDBInstanceOption structure containing information about
-- orderable options for the DB instance.
dodiorsOrderableDBInstanceOptions :: Lens' DescribeOrderableDBInstanceOptionsResponse [OrderableDBInstanceOption]
dodiorsOrderableDBInstanceOptions = lens _dodiorsOrderableDBInstanceOptions (\ s a -> s{_dodiorsOrderableDBInstanceOptions = a}) . _Default;

-- | An optional pagination token provided by a previous
-- OrderableDBInstanceOptions request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@ .
dodiorsMarker :: Lens' DescribeOrderableDBInstanceOptionsResponse (Maybe Text)
dodiorsMarker = lens _dodiorsMarker (\ s a -> s{_dodiorsMarker = a});

-- | FIXME: Undocumented member.
dodiorsStatus :: Lens' DescribeOrderableDBInstanceOptionsResponse Int
dodiorsStatus = lens _dodiorsStatus (\ s a -> s{_dodiorsStatus = a});
