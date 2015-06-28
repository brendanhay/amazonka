{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.RDS.DescribeDBParameters
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns the detailed parameter list for a particular DB parameter group.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DescribeDBParameters.html>
module Network.AWS.RDS.DescribeDBParameters
    (
    -- * Request
      DescribeDBParameters
    -- ** Request constructor
    , describeDBParameters
    -- ** Request lenses
    , ddpFilters
    , ddpMaxRecords
    , ddpMarker
    , ddpSource
    , ddpDBParameterGroupName

    -- * Response
    , DescribeDBParametersResponse
    -- ** Response constructor
    , describeDBParametersResponse
    -- ** Response lenses
    , ddprParameters
    , ddprMarker
    , ddprStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeDBParameters' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddpFilters'
--
-- * 'ddpMaxRecords'
--
-- * 'ddpMarker'
--
-- * 'ddpSource'
--
-- * 'ddpDBParameterGroupName'
data DescribeDBParameters = DescribeDBParameters'
    { _ddpFilters              :: !(Maybe [Filter])
    , _ddpMaxRecords           :: !(Maybe Int)
    , _ddpMarker               :: !(Maybe Text)
    , _ddpSource               :: !(Maybe Text)
    , _ddpDBParameterGroupName :: !Text
    } deriving (Eq,Read,Show)

-- | 'DescribeDBParameters' smart constructor.
describeDBParameters :: Text -> DescribeDBParameters
describeDBParameters pDBParameterGroupName =
    DescribeDBParameters'
    { _ddpFilters = Nothing
    , _ddpMaxRecords = Nothing
    , _ddpMarker = Nothing
    , _ddpSource = Nothing
    , _ddpDBParameterGroupName = pDBParameterGroupName
    }

-- | This parameter is not currently supported.
ddpFilters :: Lens' DescribeDBParameters [Filter]
ddpFilters = lens _ddpFilters (\ s a -> s{_ddpFilters = a}) . _Default;

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results may be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20, maximum 100
ddpMaxRecords :: Lens' DescribeDBParameters (Maybe Int)
ddpMaxRecords = lens _ddpMaxRecords (\ s a -> s{_ddpMaxRecords = a});

-- | An optional pagination token provided by a previous
-- @DescribeDBParameters@ request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
ddpMarker :: Lens' DescribeDBParameters (Maybe Text)
ddpMarker = lens _ddpMarker (\ s a -> s{_ddpMarker = a});

-- | The parameter types to return.
--
-- Default: All parameter types returned
--
-- Valid Values: @user | system | engine-default@
ddpSource :: Lens' DescribeDBParameters (Maybe Text)
ddpSource = lens _ddpSource (\ s a -> s{_ddpSource = a});

-- | The name of a specific DB parameter group to return details for.
--
-- Constraints:
--
-- -   Must be 1 to 255 alphanumeric characters
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
ddpDBParameterGroupName :: Lens' DescribeDBParameters Text
ddpDBParameterGroupName = lens _ddpDBParameterGroupName (\ s a -> s{_ddpDBParameterGroupName = a});

instance AWSPager DescribeDBParameters where
        page rq rs
          | stop (rs ^. ddprMarker) = Nothing
          | stop (rs ^. ddprParameters) = Nothing
          | otherwise =
            Just $ rq & ddpMarker .~ rs ^. ddprMarker

instance AWSRequest DescribeDBParameters where
        type Sv DescribeDBParameters = RDS
        type Rs DescribeDBParameters =
             DescribeDBParametersResponse
        request = post
        response
          = receiveXMLWrapper "DescribeDBParametersResult"
              (\ s h x ->
                 DescribeDBParametersResponse' <$>
                   (x .@? "Parameters" .!@ mempty >>=
                      may (parseXMLList "Parameter"))
                     <*> (x .@? "Marker")
                     <*> (pure s))

instance ToHeaders DescribeDBParameters where
        toHeaders = const mempty

instance ToPath DescribeDBParameters where
        toPath = const "/"

instance ToQuery DescribeDBParameters where
        toQuery DescribeDBParameters'{..}
          = mconcat
              ["Action" =: ("DescribeDBParameters" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "Filters" =:
                 toQuery (toQueryList "Filter" <$> _ddpFilters),
               "MaxRecords" =: _ddpMaxRecords,
               "Marker" =: _ddpMarker, "Source" =: _ddpSource,
               "DBParameterGroupName" =: _ddpDBParameterGroupName]

-- | Contains the result of a successful invocation of the
-- DescribeDBParameters action.
--
-- /See:/ 'describeDBParametersResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddprParameters'
--
-- * 'ddprMarker'
--
-- * 'ddprStatus'
data DescribeDBParametersResponse = DescribeDBParametersResponse'
    { _ddprParameters :: !(Maybe [Parameter])
    , _ddprMarker     :: !(Maybe Text)
    , _ddprStatus     :: !Status
    } deriving (Eq,Show)

-- | 'DescribeDBParametersResponse' smart constructor.
describeDBParametersResponse :: Status -> DescribeDBParametersResponse
describeDBParametersResponse pStatus =
    DescribeDBParametersResponse'
    { _ddprParameters = Nothing
    , _ddprMarker = Nothing
    , _ddprStatus = pStatus
    }

-- | A list of Parameter values.
ddprParameters :: Lens' DescribeDBParametersResponse [Parameter]
ddprParameters = lens _ddprParameters (\ s a -> s{_ddprParameters = a}) . _Default;

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
ddprMarker :: Lens' DescribeDBParametersResponse (Maybe Text)
ddprMarker = lens _ddprMarker (\ s a -> s{_ddprMarker = a});

-- | FIXME: Undocumented member.
ddprStatus :: Lens' DescribeDBParametersResponse Status
ddprStatus = lens _ddprStatus (\ s a -> s{_ddprStatus = a});
