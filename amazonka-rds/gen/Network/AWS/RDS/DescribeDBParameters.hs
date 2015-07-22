{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeDBParameters
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns the detailed parameter list for a particular DB parameter group.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DescribeDBParameters.html>
module Network.AWS.RDS.DescribeDBParameters
    (
    -- * Request
      DescribeDBParameters
    -- ** Request constructor
    , describeDBParameters
    -- ** Request lenses
    , ddprqFilters
    , ddprqMaxRecords
    , ddprqMarker
    , ddprqSource
    , ddprqDBParameterGroupName

    -- * Response
    , DescribeDBParametersResponse
    -- ** Response constructor
    , describeDBParametersResponse
    -- ** Response lenses
    , ddprsParameters
    , ddprsMarker
    , ddprsStatus
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
-- * 'ddprqFilters'
--
-- * 'ddprqMaxRecords'
--
-- * 'ddprqMarker'
--
-- * 'ddprqSource'
--
-- * 'ddprqDBParameterGroupName'
data DescribeDBParameters = DescribeDBParameters'
    { _ddprqFilters              :: !(Maybe [Filter])
    , _ddprqMaxRecords           :: !(Maybe Int)
    , _ddprqMarker               :: !(Maybe Text)
    , _ddprqSource               :: !(Maybe Text)
    , _ddprqDBParameterGroupName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeDBParameters' smart constructor.
describeDBParameters :: Text -> DescribeDBParameters
describeDBParameters pDBParameterGroupName =
    DescribeDBParameters'
    { _ddprqFilters = Nothing
    , _ddprqMaxRecords = Nothing
    , _ddprqMarker = Nothing
    , _ddprqSource = Nothing
    , _ddprqDBParameterGroupName = pDBParameterGroupName
    }

-- | This parameter is not currently supported.
ddprqFilters :: Lens' DescribeDBParameters [Filter]
ddprqFilters = lens _ddprqFilters (\ s a -> s{_ddprqFilters = a}) . _Default;

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results may be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20, maximum 100
ddprqMaxRecords :: Lens' DescribeDBParameters (Maybe Int)
ddprqMaxRecords = lens _ddprqMaxRecords (\ s a -> s{_ddprqMaxRecords = a});

-- | An optional pagination token provided by a previous
-- @DescribeDBParameters@ request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
ddprqMarker :: Lens' DescribeDBParameters (Maybe Text)
ddprqMarker = lens _ddprqMarker (\ s a -> s{_ddprqMarker = a});

-- | The parameter types to return.
--
-- Default: All parameter types returned
--
-- Valid Values: @user | system | engine-default@
ddprqSource :: Lens' DescribeDBParameters (Maybe Text)
ddprqSource = lens _ddprqSource (\ s a -> s{_ddprqSource = a});

-- | The name of a specific DB parameter group to return details for.
--
-- Constraints:
--
-- -   Must be 1 to 255 alphanumeric characters
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
ddprqDBParameterGroupName :: Lens' DescribeDBParameters Text
ddprqDBParameterGroupName = lens _ddprqDBParameterGroupName (\ s a -> s{_ddprqDBParameterGroupName = a});

instance AWSPager DescribeDBParameters where
        page rq rs
          | stop (rs ^. ddprsMarker) = Nothing
          | stop (rs ^. ddprsParameters) = Nothing
          | otherwise =
            Just $ rq & ddprqMarker .~ rs ^. ddprsMarker

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
                     <*> (pure (fromEnum s)))

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
                 toQuery (toQueryList "Filter" <$> _ddprqFilters),
               "MaxRecords" =: _ddprqMaxRecords,
               "Marker" =: _ddprqMarker, "Source" =: _ddprqSource,
               "DBParameterGroupName" =: _ddprqDBParameterGroupName]

-- | Contains the result of a successful invocation of the
-- DescribeDBParameters action.
--
-- /See:/ 'describeDBParametersResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddprsParameters'
--
-- * 'ddprsMarker'
--
-- * 'ddprsStatus'
data DescribeDBParametersResponse = DescribeDBParametersResponse'
    { _ddprsParameters :: !(Maybe [Parameter])
    , _ddprsMarker     :: !(Maybe Text)
    , _ddprsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeDBParametersResponse' smart constructor.
describeDBParametersResponse :: Int -> DescribeDBParametersResponse
describeDBParametersResponse pStatus =
    DescribeDBParametersResponse'
    { _ddprsParameters = Nothing
    , _ddprsMarker = Nothing
    , _ddprsStatus = pStatus
    }

-- | A list of Parameter values.
ddprsParameters :: Lens' DescribeDBParametersResponse [Parameter]
ddprsParameters = lens _ddprsParameters (\ s a -> s{_ddprsParameters = a}) . _Default;

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
ddprsMarker :: Lens' DescribeDBParametersResponse (Maybe Text)
ddprsMarker = lens _ddprsMarker (\ s a -> s{_ddprsMarker = a});

-- | FIXME: Undocumented member.
ddprsStatus :: Lens' DescribeDBParametersResponse Int
ddprsStatus = lens _ddprsStatus (\ s a -> s{_ddprsStatus = a});
