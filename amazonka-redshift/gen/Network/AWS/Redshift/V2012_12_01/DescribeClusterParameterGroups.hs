{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.DescribeClusterParameterGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of Amazon Redshift parameter groups, including parameter
-- groups you created and the default parameter group. For each parameter
-- group, the response includes the parameter group name, description, and
-- parameter group family name. You can optionally specify a name to retrieve
-- the description of a specific parameter group. For more information about
-- managing parameter groups, go to Amazon Redshift Parameter Groups in the
-- Amazon Redshift Management Guide. https://redshift.us-east-1.amazonaws.com/
-- ?Action=DescribeClusterParameterGroups &Version=2012-12-01
-- &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130123/us-east-1/redshift/aws4_request
-- &x-amz-date=20130123T004002Z
-- &x-amz-signedheaders=content-type;host;x-amz-date redshift-1.0 Default
-- parameter group for redshift-1.0 default.redshift-1.0 redshift-1.0
-- description my parameter group parametergroup1
-- 6d28788b-64f5-11e2-b343-393adc3f0a21.
module Network.AWS.Redshift.V2012_12_01.DescribeClusterParameterGroups
    (
    -- * Request
      DescribeClusterParameterGroups
    -- ** Request constructor
    , describeClusterParameterGroups
    -- ** Request lenses
    , dcpgnMaxRecords
    , dcpgnParameterGroupName
    , dcpgnMarker

    -- * Response
    , DescribeClusterParameterGroupsResponse
    -- ** Response lenses
    , cpgmParameterGroups
    , cpgmMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeClusterParameterGroups' request.
describeClusterParameterGroups :: DescribeClusterParameterGroups
describeClusterParameterGroups = DescribeClusterParameterGroups
    { _dcpgnMaxRecords = Nothing
    , _dcpgnParameterGroupName = Nothing
    , _dcpgnMarker = Nothing
    }
{-# INLINE describeClusterParameterGroups #-}

data DescribeClusterParameterGroups = DescribeClusterParameterGroups
    { _dcpgnMaxRecords :: Maybe Integer
      -- ^ The maximum number of response records to return in each call. If
      -- the number of remaining response records exceeds the specified
      -- MaxRecords value, a value is returned in a marker field of the
      -- response. You can retrieve the next set of records by retrying
      -- the command with the returned marker value. Default: 100
      -- Constraints: minimum 20, maximum 100.
    , _dcpgnParameterGroupName :: Maybe Text
      -- ^ The name of a specific parameter group for which to return
      -- details. By default, details about all parameter groups and the
      -- default parameter group are returned.
    , _dcpgnMarker :: Maybe Text
      -- ^ An optional parameter that specifies the starting point to return
      -- a set of response records. When the results of a
      -- DescribeClusterParameterGroups request exceed the value specified
      -- in MaxRecords, AWS returns a value in the Marker field of the
      -- response. You can retrieve the next set of response records by
      -- providing the returned marker value in the Marker parameter and
      -- retrying the request.
    } deriving (Show, Generic)

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified MaxRecords
-- value, a value is returned in a marker field of the response. You can
-- retrieve the next set of records by retrying the command with the returned
-- marker value. Default: 100 Constraints: minimum 20, maximum 100.
dcpgnMaxRecords :: Lens' DescribeClusterParameterGroups (Maybe Integer)
dcpgnMaxRecords f x =
    f (_dcpgnMaxRecords x)
        <&> \y -> x { _dcpgnMaxRecords = y }
{-# INLINE dcpgnMaxRecords #-}

-- | The name of a specific parameter group for which to return details. By
-- default, details about all parameter groups and the default parameter group
-- are returned.
dcpgnParameterGroupName :: Lens' DescribeClusterParameterGroups (Maybe Text)
dcpgnParameterGroupName f x =
    f (_dcpgnParameterGroupName x)
        <&> \y -> x { _dcpgnParameterGroupName = y }
{-# INLINE dcpgnParameterGroupName #-}

-- | An optional parameter that specifies the starting point to return a set of
-- response records. When the results of a DescribeClusterParameterGroups
-- request exceed the value specified in MaxRecords, AWS returns a value in
-- the Marker field of the response. You can retrieve the next set of response
-- records by providing the returned marker value in the Marker parameter and
-- retrying the request.
dcpgnMarker :: Lens' DescribeClusterParameterGroups (Maybe Text)
dcpgnMarker f x =
    f (_dcpgnMarker x)
        <&> \y -> x { _dcpgnMarker = y }
{-# INLINE dcpgnMarker #-}

instance ToQuery DescribeClusterParameterGroups where
    toQuery = genericQuery def

data DescribeClusterParameterGroupsResponse = DescribeClusterParameterGroupsResponse
    { _cpgmParameterGroups :: [ClusterParameterGroup]
      -- ^ A list of ClusterParameterGroup instances. Each instance
      -- describes one cluster parameter group.
    , _cpgmMarker :: Maybe Text
      -- ^ A value that indicates the starting point for the next set of
      -- response records in a subsequent request. If a value is returned
      -- in a response, you can retrieve the next set of records by
      -- providing this returned marker value in the Marker parameter and
      -- retrying the command. If the Marker field is empty, all response
      -- records have been retrieved for the request.
    } deriving (Show, Generic)

-- | A list of ClusterParameterGroup instances. Each instance describes one
-- cluster parameter group.
cpgmParameterGroups :: Lens' DescribeClusterParameterGroupsResponse ([ClusterParameterGroup])
cpgmParameterGroups f x =
    f (_cpgmParameterGroups x)
        <&> \y -> x { _cpgmParameterGroups = y }
{-# INLINE cpgmParameterGroups #-}

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response, you
-- can retrieve the next set of records by providing this returned marker
-- value in the Marker parameter and retrying the command. If the Marker field
-- is empty, all response records have been retrieved for the request.
cpgmMarker :: Lens' DescribeClusterParameterGroupsResponse (Maybe Text)
cpgmMarker f x =
    f (_cpgmMarker x)
        <&> \y -> x { _cpgmMarker = y }
{-# INLINE cpgmMarker #-}

instance FromXML DescribeClusterParameterGroupsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeClusterParameterGroups where
    type Sv DescribeClusterParameterGroups = Redshift
    type Rs DescribeClusterParameterGroups = DescribeClusterParameterGroupsResponse

    request = post "DescribeClusterParameterGroups"
    response _ = xmlResponse

instance AWSPager DescribeClusterParameterGroups where
    next rq rs = (\x -> rq { _dcpgnMarker = Just x })
        <$> (_cpgmMarker rs)
