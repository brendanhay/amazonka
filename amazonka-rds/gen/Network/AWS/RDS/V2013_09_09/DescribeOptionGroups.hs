{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.V2013_09_09.DescribeOptionGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes the available option groups. https://rds.amazonaws.com/
-- ?Action=DescribeOptionGroups &OptionGroupName=myoptiongroup &MaxRecords=100
-- 11.2 myoptiongroup oracle-se1 Test option group
-- 6088823d-84c8-11e1-a264-0b23c28bc344 https://rds.amazonaws.com/
-- ?Action=DescribeOptionGroups &MaxRecords=100 11.2 myoptiongroup oracle-se1
-- Test option group 11.2 default:oracle-se1-11-2 oracle-se1 Default option
-- group. e4b234d9-84d5-11e1-87a6-71059839a52b.
module Network.AWS.RDS.V2013_09_09.DescribeOptionGroups
    (
    -- * Request
      DescribeOptionGroups
    -- ** Request constructor
    , mkDescribeOptionGroups
    -- ** Request lenses
    , dog1OptionGroupName
    , dog1Marker
    , dog1MaxRecords
    , dog1EngineName
    , dog1MajorEngineVersion

    -- * Response
    , DescribeOptionGroupsResponse
    -- ** Response constructor
    , mkDescribeOptionGroupsResponse
    -- ** Response lenses
    , dogrOptionGroupsList
    , dogrMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | 
data DescribeOptionGroups = DescribeOptionGroups
    { _dog1OptionGroupName :: Maybe Text
    , _dog1Marker :: Maybe Text
    , _dog1MaxRecords :: Maybe Integer
    , _dog1EngineName :: Maybe Text
    , _dog1MajorEngineVersion :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeOptionGroups' request.
mkDescribeOptionGroups :: DescribeOptionGroups
mkDescribeOptionGroups = DescribeOptionGroups
    { _dog1OptionGroupName = Nothing
    , _dog1Marker = Nothing
    , _dog1MaxRecords = Nothing
    , _dog1EngineName = Nothing
    , _dog1MajorEngineVersion = Nothing
    }

-- | The name of the option group to describe. Cannot be supplied together with
-- EngineName or MajorEngineVersion.
dog1OptionGroupName :: Lens' DescribeOptionGroups (Maybe Text)
dog1OptionGroupName =
    lens _dog1OptionGroupName (\s a -> s { _dog1OptionGroupName = a })

-- | An optional pagination token provided by a previous DescribeOptionGroups
-- request. If this parameter is specified, the response includes only records
-- beyond the marker, up to the value specified by MaxRecords.
dog1Marker :: Lens' DescribeOptionGroups (Maybe Text)
dog1Marker = lens _dog1Marker (\s a -> s { _dog1Marker = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a pagination token called a
-- marker is included in the response so that the remaining results can be
-- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
dog1MaxRecords :: Lens' DescribeOptionGroups (Maybe Integer)
dog1MaxRecords = lens _dog1MaxRecords (\s a -> s { _dog1MaxRecords = a })

-- | Filters the list of option groups to only include groups associated with a
-- specific database engine.
dog1EngineName :: Lens' DescribeOptionGroups (Maybe Text)
dog1EngineName = lens _dog1EngineName (\s a -> s { _dog1EngineName = a })

-- | Filters the list of option groups to only include groups associated with a
-- specific database engine version. If specified, then EngineName must also
-- be specified.
dog1MajorEngineVersion :: Lens' DescribeOptionGroups (Maybe Text)
dog1MajorEngineVersion =
    lens _dog1MajorEngineVersion (\s a -> s { _dog1MajorEngineVersion = a })

instance ToQuery DescribeOptionGroups where
    toQuery = genericQuery def

-- | List of option groups.
data DescribeOptionGroupsResponse = DescribeOptionGroupsResponse
    { _dogrOptionGroupsList :: [OptionGroup]
    , _dogrMarker :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeOptionGroupsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDescribeOptionGroupsResponse :: DescribeOptionGroupsResponse
mkDescribeOptionGroupsResponse = DescribeOptionGroupsResponse
    { _dogrOptionGroupsList = mempty
    , _dogrMarker = Nothing
    }

-- | List of option groups.
dogrOptionGroupsList :: Lens' DescribeOptionGroupsResponse [OptionGroup]
dogrOptionGroupsList =
    lens _dogrOptionGroupsList (\s a -> s { _dogrOptionGroupsList = a })

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by MaxRecords.
dogrMarker :: Lens' DescribeOptionGroupsResponse (Maybe Text)
dogrMarker = lens _dogrMarker (\s a -> s { _dogrMarker = a })

instance FromXML DescribeOptionGroupsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeOptionGroups where
    type Sv DescribeOptionGroups = RDS
    type Rs DescribeOptionGroups = DescribeOptionGroupsResponse

    request = post "DescribeOptionGroups"
    response _ = xmlResponse

instance AWSPager DescribeOptionGroups where
    next rq rs = (\x -> rq & dog1Marker ?~ x)
        <$> (rs ^. dogrMarker)
