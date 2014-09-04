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
    , describeOptionGroups
    -- ** Request lenses
    , dognMaxRecords
    , dognOptionGroupName
    , dognMarker
    , dognEngineName
    , dognMajorEngineVersion

    -- * Response
    , DescribeOptionGroupsResponse
    -- ** Response lenses
    , ohOptionGroupsList
    , ohMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeOptionGroups' request.
describeOptionGroups :: DescribeOptionGroups
describeOptionGroups = DescribeOptionGroups
    { _dognMaxRecords = Nothing
    , _dognOptionGroupName = Nothing
    , _dognMarker = Nothing
    , _dognEngineName = Nothing
    , _dognMajorEngineVersion = Nothing
    }
{-# INLINE describeOptionGroups #-}

data DescribeOptionGroups = DescribeOptionGroups
    { _dognMaxRecords :: Maybe Integer
      -- ^ The maximum number of records to include in the response. If more
      -- records exist than the specified MaxRecords value, a pagination
      -- token called a marker is included in the response so that the
      -- remaining results can be retrieved. Default: 100 Constraints:
      -- minimum 20, maximum 100.
    , _dognOptionGroupName :: Maybe Text
      -- ^ The name of the option group to describe. Cannot be supplied
      -- together with EngineName or MajorEngineVersion.
    , _dognMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous
      -- DescribeOptionGroups request. If this parameter is specified, the
      -- response includes only records beyond the marker, up to the value
      -- specified by MaxRecords.
    , _dognEngineName :: Maybe Text
      -- ^ Filters the list of option groups to only include groups
      -- associated with a specific database engine.
    , _dognMajorEngineVersion :: Maybe Text
      -- ^ Filters the list of option groups to only include groups
      -- associated with a specific database engine version. If specified,
      -- then EngineName must also be specified.
    } deriving (Show, Generic)

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a pagination token called a
-- marker is included in the response so that the remaining results can be
-- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
dognMaxRecords :: Lens' DescribeOptionGroups (Maybe Integer)
dognMaxRecords f x =
    f (_dognMaxRecords x)
        <&> \y -> x { _dognMaxRecords = y }
{-# INLINE dognMaxRecords #-}

-- | The name of the option group to describe. Cannot be supplied together with
-- EngineName or MajorEngineVersion.
dognOptionGroupName :: Lens' DescribeOptionGroups (Maybe Text)
dognOptionGroupName f x =
    f (_dognOptionGroupName x)
        <&> \y -> x { _dognOptionGroupName = y }
{-# INLINE dognOptionGroupName #-}

-- | An optional pagination token provided by a previous DescribeOptionGroups
-- request. If this parameter is specified, the response includes only records
-- beyond the marker, up to the value specified by MaxRecords.
dognMarker :: Lens' DescribeOptionGroups (Maybe Text)
dognMarker f x =
    f (_dognMarker x)
        <&> \y -> x { _dognMarker = y }
{-# INLINE dognMarker #-}

-- | Filters the list of option groups to only include groups associated with a
-- specific database engine.
dognEngineName :: Lens' DescribeOptionGroups (Maybe Text)
dognEngineName f x =
    f (_dognEngineName x)
        <&> \y -> x { _dognEngineName = y }
{-# INLINE dognEngineName #-}

-- | Filters the list of option groups to only include groups associated with a
-- specific database engine version. If specified, then EngineName must also
-- be specified.
dognMajorEngineVersion :: Lens' DescribeOptionGroups (Maybe Text)
dognMajorEngineVersion f x =
    f (_dognMajorEngineVersion x)
        <&> \y -> x { _dognMajorEngineVersion = y }
{-# INLINE dognMajorEngineVersion #-}

instance ToQuery DescribeOptionGroups where
    toQuery = genericQuery def

data DescribeOptionGroupsResponse = DescribeOptionGroupsResponse
    { _ohOptionGroupsList :: [OptionGroup]
      -- ^ List of option groups.
    , _ohMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous request. If
      -- this parameter is specified, the response includes only records
      -- beyond the marker, up to the value specified by MaxRecords.
    } deriving (Show, Generic)

-- | List of option groups.
ohOptionGroupsList :: Lens' DescribeOptionGroupsResponse ([OptionGroup])
ohOptionGroupsList f x =
    f (_ohOptionGroupsList x)
        <&> \y -> x { _ohOptionGroupsList = y }
{-# INLINE ohOptionGroupsList #-}

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by MaxRecords.
ohMarker :: Lens' DescribeOptionGroupsResponse (Maybe Text)
ohMarker f x =
    f (_ohMarker x)
        <&> \y -> x { _ohMarker = y }
{-# INLINE ohMarker #-}

instance FromXML DescribeOptionGroupsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeOptionGroups where
    type Sv DescribeOptionGroups = RDS
    type Rs DescribeOptionGroups = DescribeOptionGroupsResponse

    request = post "DescribeOptionGroups"
    response _ = xmlResponse

instance AWSPager DescribeOptionGroups where
    next rq rs = (\x -> rq { _dognMarker = Just x })
        <$> (_ohMarker rs)
