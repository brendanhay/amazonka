{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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
module Network.AWS.RDS.V2013_09_09.DescribeOptionGroups where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error, Endpoint, Region)
import           Network.AWS.Request.Query
import           Network.AWS.RDS.V2013_09_09.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

-- | Minimum specification for a 'DescribeOptionGroups' request.
describeOptionGroups :: DescribeOptionGroups
describeOptionGroups = DescribeOptionGroups
    { _dogmMaxRecords = Nothing
    , _dogmEngineName = Nothing
    , _dogmMajorEngineVersion = Nothing
    , _dogmMarker = Nothing
    , _dogmOptionGroupName = Nothing
    }

data DescribeOptionGroups = DescribeOptionGroups
    { _dogmMaxRecords :: Maybe Integer
      -- ^ The maximum number of records to include in the response. If more
      -- records exist than the specified MaxRecords value, a pagination
      -- token called a marker is included in the response so that the
      -- remaining results can be retrieved. Default: 100 Constraints:
      -- minimum 20, maximum 100.
    , _dogmEngineName :: Maybe Text
      -- ^ Filters the list of option groups to only include groups
      -- associated with a specific database engine.
    , _dogmMajorEngineVersion :: Maybe Text
      -- ^ Filters the list of option groups to only include groups
      -- associated with a specific database engine version. If specified,
      -- then EngineName must also be specified.
    , _dogmMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous
      -- DescribeOptionGroups request. If this parameter is specified, the
      -- response includes only records beyond the marker, up to the value
      -- specified by MaxRecords.
    , _dogmOptionGroupName :: Maybe Text
      -- ^ The name of the option group to describe. Cannot be supplied
      -- together with EngineName or MajorEngineVersion.
    } deriving (Generic)

instance ToQuery DescribeOptionGroups where
    toQuery = genericToQuery def

instance AWSRequest DescribeOptionGroups where
    type Sv DescribeOptionGroups = RDS
    type Rs DescribeOptionGroups = DescribeOptionGroupsResponse

    request = post "DescribeOptionGroups"
    response _ = xmlResponse

instance AWSPager DescribeOptionGroups where
    next rq rs = (\x -> rq { _dogmMarker = Just x })
        <$> _ogMarker rs

data DescribeOptionGroupsResponse = DescribeOptionGroupsResponse
    { _ogOptionGroupsList :: [OptionGroup]
      -- ^ List of option groups.
    , _ogMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous request. If
      -- this parameter is specified, the response includes only records
      -- beyond the marker, up to the value specified by MaxRecords.
    } deriving (Generic)

instance FromXML DescribeOptionGroupsResponse where
    fromXMLOptions = xmlOptions
