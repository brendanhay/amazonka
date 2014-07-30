{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.V2013_09_09.DescribeOptionGroupOptions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes all available options. https://rds.amazonaws.com/
-- ?Action=DescribeOptionGroupOptions &EngineName=oracle-se1
-- &MajorEngineVersion=11.2 11.2 true Oracle Enterprise Manager 1158 OEM
-- oracle-se1 0.2.v3 false false d9c8f6a1-84c7-11e1-a264-0b23c28bc344.
module Network.AWS.RDS.V2013_09_09.DescribeOptionGroupOptions where

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

-- | Minimum specification for a 'DescribeOptionGroupOptions' request.
describeOptionGroupOptions :: Text -- ^ '_dogomEngineName'
                           -> DescribeOptionGroupOptions
describeOptionGroupOptions p1 = DescribeOptionGroupOptions
    { _dogomEngineName = p1
    , _dogomMaxRecords = Nothing
    , _dogomMajorEngineVersion = Nothing
    , _dogomMarker = Nothing
    }

data DescribeOptionGroupOptions = DescribeOptionGroupOptions
    { _dogomEngineName :: Text
      -- ^ A required parameter. Options available for the given Engine name
      -- will be described.
    , _dogomMaxRecords :: Maybe Integer
      -- ^ The maximum number of records to include in the response. If more
      -- records exist than the specified MaxRecords value, a pagination
      -- token called a marker is included in the response so that the
      -- remaining results can be retrieved. Default: 100 Constraints:
      -- minimum 20, maximum 100.
    , _dogomMajorEngineVersion :: Maybe Text
      -- ^ If specified, filters the results to include only options for the
      -- specified major engine version.
    , _dogomMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous request. If
      -- this parameter is specified, the response includes only records
      -- beyond the marker, up to the value specified by MaxRecords.
    } deriving (Generic)

instance ToQuery DescribeOptionGroupOptions where
    toQuery = genericToQuery def

instance AWSRequest DescribeOptionGroupOptions where
    type Sv DescribeOptionGroupOptions = RDS
    type Rs DescribeOptionGroupOptions = DescribeOptionGroupOptionsResponse

    request = post "DescribeOptionGroupOptions"
    response _ = xmlResponse

instance AWSPager DescribeOptionGroupOptions where
    next rq rs = (\x -> rq { _dogomMarker = Just x })
        <$> _ogomMarker rs

data DescribeOptionGroupOptionsResponse = DescribeOptionGroupOptionsResponse
    { _ogomOptionGroupOptions :: [OptionGroupOption]
      -- ^ List of available option group options.
    , _ogomMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous request. If
      -- this parameter is specified, the response includes only records
      -- beyond the marker, up to the value specified by MaxRecords.
    } deriving (Generic)

instance FromXML DescribeOptionGroupOptionsResponse where
    fromXMLOptions = xmlOptions
