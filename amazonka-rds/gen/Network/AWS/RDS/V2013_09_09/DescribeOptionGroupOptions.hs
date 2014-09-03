{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
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
module Network.AWS.RDS.V2013_09_09.DescribeOptionGroupOptions
    (
    -- * Request
      DescribeOptionGroupOptions
    -- ** Request constructor
    , describeOptionGroupOptions
    -- ** Request lenses
    , dogomEngineName
    , dogomMaxRecords
    , dogomMajorEngineVersion
    , dogomMarker

    -- * Response
    , DescribeOptionGroupOptionsResponse
    -- ** Response lenses
    , ogomOptionGroupOptions
    , ogomMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeOptionGroupOptions' request.
describeOptionGroupOptions :: Text -- ^ 'dogomEngineName'
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
    } deriving (Show, Generic)

-- | A required parameter. Options available for the given Engine name will be
-- described.
dogomEngineName
    :: Functor f
    => (Text
    -> f (Text))
    -> DescribeOptionGroupOptions
    -> f DescribeOptionGroupOptions
dogomEngineName f x =
    (\y -> x { _dogomEngineName = y })
       <$> f (_dogomEngineName x)
{-# INLINE dogomEngineName #-}

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a pagination token called a
-- marker is included in the response so that the remaining results can be
-- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
dogomMaxRecords
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DescribeOptionGroupOptions
    -> f DescribeOptionGroupOptions
dogomMaxRecords f x =
    (\y -> x { _dogomMaxRecords = y })
       <$> f (_dogomMaxRecords x)
{-# INLINE dogomMaxRecords #-}

-- | If specified, filters the results to include only options for the specified
-- major engine version.
dogomMajorEngineVersion
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeOptionGroupOptions
    -> f DescribeOptionGroupOptions
dogomMajorEngineVersion f x =
    (\y -> x { _dogomMajorEngineVersion = y })
       <$> f (_dogomMajorEngineVersion x)
{-# INLINE dogomMajorEngineVersion #-}

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by MaxRecords.
dogomMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeOptionGroupOptions
    -> f DescribeOptionGroupOptions
dogomMarker f x =
    (\y -> x { _dogomMarker = y })
       <$> f (_dogomMarker x)
{-# INLINE dogomMarker #-}

instance ToQuery DescribeOptionGroupOptions where
    toQuery = genericQuery def

data DescribeOptionGroupOptionsResponse = DescribeOptionGroupOptionsResponse
    { _ogomOptionGroupOptions :: [OptionGroupOption]
      -- ^ List of available option group options.
    , _ogomMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous request. If
      -- this parameter is specified, the response includes only records
      -- beyond the marker, up to the value specified by MaxRecords.
    } deriving (Show, Generic)

-- | List of available option group options.
ogomOptionGroupOptions
    :: Functor f
    => ([OptionGroupOption]
    -> f ([OptionGroupOption]))
    -> DescribeOptionGroupOptionsResponse
    -> f DescribeOptionGroupOptionsResponse
ogomOptionGroupOptions f x =
    (\y -> x { _ogomOptionGroupOptions = y })
       <$> f (_ogomOptionGroupOptions x)
{-# INLINE ogomOptionGroupOptions #-}

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by MaxRecords.
ogomMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeOptionGroupOptionsResponse
    -> f DescribeOptionGroupOptionsResponse
ogomMarker f x =
    (\y -> x { _ogomMarker = y })
       <$> f (_ogomMarker x)
{-# INLINE ogomMarker #-}

instance FromXML DescribeOptionGroupOptionsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeOptionGroupOptions where
    type Sv DescribeOptionGroupOptions = RDS
    type Rs DescribeOptionGroupOptions = DescribeOptionGroupOptionsResponse

    request = post "DescribeOptionGroupOptions"
    response _ = xmlResponse

instance AWSPager DescribeOptionGroupOptions where
    next rq rs = (\x -> rq { _dogomMarker = Just x })
        <$> (_ogomMarker rs)
