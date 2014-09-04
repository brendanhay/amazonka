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
    , mkDescribeOptionGroupOptionsMessage
    -- ** Request lenses
    , dogomEngineName
    , dogomMajorEngineVersion
    , dogomMaxRecords
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

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeOptionGroupOptions' request.
mkDescribeOptionGroupOptionsMessage :: Text -- ^ 'dogomEngineName'
                                    -> DescribeOptionGroupOptions
mkDescribeOptionGroupOptionsMessage p1 = DescribeOptionGroupOptions
    { _dogomEngineName = p1
    , _dogomMajorEngineVersion = Nothing
    , _dogomMaxRecords = Nothing
    , _dogomMarker = Nothing
    }
{-# INLINE mkDescribeOptionGroupOptionsMessage #-}

data DescribeOptionGroupOptions = DescribeOptionGroupOptions
    { _dogomEngineName :: Text
      -- ^ A required parameter. Options available for the given Engine name
      -- will be described.
    , _dogomMajorEngineVersion :: Maybe Text
      -- ^ If specified, filters the results to include only options for the
      -- specified major engine version.
    , _dogomMaxRecords :: Maybe Integer
      -- ^ The maximum number of records to include in the response. If more
      -- records exist than the specified MaxRecords value, a pagination
      -- token called a marker is included in the response so that the
      -- remaining results can be retrieved. Default: 100 Constraints:
      -- minimum 20, maximum 100.
    , _dogomMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous request. If
      -- this parameter is specified, the response includes only records
      -- beyond the marker, up to the value specified by MaxRecords.
    } deriving (Show, Generic)

-- | A required parameter. Options available for the given Engine name will be
-- described.
dogomEngineName :: Lens' DescribeOptionGroupOptions (Text)
dogomEngineName = lens _dogomEngineName (\s a -> s { _dogomEngineName = a })
{-# INLINE dogomEngineName #-}

-- | If specified, filters the results to include only options for the specified
-- major engine version.
dogomMajorEngineVersion :: Lens' DescribeOptionGroupOptions (Maybe Text)
dogomMajorEngineVersion = lens _dogomMajorEngineVersion (\s a -> s { _dogomMajorEngineVersion = a })
{-# INLINE dogomMajorEngineVersion #-}

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a pagination token called a
-- marker is included in the response so that the remaining results can be
-- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
dogomMaxRecords :: Lens' DescribeOptionGroupOptions (Maybe Integer)
dogomMaxRecords = lens _dogomMaxRecords (\s a -> s { _dogomMaxRecords = a })
{-# INLINE dogomMaxRecords #-}

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by MaxRecords.
dogomMarker :: Lens' DescribeOptionGroupOptions (Maybe Text)
dogomMarker = lens _dogomMarker (\s a -> s { _dogomMarker = a })
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
ogomOptionGroupOptions :: Lens' DescribeOptionGroupOptionsResponse ([OptionGroupOption])
ogomOptionGroupOptions = lens _ogomOptionGroupOptions (\s a -> s { _ogomOptionGroupOptions = a })
{-# INLINE ogomOptionGroupOptions #-}

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by MaxRecords.
ogomMarker :: Lens' DescribeOptionGroupOptionsResponse (Maybe Text)
ogomMarker = lens _ogomMarker (\s a -> s { _ogomMarker = a })
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
