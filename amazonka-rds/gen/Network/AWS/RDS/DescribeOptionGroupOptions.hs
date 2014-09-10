{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.DescribeOptionGroupOptions
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
module Network.AWS.RDS.DescribeOptionGroupOptions
    (
    -- * Request
      DescribeOptionGroupOptions
    -- ** Request constructor
    , mkDescribeOptionGroupOptions
    -- ** Request lenses
    , dogoEngineName
    , dogoMajorEngineVersion
    , dogoMaxRecords
    , dogoMarker

    -- * Response
    , DescribeOptionGroupOptionsResponse
    -- ** Response constructor
    , mkDescribeOptionGroupOptionsResponse
    -- ** Response lenses
    , dogorOptionGroupOptions
    , dogorMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import Network.AWS.Prelude

-- | 
data DescribeOptionGroupOptions = DescribeOptionGroupOptions
    { _dogoEngineName :: !Text
    , _dogoMajorEngineVersion :: !(Maybe Text)
    , _dogoMaxRecords :: !(Maybe Integer)
    , _dogoMarker :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeOptionGroupOptions' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @EngineName ::@ @Text@
--
-- * @MajorEngineVersion ::@ @Maybe Text@
--
-- * @MaxRecords ::@ @Maybe Integer@
--
-- * @Marker ::@ @Maybe Text@
--
mkDescribeOptionGroupOptions :: Text -- ^ 'dogoEngineName'
                             -> DescribeOptionGroupOptions
mkDescribeOptionGroupOptions p1 = DescribeOptionGroupOptions
    { _dogoEngineName = p1
    , _dogoMajorEngineVersion = Nothing
    , _dogoMaxRecords = Nothing
    , _dogoMarker = Nothing
    }

-- | A required parameter. Options available for the given Engine name will be
-- described.
dogoEngineName :: Lens' DescribeOptionGroupOptions Text
dogoEngineName = lens _dogoEngineName (\s a -> s { _dogoEngineName = a })

-- | If specified, filters the results to include only options for the specified
-- major engine version.
dogoMajorEngineVersion :: Lens' DescribeOptionGroupOptions (Maybe Text)
dogoMajorEngineVersion =
    lens _dogoMajorEngineVersion (\s a -> s { _dogoMajorEngineVersion = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a pagination token called a
-- marker is included in the response so that the remaining results can be
-- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
dogoMaxRecords :: Lens' DescribeOptionGroupOptions (Maybe Integer)
dogoMaxRecords = lens _dogoMaxRecords (\s a -> s { _dogoMaxRecords = a })

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by MaxRecords.
dogoMarker :: Lens' DescribeOptionGroupOptions (Maybe Text)
dogoMarker = lens _dogoMarker (\s a -> s { _dogoMarker = a })

instance ToQuery DescribeOptionGroupOptions where
    toQuery = genericQuery def

-- | 
data DescribeOptionGroupOptionsResponse = DescribeOptionGroupOptionsResponse
    { _dogorOptionGroupOptions :: [OptionGroupOption]
    , _dogorMarker :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeOptionGroupOptionsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @OptionGroupOptions ::@ @[OptionGroupOption]@
--
-- * @Marker ::@ @Maybe Text@
--
mkDescribeOptionGroupOptionsResponse :: DescribeOptionGroupOptionsResponse
mkDescribeOptionGroupOptionsResponse = DescribeOptionGroupOptionsResponse
    { _dogorOptionGroupOptions = mempty
    , _dogorMarker = Nothing
    }

-- | List of available option group options.
dogorOptionGroupOptions :: Lens' DescribeOptionGroupOptionsResponse [OptionGroupOption]
dogorOptionGroupOptions =
    lens _dogorOptionGroupOptions
         (\s a -> s { _dogorOptionGroupOptions = a })

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by MaxRecords.
dogorMarker :: Lens' DescribeOptionGroupOptionsResponse (Maybe Text)
dogorMarker = lens _dogorMarker (\s a -> s { _dogorMarker = a })

instance FromXML DescribeOptionGroupOptionsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeOptionGroupOptions where
    type Sv DescribeOptionGroupOptions = RDS
    type Rs DescribeOptionGroupOptions = DescribeOptionGroupOptionsResponse

    request = post "DescribeOptionGroupOptions"
    response _ = xmlResponse

instance AWSPager DescribeOptionGroupOptions where
    next rq rs = (\x -> rq & dogoMarker ?~ x)
        <$> (rs ^. dogorMarker)
