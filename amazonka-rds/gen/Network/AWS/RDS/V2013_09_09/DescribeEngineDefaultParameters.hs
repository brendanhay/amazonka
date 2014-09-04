{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.V2013_09_09.DescribeEngineDefaultParameters
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the default engine and system parameter information for the
-- specified database engine. https://rds.amazonaws.com/
-- ?Action=DescribeEngineDefaultParameters &DBParameterGroupFamily=mysql5.1
-- &Version=2013-05-15 &MaxRecords=100 &SignatureVersion=2
-- &SignatureMethod=HmacSHA256 &Timestamp=2011-02-15T19%3A10%3A03.510Z
-- &AWSAccessKeyId= &Signature= bG93ZXJfY2FzZV90YWJsZV9uYW1lcw== mysql5.1
-- boolean engine-default false Controls whether user-defined functions that
-- have only an xxx symbol for the main function can be loaded static 0,1
-- allow-suspicious-udfs integer engine-default true Intended for use with
-- master-to-master replication, and can be used to control the operation of
-- AUTO_INCREMENT columns dynamic 1-65535 auto_increment_increment integer
-- engine-default true Determines the starting point for the AUTO_INCREMENT
-- column value dynamic 1-65535 auto_increment_offset
-- 6c1341eb-a124-11df-bf5c-973b09643c5d.
module Network.AWS.RDS.V2013_09_09.DescribeEngineDefaultParameters
    (
    -- * Request
      DescribeEngineDefaultParameters
    -- ** Request constructor
    , mkDescribeEngineDefaultParametersMessage
    -- ** Request lenses
    , dedpmDBParameterGroupFamily
    , dedpmMaxRecords
    , dedpmMarker

    -- * Response
    , DescribeEngineDefaultParametersResponse
    -- ** Response lenses
    , edwEngineDefaults
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeEngineDefaultParameters' request.
mkDescribeEngineDefaultParametersMessage :: Text -- ^ 'dedpmDBParameterGroupFamily'
                                         -> DescribeEngineDefaultParameters
mkDescribeEngineDefaultParametersMessage p1 = DescribeEngineDefaultParameters
    { _dedpmDBParameterGroupFamily = p1
    , _dedpmMaxRecords = Nothing
    , _dedpmMarker = Nothing
    }
{-# INLINE mkDescribeEngineDefaultParametersMessage #-}

data DescribeEngineDefaultParameters = DescribeEngineDefaultParameters
    { _dedpmDBParameterGroupFamily :: Text
      -- ^ The name of the DB parameter group family.
    , _dedpmMaxRecords :: Maybe Integer
      -- ^ The maximum number of records to include in the response. If more
      -- records exist than the specified MaxRecords value, a pagination
      -- token called a marker is included in the response so that the
      -- remaining results may be retrieved. Default: 100 Constraints:
      -- minimum 20, maximum 100.
    , _dedpmMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous
      -- DescribeEngineDefaultParameters request. If this parameter is
      -- specified, the response includes only records beyond the marker,
      -- up to the value specified by MaxRecords.
    } deriving (Show, Generic)

-- | The name of the DB parameter group family.
dedpmDBParameterGroupFamily :: Lens' DescribeEngineDefaultParameters (Text)
dedpmDBParameterGroupFamily = lens _dedpmDBParameterGroupFamily (\s a -> s { _dedpmDBParameterGroupFamily = a })
{-# INLINE dedpmDBParameterGroupFamily #-}

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a pagination token called a
-- marker is included in the response so that the remaining results may be
-- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
dedpmMaxRecords :: Lens' DescribeEngineDefaultParameters (Maybe Integer)
dedpmMaxRecords = lens _dedpmMaxRecords (\s a -> s { _dedpmMaxRecords = a })
{-# INLINE dedpmMaxRecords #-}

-- | An optional pagination token provided by a previous
-- DescribeEngineDefaultParameters request. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by MaxRecords.
dedpmMarker :: Lens' DescribeEngineDefaultParameters (Maybe Text)
dedpmMarker = lens _dedpmMarker (\s a -> s { _dedpmMarker = a })
{-# INLINE dedpmMarker #-}

instance ToQuery DescribeEngineDefaultParameters where
    toQuery = genericQuery def

newtype DescribeEngineDefaultParametersResponse = DescribeEngineDefaultParametersResponse
    { _edwEngineDefaults :: Maybe EngineDefaults
      -- ^ Contains the result of a successful invocation of the
      -- DescribeEngineDefaultParameters action.
    } deriving (Show, Generic)

-- | Contains the result of a successful invocation of the
-- DescribeEngineDefaultParameters action.
edwEngineDefaults :: Lens' DescribeEngineDefaultParametersResponse (Maybe EngineDefaults)
edwEngineDefaults = lens _edwEngineDefaults (\s a -> s { _edwEngineDefaults = a })
{-# INLINE edwEngineDefaults #-}

instance FromXML DescribeEngineDefaultParametersResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeEngineDefaultParameters where
    type Sv DescribeEngineDefaultParameters = RDS
    type Rs DescribeEngineDefaultParameters = DescribeEngineDefaultParametersResponse

    request = post "DescribeEngineDefaultParameters"
    response _ = xmlResponse

instance AWSPager DescribeEngineDefaultParameters where
    next rq rs = (\x -> rq { _dedpmMarker = Just x })
        <$> (_edMarker $ _edwEngineDefaults rs)
