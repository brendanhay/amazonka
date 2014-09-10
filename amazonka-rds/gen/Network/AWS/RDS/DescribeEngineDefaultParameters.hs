{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS
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
module Network.AWS.RDS
    (
    -- * Request
      DescribeEngineDefaultParameters
    -- ** Request constructor
    , mkDescribeEngineDefaultParameters
    -- ** Request lenses
    , dedpDBParameterGroupFamily
    , dedpMaxRecords
    , dedpMarker

    -- * Response
    , DescribeEngineDefaultParametersResponse
    -- ** Response constructor
    , mkDescribeEngineDefaultParametersResponse
    -- ** Response lenses
    , dedprEngineDefaults
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import Network.AWS.Prelude

-- | 
data DescribeEngineDefaultParameters = DescribeEngineDefaultParameters
    { _dedpDBParameterGroupFamily :: Text
    , _dedpMaxRecords :: Maybe Integer
    , _dedpMarker :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeEngineDefaultParameters' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DBParameterGroupFamily ::@ @Text@
--
-- * @MaxRecords ::@ @Maybe Integer@
--
-- * @Marker ::@ @Maybe Text@
--
mkDescribeEngineDefaultParameters :: Text -- ^ 'dedpDBParameterGroupFamily'
                                  -> DescribeEngineDefaultParameters
mkDescribeEngineDefaultParameters p1 = DescribeEngineDefaultParameters
    { _dedpDBParameterGroupFamily = p1
    , _dedpMaxRecords = Nothing
    , _dedpMarker = Nothing
    }

-- | The name of the DB parameter group family.
dedpDBParameterGroupFamily :: Lens' DescribeEngineDefaultParameters Text
dedpDBParameterGroupFamily =
    lens _dedpDBParameterGroupFamily
         (\s a -> s { _dedpDBParameterGroupFamily = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a pagination token called a
-- marker is included in the response so that the remaining results may be
-- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
dedpMaxRecords :: Lens' DescribeEngineDefaultParameters (Maybe Integer)
dedpMaxRecords = lens _dedpMaxRecords (\s a -> s { _dedpMaxRecords = a })

-- | An optional pagination token provided by a previous
-- DescribeEngineDefaultParameters request. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by MaxRecords.
dedpMarker :: Lens' DescribeEngineDefaultParameters (Maybe Text)
dedpMarker = lens _dedpMarker (\s a -> s { _dedpMarker = a })

instance ToQuery DescribeEngineDefaultParameters where
    toQuery = genericQuery def

newtype DescribeEngineDefaultParametersResponse = DescribeEngineDefaultParametersResponse
    { _dedprEngineDefaults :: EngineDefaults
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeEngineDefaultParametersResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @EngineDefaults ::@ @EngineDefaults@
--
mkDescribeEngineDefaultParametersResponse :: EngineDefaults -- ^ 'dedprEngineDefaults'
                                          -> DescribeEngineDefaultParametersResponse
mkDescribeEngineDefaultParametersResponse p1 = DescribeEngineDefaultParametersResponse
    { _dedprEngineDefaults = p1
    }

-- | Contains the result of a successful invocation of the
-- DescribeEngineDefaultParameters action.
dedprEngineDefaults :: Lens' DescribeEngineDefaultParametersResponse EngineDefaults
dedprEngineDefaults =
    lens _dedprEngineDefaults (\s a -> s { _dedprEngineDefaults = a })

instance FromXML DescribeEngineDefaultParametersResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeEngineDefaultParameters where
    type Sv DescribeEngineDefaultParameters = RDS
    type Rs DescribeEngineDefaultParameters = DescribeEngineDefaultParametersResponse

    request = post "DescribeEngineDefaultParameters"
    response _ = xmlResponse

instance AWSPager DescribeEngineDefaultParameters where
    next rq rs = (\x -> rq & dedpMarker ?~ x)
        <$> (rs ^. dedprEngineDefaults . edMarker)
