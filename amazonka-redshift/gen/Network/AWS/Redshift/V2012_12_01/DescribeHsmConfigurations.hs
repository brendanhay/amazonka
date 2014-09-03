{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.DescribeHsmConfigurations
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns information about the specified Amazon Redshift HSM configuration.
-- If no configuration ID is specified, returns information about all the HSM
-- configurations owned by your AWS customer account.
module Network.AWS.Redshift.V2012_12_01.DescribeHsmConfigurations
    (
    -- * Request
      DescribeHsmConfigurations
    -- ** Request constructor
    , describeHsmConfigurations
    -- ** Request lenses
    , dhcnMaxRecords
    , dhcnHsmConfigurationIdentifier
    , dhcnMarker

    -- * Response
    , DescribeHsmConfigurationsResponse
    -- ** Response lenses
    , hcmHsmConfigurations
    , hcmMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeHsmConfigurations' request.
describeHsmConfigurations :: DescribeHsmConfigurations
describeHsmConfigurations = DescribeHsmConfigurations
    { _dhcnMaxRecords = Nothing
    , _dhcnHsmConfigurationIdentifier = Nothing
    , _dhcnMarker = Nothing
    }

data DescribeHsmConfigurations = DescribeHsmConfigurations
    { _dhcnMaxRecords :: Maybe Integer
      -- ^ The maximum number of response records to return in each call. If
      -- the number of remaining response records exceeds the specified
      -- MaxRecords value, a value is returned in a marker field of the
      -- response. You can retrieve the next set of records by retrying
      -- the command with the returned marker value. Default: 100
      -- Constraints: minimum 20, maximum 100.
    , _dhcnHsmConfigurationIdentifier :: Maybe Text
      -- ^ The identifier of a specific Amazon Redshift HSM configuration to
      -- be described. If no identifier is specified, information is
      -- returned for all HSM configurations owned by your AWS customer
      -- account.
    , _dhcnMarker :: Maybe Text
      -- ^ An optional parameter that specifies the starting point to return
      -- a set of response records. When the results of a
      -- DescribeHsmConfigurations request exceed the value specified in
      -- MaxRecords, AWS returns a value in the Marker field of the
      -- response. You can retrieve the next set of response records by
      -- providing the returned marker value in the Marker parameter and
      -- retrying the request.
    } deriving (Show, Generic)

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified MaxRecords
-- value, a value is returned in a marker field of the response. You can
-- retrieve the next set of records by retrying the command with the returned
-- marker value. Default: 100 Constraints: minimum 20, maximum 100.
dhcnMaxRecords
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DescribeHsmConfigurations
    -> f DescribeHsmConfigurations
dhcnMaxRecords f x =
    (\y -> x { _dhcnMaxRecords = y })
       <$> f (_dhcnMaxRecords x)
{-# INLINE dhcnMaxRecords #-}

-- | The identifier of a specific Amazon Redshift HSM configuration to be
-- described. If no identifier is specified, information is returned for all
-- HSM configurations owned by your AWS customer account.
dhcnHsmConfigurationIdentifier
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeHsmConfigurations
    -> f DescribeHsmConfigurations
dhcnHsmConfigurationIdentifier f x =
    (\y -> x { _dhcnHsmConfigurationIdentifier = y })
       <$> f (_dhcnHsmConfigurationIdentifier x)
{-# INLINE dhcnHsmConfigurationIdentifier #-}

-- | An optional parameter that specifies the starting point to return a set of
-- response records. When the results of a DescribeHsmConfigurations request
-- exceed the value specified in MaxRecords, AWS returns a value in the Marker
-- field of the response. You can retrieve the next set of response records by
-- providing the returned marker value in the Marker parameter and retrying
-- the request.
dhcnMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeHsmConfigurations
    -> f DescribeHsmConfigurations
dhcnMarker f x =
    (\y -> x { _dhcnMarker = y })
       <$> f (_dhcnMarker x)
{-# INLINE dhcnMarker #-}

instance ToQuery DescribeHsmConfigurations where
    toQuery = genericQuery def

data DescribeHsmConfigurationsResponse = DescribeHsmConfigurationsResponse
    { _hcmHsmConfigurations :: [HsmConfiguration]
      -- ^ A list of Amazon Redshift HSM configurations.
    , _hcmMarker :: Maybe Text
      -- ^ A value that indicates the starting point for the next set of
      -- response records in a subsequent request. If a value is returned
      -- in a response, you can retrieve the next set of records by
      -- providing this returned marker value in the Marker parameter and
      -- retrying the command. If the Marker field is empty, all response
      -- records have been retrieved for the request.
    } deriving (Show, Generic)

-- | A list of Amazon Redshift HSM configurations.
hcmHsmConfigurations
    :: Functor f
    => ([HsmConfiguration]
    -> f ([HsmConfiguration]))
    -> DescribeHsmConfigurationsResponse
    -> f DescribeHsmConfigurationsResponse
hcmHsmConfigurations f x =
    (\y -> x { _hcmHsmConfigurations = y })
       <$> f (_hcmHsmConfigurations x)
{-# INLINE hcmHsmConfigurations #-}

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response, you
-- can retrieve the next set of records by providing this returned marker
-- value in the Marker parameter and retrying the command. If the Marker field
-- is empty, all response records have been retrieved for the request.
hcmMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeHsmConfigurationsResponse
    -> f DescribeHsmConfigurationsResponse
hcmMarker f x =
    (\y -> x { _hcmMarker = y })
       <$> f (_hcmMarker x)
{-# INLINE hcmMarker #-}

instance FromXML DescribeHsmConfigurationsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeHsmConfigurations where
    type Sv DescribeHsmConfigurations = Redshift
    type Rs DescribeHsmConfigurations = DescribeHsmConfigurationsResponse

    request = post "DescribeHsmConfigurations"
    response _ = xmlResponse

instance AWSPager DescribeHsmConfigurations where
    next rq rs = (\x -> rq { _dhcnMarker = Just x })
        <$> (_hcmMarker rs)
