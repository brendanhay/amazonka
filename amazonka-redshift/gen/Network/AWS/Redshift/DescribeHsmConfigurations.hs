{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.DescribeHsmConfigurations
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns information about the specified Amazon Redshift HSM configuration. If
-- no configuration ID is specified, returns information about all the HSM
-- configurations owned by your AWS customer account.
--
-- If you specify both tag keys and tag values in the same request, Amazon
-- Redshift returns all HSM connections that match any combination of the
-- specified keys and values. For example, if you have 'owner' and 'environment' for
-- tag keys, and 'admin' and 'test' for tag values, all HSM connections that have
-- any combination of those values are returned.
--
-- If both tag keys and values are omitted from the request, HSM connections
-- are returned regardless of whether they have tag keys or values associated
-- with them.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DescribeHsmConfigurations.html>
module Network.AWS.Redshift.DescribeHsmConfigurations
    (
    -- * Request
      DescribeHsmConfigurations
    -- ** Request constructor
    , describeHsmConfigurations
    -- ** Request lenses
    , dhc1HsmConfigurationIdentifier
    , dhc1Marker
    , dhc1MaxRecords
    , dhc1TagKeys
    , dhc1TagValues

    -- * Response
    , DescribeHsmConfigurationsResponse
    -- ** Response constructor
    , describeHsmConfigurationsResponse
    -- ** Response lenses
    , dhcrHsmConfigurations
    , dhcrMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import qualified GHC.Exts

data DescribeHsmConfigurations = DescribeHsmConfigurations
    { _dhc1HsmConfigurationIdentifier :: Maybe Text
    , _dhc1Marker                     :: Maybe Text
    , _dhc1MaxRecords                 :: Maybe Int
    , _dhc1TagKeys                    :: List "member" Text
    , _dhc1TagValues                  :: List "member" Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DescribeHsmConfigurations' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dhc1HsmConfigurationIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'dhc1Marker' @::@ 'Maybe' 'Text'
--
-- * 'dhc1MaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'dhc1TagKeys' @::@ ['Text']
--
-- * 'dhc1TagValues' @::@ ['Text']
--
describeHsmConfigurations :: DescribeHsmConfigurations
describeHsmConfigurations = DescribeHsmConfigurations
    { _dhc1HsmConfigurationIdentifier = Nothing
    , _dhc1MaxRecords                 = Nothing
    , _dhc1Marker                     = Nothing
    , _dhc1TagKeys                    = mempty
    , _dhc1TagValues                  = mempty
    }

-- | The identifier of a specific Amazon Redshift HSM configuration to be
-- described. If no identifier is specified, information is returned for all HSM
-- configurations owned by your AWS customer account.
dhc1HsmConfigurationIdentifier :: Lens' DescribeHsmConfigurations (Maybe Text)
dhc1HsmConfigurationIdentifier =
    lens _dhc1HsmConfigurationIdentifier
        (\s a -> s { _dhc1HsmConfigurationIdentifier = a })

-- | An optional parameter that specifies the starting point to return a set of
-- response records. When the results of a 'DescribeHsmConfigurations' request
-- exceed the value specified in 'MaxRecords', AWS returns a value in the 'Marker'
-- field of the response. You can retrieve the next set of response records by
-- providing the returned marker value in the 'Marker' parameter and retrying the
-- request.
dhc1Marker :: Lens' DescribeHsmConfigurations (Maybe Text)
dhc1Marker = lens _dhc1Marker (\s a -> s { _dhc1Marker = a })

-- | The maximum number of response records to return in each call. If the number
-- of remaining response records exceeds the specified 'MaxRecords' value, a value
-- is returned in a 'marker' field of the response. You can retrieve the next set
-- of records by retrying the command with the returned marker value.
--
-- Default: '100'
--
-- Constraints: minimum 20, maximum 100.
dhc1MaxRecords :: Lens' DescribeHsmConfigurations (Maybe Int)
dhc1MaxRecords = lens _dhc1MaxRecords (\s a -> s { _dhc1MaxRecords = a })

-- | A tag key or keys for which you want to return all matching HSM
-- configurations that are associated with the specified key or keys. For
-- example, suppose that you have HSM configurations that are tagged with keys
-- called 'owner' and 'environment'. If you specify both of these tag keys in the
-- request, Amazon Redshift returns a response with the HSM configurations that
-- have either or both of these tag keys associated with them.
dhc1TagKeys :: Lens' DescribeHsmConfigurations [Text]
dhc1TagKeys = lens _dhc1TagKeys (\s a -> s { _dhc1TagKeys = a }) . _List

-- | A tag value or values for which you want to return all matching HSM
-- configurations that are associated with the specified tag value or values.
-- For example, suppose that you have HSM configurations that are tagged with
-- values called 'admin' and 'test'. If you specify both of these tag values in the
-- request, Amazon Redshift returns a response with the HSM configurations that
-- have either or both of these tag values associated with them.
dhc1TagValues :: Lens' DescribeHsmConfigurations [Text]
dhc1TagValues = lens _dhc1TagValues (\s a -> s { _dhc1TagValues = a }) . _List

data DescribeHsmConfigurationsResponse = DescribeHsmConfigurationsResponse
    { _dhcrHsmConfigurations :: List "member" HsmConfiguration
    , _dhcrMarker            :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'DescribeHsmConfigurationsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dhcrHsmConfigurations' @::@ ['HsmConfiguration']
--
-- * 'dhcrMarker' @::@ 'Maybe' 'Text'
--
describeHsmConfigurationsResponse :: DescribeHsmConfigurationsResponse
describeHsmConfigurationsResponse = DescribeHsmConfigurationsResponse
    { _dhcrMarker            = Nothing
    , _dhcrHsmConfigurations = mempty
    }

-- | A list of Amazon Redshift HSM configurations.
dhcrHsmConfigurations :: Lens' DescribeHsmConfigurationsResponse [HsmConfiguration]
dhcrHsmConfigurations =
    lens _dhcrHsmConfigurations (\s a -> s { _dhcrHsmConfigurations = a })
        . _List

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response, you
-- can retrieve the next set of records by providing this returned marker value
-- in the 'Marker' parameter and retrying the command. If the 'Marker' field is
-- empty, all response records have been retrieved for the request.
dhcrMarker :: Lens' DescribeHsmConfigurationsResponse (Maybe Text)
dhcrMarker = lens _dhcrMarker (\s a -> s { _dhcrMarker = a })

instance ToPath DescribeHsmConfigurations where
    toPath = const "/"

instance ToQuery DescribeHsmConfigurations where
    toQuery DescribeHsmConfigurations{..} = mconcat
        [ "HsmConfigurationIdentifier" =? _dhc1HsmConfigurationIdentifier
        , "Marker"                     =? _dhc1Marker
        , "MaxRecords"                 =? _dhc1MaxRecords
        , "TagKeys"                    =? _dhc1TagKeys
        , "TagValues"                  =? _dhc1TagValues
        ]

instance ToHeaders DescribeHsmConfigurations

instance AWSRequest DescribeHsmConfigurations where
    type Sv DescribeHsmConfigurations = Redshift
    type Rs DescribeHsmConfigurations = DescribeHsmConfigurationsResponse

    request  = post "DescribeHsmConfigurations"
    response = xmlResponse

instance FromXML DescribeHsmConfigurationsResponse where
    parseXML = withElement "DescribeHsmConfigurationsResult" $ \x -> DescribeHsmConfigurationsResponse
        <$> x .@? "HsmConfigurations" .!@ mempty
        <*> x .@? "Marker"

instance AWSPager DescribeHsmConfigurations where
    page rq rs
        | stop (rq ^. dhc1Marker) = Nothing
        | otherwise = (\x -> rq & dhc1Marker ?~ x)
            <$> (rs ^. dhcrMarker)
