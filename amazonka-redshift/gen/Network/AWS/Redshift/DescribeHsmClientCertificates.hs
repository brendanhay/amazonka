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

-- Module      : Network.AWS.Redshift.DescribeHsmClientCertificates
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

-- | Returns information about the specified HSM client certificate. If no
-- certificate ID is specified, returns information about all the HSM
-- certificates owned by your AWS customer account.
--
-- If you specify both tag keys and tag values in the same request, Amazon
-- Redshift returns all HSM client certificates that match any combination of
-- the specified keys and values. For example, if you have 'owner' and 'environment'
-- for tag keys, and 'admin' and 'test' for tag values, all HSM client certificates
-- that have any combination of those values are returned.
--
-- If both tag keys and values are omitted from the request, HSM client
-- certificates are returned regardless of whether they have tag keys or values
-- associated with them.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DescribeHsmClientCertificates.html>
module Network.AWS.Redshift.DescribeHsmClientCertificates
    (
    -- * Request
      DescribeHsmClientCertificates
    -- ** Request constructor
    , describeHsmClientCertificates
    -- ** Request lenses
    , dhccHsmClientCertificateIdentifier
    , dhccMarker
    , dhccMaxRecords
    , dhccTagKeys
    , dhccTagValues

    -- * Response
    , DescribeHsmClientCertificatesResponse
    -- ** Response constructor
    , describeHsmClientCertificatesResponse
    -- ** Response lenses
    , dhccrHsmClientCertificates
    , dhccrMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import qualified GHC.Exts

data DescribeHsmClientCertificates = DescribeHsmClientCertificates
    { _dhccHsmClientCertificateIdentifier :: Maybe Text
    , _dhccMarker                         :: Maybe Text
    , _dhccMaxRecords                     :: Maybe Int
    , _dhccTagKeys                        :: List "member" Text
    , _dhccTagValues                      :: List "member" Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DescribeHsmClientCertificates' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dhccHsmClientCertificateIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'dhccMarker' @::@ 'Maybe' 'Text'
--
-- * 'dhccMaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'dhccTagKeys' @::@ ['Text']
--
-- * 'dhccTagValues' @::@ ['Text']
--
describeHsmClientCertificates :: DescribeHsmClientCertificates
describeHsmClientCertificates = DescribeHsmClientCertificates
    { _dhccHsmClientCertificateIdentifier = Nothing
    , _dhccMaxRecords                     = Nothing
    , _dhccMarker                         = Nothing
    , _dhccTagKeys                        = mempty
    , _dhccTagValues                      = mempty
    }

-- | The identifier of a specific HSM client certificate for which you want
-- information. If no identifier is specified, information is returned for all
-- HSM client certificates owned by your AWS customer account.
dhccHsmClientCertificateIdentifier :: Lens' DescribeHsmClientCertificates (Maybe Text)
dhccHsmClientCertificateIdentifier =
    lens _dhccHsmClientCertificateIdentifier
        (\s a -> s { _dhccHsmClientCertificateIdentifier = a })

-- | An optional parameter that specifies the starting point to return a set of
-- response records. When the results of a 'DescribeHsmClientCertificates' request
-- exceed the value specified in 'MaxRecords', AWS returns a value in the 'Marker'
-- field of the response. You can retrieve the next set of response records by
-- providing the returned marker value in the 'Marker' parameter and retrying the
-- request.
dhccMarker :: Lens' DescribeHsmClientCertificates (Maybe Text)
dhccMarker = lens _dhccMarker (\s a -> s { _dhccMarker = a })

-- | The maximum number of response records to return in each call. If the number
-- of remaining response records exceeds the specified 'MaxRecords' value, a value
-- is returned in a 'marker' field of the response. You can retrieve the next set
-- of records by retrying the command with the returned marker value.
--
-- Default: '100'
--
-- Constraints: minimum 20, maximum 100.
dhccMaxRecords :: Lens' DescribeHsmClientCertificates (Maybe Int)
dhccMaxRecords = lens _dhccMaxRecords (\s a -> s { _dhccMaxRecords = a })

-- | A tag key or keys for which you want to return all matching HSM client
-- certificates that are associated with the specified key or keys. For example,
-- suppose that you have HSM client certificates that are tagged with keys
-- called 'owner' and 'environment'. If you specify both of these tag keys in the
-- request, Amazon Redshift returns a response with the HSM client certificates
-- that have either or both of these tag keys associated with them.
dhccTagKeys :: Lens' DescribeHsmClientCertificates [Text]
dhccTagKeys = lens _dhccTagKeys (\s a -> s { _dhccTagKeys = a }) . _List

-- | A tag value or values for which you want to return all matching HSM client
-- certificates that are associated with the specified tag value or values. For
-- example, suppose that you have HSM client certificates that are tagged with
-- values called 'admin' and 'test'. If you specify both of these tag values in the
-- request, Amazon Redshift returns a response with the HSM client certificates
-- that have either or both of these tag values associated with them.
dhccTagValues :: Lens' DescribeHsmClientCertificates [Text]
dhccTagValues = lens _dhccTagValues (\s a -> s { _dhccTagValues = a }) . _List

data DescribeHsmClientCertificatesResponse = DescribeHsmClientCertificatesResponse
    { _dhccrHsmClientCertificates :: List "member" HsmClientCertificate
    , _dhccrMarker                :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'DescribeHsmClientCertificatesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dhccrHsmClientCertificates' @::@ ['HsmClientCertificate']
--
-- * 'dhccrMarker' @::@ 'Maybe' 'Text'
--
describeHsmClientCertificatesResponse :: DescribeHsmClientCertificatesResponse
describeHsmClientCertificatesResponse = DescribeHsmClientCertificatesResponse
    { _dhccrMarker                = Nothing
    , _dhccrHsmClientCertificates = mempty
    }

-- | A list of the identifiers for one or more HSM client certificates used by
-- Amazon Redshift clusters to store and retrieve database encryption keys in an
-- HSM.
dhccrHsmClientCertificates :: Lens' DescribeHsmClientCertificatesResponse [HsmClientCertificate]
dhccrHsmClientCertificates =
    lens _dhccrHsmClientCertificates
        (\s a -> s { _dhccrHsmClientCertificates = a })
            . _List

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response, you
-- can retrieve the next set of records by providing this returned marker value
-- in the 'Marker' parameter and retrying the command. If the 'Marker' field is
-- empty, all response records have been retrieved for the request.
dhccrMarker :: Lens' DescribeHsmClientCertificatesResponse (Maybe Text)
dhccrMarker = lens _dhccrMarker (\s a -> s { _dhccrMarker = a })

instance ToPath DescribeHsmClientCertificates where
    toPath = const "/"

instance ToQuery DescribeHsmClientCertificates where
    toQuery DescribeHsmClientCertificates{..} = mconcat
        [ "HsmClientCertificateIdentifier" =? _dhccHsmClientCertificateIdentifier
        , "Marker"                         =? _dhccMarker
        , "MaxRecords"                     =? _dhccMaxRecords
        , "TagKeys"                        =? _dhccTagKeys
        , "TagValues"                      =? _dhccTagValues
        ]

instance ToHeaders DescribeHsmClientCertificates

instance AWSRequest DescribeHsmClientCertificates where
    type Sv DescribeHsmClientCertificates = Redshift
    type Rs DescribeHsmClientCertificates = DescribeHsmClientCertificatesResponse

    request  = post "DescribeHsmClientCertificates"
    response = xmlResponse

instance FromXML DescribeHsmClientCertificatesResponse where
    parseXML = withElement "DescribeHsmClientCertificatesResult" $ \x -> DescribeHsmClientCertificatesResponse
        <$> x .@? "HsmClientCertificates" .!@ mempty
        <*> x .@? "Marker"

instance AWSPager DescribeHsmClientCertificates where
    page rq rs
        | stop (rq ^. dhccMarker) = Nothing
        | otherwise = (\x -> rq & dhccMarker ?~ x)
            <$> (rs ^. dhccrMarker)
