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

-- | Returns information about the specified HSM client certificate. If no
-- certificate ID is specified, returns information about all the HSM
-- certificates owned by your AWS customer account.
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
    } deriving (Eq, Ord, Show, Generic)

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
describeHsmClientCertificates :: DescribeHsmClientCertificates
describeHsmClientCertificates = DescribeHsmClientCertificates
    { _dhccHsmClientCertificateIdentifier = Nothing
    , _dhccMaxRecords                     = Nothing
    , _dhccMarker                         = Nothing
    }

-- | The identifier of a specific HSM client certificate for which you want
-- information. If no identifier is specified, information is returned for
-- all HSM client certificates owned by your AWS customer account.
dhccHsmClientCertificateIdentifier :: Lens' DescribeHsmClientCertificates (Maybe Text)
dhccHsmClientCertificateIdentifier =
    lens _dhccHsmClientCertificateIdentifier
        (\s a -> s { _dhccHsmClientCertificateIdentifier = a })

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeHsmClientCertificates
-- request exceed the value specified in MaxRecords, AWS returns a value in
-- the Marker field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the Marker
-- parameter and retrying the request.
dhccMarker :: Lens' DescribeHsmClientCertificates (Maybe Text)
dhccMarker = lens _dhccMarker (\s a -> s { _dhccMarker = a })

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified MaxRecords
-- value, a value is returned in a marker field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value. Default: 100 Constraints: minimum 20, maximum 100.
dhccMaxRecords :: Lens' DescribeHsmClientCertificates (Maybe Int)
dhccMaxRecords = lens _dhccMaxRecords (\s a -> s { _dhccMaxRecords = a })

data DescribeHsmClientCertificatesResponse = DescribeHsmClientCertificatesResponse
    { _dhccrHsmClientCertificates :: [HsmClientCertificate]
    , _dhccrMarker                :: Maybe Text
    } deriving (Eq, Show, Generic)

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
-- Amazon Redshift clusters to store and retrieve database encryption keys
-- in an HSM.
dhccrHsmClientCertificates :: Lens' DescribeHsmClientCertificatesResponse [HsmClientCertificate]
dhccrHsmClientCertificates =
    lens _dhccrHsmClientCertificates
        (\s a -> s { _dhccrHsmClientCertificates = a })

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the Marker parameter and retrying the command. If the
-- Marker field is empty, all response records have been retrieved for the
-- request.
dhccrMarker :: Lens' DescribeHsmClientCertificatesResponse (Maybe Text)
dhccrMarker = lens _dhccrMarker (\s a -> s { _dhccrMarker = a })

instance ToPath DescribeHsmClientCertificates where
    toPath = const "/"

instance ToQuery DescribeHsmClientCertificates

instance ToHeaders DescribeHsmClientCertificates

instance AWSRequest DescribeHsmClientCertificates where
    type Sv DescribeHsmClientCertificates = Redshift
    type Rs DescribeHsmClientCertificates = DescribeHsmClientCertificatesResponse

    request  = post "DescribeHsmClientCertificates"
    response = xmlResponse

instance FromXML DescribeHsmClientCertificatesResponse where
    parseXML c = DescribeHsmClientCertificatesResponse
        <$> c .: "HsmClientCertificates"
        <*> c .:? "Marker"
