{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.DescribeHsmClientCertificates
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
module Network.AWS.Redshift.V2012_12_01.DescribeHsmClientCertificates
    (
    -- * Request
      DescribeHsmClientCertificates
    -- ** Request constructor
    , mkDescribeHsmClientCertificates
    -- ** Request lenses
    , dhcc1HsmClientCertificateIdentifier
    , dhcc1MaxRecords
    , dhcc1Marker

    -- * Response
    , DescribeHsmClientCertificatesResponse
    -- ** Response lenses
    , dhccrsMarker
    , dhccrsHsmClientCertificates
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | 
data DescribeHsmClientCertificates = DescribeHsmClientCertificates
    { _dhcc1HsmClientCertificateIdentifier :: Maybe Text
    , _dhcc1MaxRecords :: Maybe Integer
    , _dhcc1Marker :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeHsmClientCertificates' request.
mkDescribeHsmClientCertificates :: DescribeHsmClientCertificates
mkDescribeHsmClientCertificates = DescribeHsmClientCertificates
    { _dhcc1HsmClientCertificateIdentifier = Nothing
    , _dhcc1MaxRecords = Nothing
    , _dhcc1Marker = Nothing
    }

-- | The identifier of a specific HSM client certificate for which you want
-- information. If no identifier is specified, information is returned for all
-- HSM client certificates owned by your AWS customer account.
dhcc1HsmClientCertificateIdentifier :: Lens' DescribeHsmClientCertificates (Maybe Text)
dhcc1HsmClientCertificateIdentifier =
    lens _dhcc1HsmClientCertificateIdentifier
         (\s a -> s { _dhcc1HsmClientCertificateIdentifier = a })

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified MaxRecords
-- value, a value is returned in a marker field of the response. You can
-- retrieve the next set of records by retrying the command with the returned
-- marker value. Default: 100 Constraints: minimum 20, maximum 100.
dhcc1MaxRecords :: Lens' DescribeHsmClientCertificates (Maybe Integer)
dhcc1MaxRecords = lens _dhcc1MaxRecords (\s a -> s { _dhcc1MaxRecords = a })

-- | An optional parameter that specifies the starting point to return a set of
-- response records. When the results of a DescribeHsmClientCertificates
-- request exceed the value specified in MaxRecords, AWS returns a value in
-- the Marker field of the response. You can retrieve the next set of response
-- records by providing the returned marker value in the Marker parameter and
-- retrying the request.
dhcc1Marker :: Lens' DescribeHsmClientCertificates (Maybe Text)
dhcc1Marker = lens _dhcc1Marker (\s a -> s { _dhcc1Marker = a })

instance ToQuery DescribeHsmClientCertificates where
    toQuery = genericQuery def

-- | 
data DescribeHsmClientCertificatesResponse = DescribeHsmClientCertificatesResponse
    { _dhccrsMarker :: Maybe Text
    , _dhccrsHsmClientCertificates :: [HsmClientCertificate]
    } deriving (Show, Generic)

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response, you
-- can retrieve the next set of records by providing this returned marker
-- value in the Marker parameter and retrying the command. If the Marker field
-- is empty, all response records have been retrieved for the request.
dhccrsMarker :: Lens' DescribeHsmClientCertificatesResponse (Maybe Text)
dhccrsMarker = lens _dhccrsMarker (\s a -> s { _dhccrsMarker = a })

-- | A list of the identifiers for one or more HSM client certificates used by
-- Amazon Redshift clusters to store and retrieve database encryption keys in
-- an HSM.
dhccrsHsmClientCertificates :: Lens' DescribeHsmClientCertificatesResponse [HsmClientCertificate]
dhccrsHsmClientCertificates =
    lens _dhccrsHsmClientCertificates
         (\s a -> s { _dhccrsHsmClientCertificates = a })

instance FromXML DescribeHsmClientCertificatesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeHsmClientCertificates where
    type Sv DescribeHsmClientCertificates = Redshift
    type Rs DescribeHsmClientCertificates = DescribeHsmClientCertificatesResponse

    request = post "DescribeHsmClientCertificates"
    response _ = xmlResponse

instance AWSPager DescribeHsmClientCertificates where
    next rq rs = (\x -> rq & dhcc1Marker ?~ x)
        <$> (rs ^. dhccrsMarker)
