{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
module Network.AWS.Redshift.DescribeHsmClientCertificates
    (
    -- * Request
      DescribeHsmClientCertificatesMessage
    -- ** Request constructor
    , describeHsmClientCertificatesMessage
    -- ** Request lenses
    , dhccmHsmClientCertificateIdentifier
    , dhccmMarker
    , dhccmMaxRecords

    -- * Response
    , HsmClientCertificateMessage
    -- ** Response constructor
    , hsmClientCertificateMessage
    -- ** Response lenses
    , hccmHsmClientCertificates
    , hccmMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types

data DescribeHsmClientCertificatesMessage = DescribeHsmClientCertificatesMessage
    { _dhccmHsmClientCertificateIdentifier :: Maybe Text
    , _dhccmMarker                         :: Maybe Text
    , _dhccmMaxRecords                     :: Maybe Int
    } (Eq, Ord, Show, Generic)

-- | 'DescribeHsmClientCertificatesMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dhccmHsmClientCertificateIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'dhccmMarker' @::@ 'Maybe' 'Text'
--
-- * 'dhccmMaxRecords' @::@ 'Maybe' 'Int'
--
describeHsmClientCertificatesMessage :: DescribeHsmClientCertificatesMessage
describeHsmClientCertificatesMessage = DescribeHsmClientCertificatesMessage
    { _dhccmHsmClientCertificateIdentifier = Nothing
    , _dhccmMaxRecords                     = Nothing
    , _dhccmMarker                         = Nothing
    }

-- | The identifier of a specific HSM client certificate for which you want
-- information. If no identifier is specified, information is returned for
-- all HSM client certificates owned by your AWS customer account.
dhccmHsmClientCertificateIdentifier :: Lens' DescribeHsmClientCertificatesMessage (Maybe Text)
dhccmHsmClientCertificateIdentifier =
    lens _dhccmHsmClientCertificateIdentifier
        (\s a -> s { _dhccmHsmClientCertificateIdentifier = a })

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeHsmClientCertificates
-- request exceed the value specified in MaxRecords, AWS returns a value in
-- the Marker field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the Marker
-- parameter and retrying the request.
dhccmMarker :: Lens' DescribeHsmClientCertificatesMessage (Maybe Text)
dhccmMarker = lens _dhccmMarker (\s a -> s { _dhccmMarker = a })

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified MaxRecords
-- value, a value is returned in a marker field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value. Default: 100 Constraints: minimum 20, maximum 100.
dhccmMaxRecords :: Lens' DescribeHsmClientCertificatesMessage (Maybe Int)
dhccmMaxRecords = lens _dhccmMaxRecords (\s a -> s { _dhccmMaxRecords = a })
instance ToQuery DescribeHsmClientCertificatesMessage

instance ToPath DescribeHsmClientCertificatesMessage where
    toPath = const "/"

data HsmClientCertificateMessage = HsmClientCertificateMessage
    { _hccmHsmClientCertificates :: [HsmClientCertificate]
    , _hccmMarker                :: Maybe Text
    } (Eq, Show, Generic)

-- | 'HsmClientCertificateMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'hccmHsmClientCertificates' @::@ ['HsmClientCertificate']
--
-- * 'hccmMarker' @::@ 'Maybe' 'Text'
--
hsmClientCertificateMessage :: HsmClientCertificateMessage
hsmClientCertificateMessage = HsmClientCertificateMessage
    { _hccmMarker                = Nothing
    , _hccmHsmClientCertificates = mempty
    }

-- | A list of the identifiers for one or more HSM client certificates used by
-- Amazon Redshift clusters to store and retrieve database encryption keys
-- in an HSM.
hccmHsmClientCertificates :: Lens' HsmClientCertificateMessage [HsmClientCertificate]
hccmHsmClientCertificates =
    lens _hccmHsmClientCertificates
        (\s a -> s { _hccmHsmClientCertificates = a })

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the Marker parameter and retrying the command. If the
-- Marker field is empty, all response records have been retrieved for the
-- request.
hccmMarker :: Lens' HsmClientCertificateMessage (Maybe Text)
hccmMarker = lens _hccmMarker (\s a -> s { _hccmMarker = a })

instance FromXML HsmClientCertificateMessage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "HsmClientCertificateMessage"

instance AWSRequest DescribeHsmClientCertificatesMessage where
    type Sv DescribeHsmClientCertificatesMessage = Redshift
    type Rs DescribeHsmClientCertificatesMessage = HsmClientCertificateMessage

    request  = post "DescribeHsmClientCertificates"
    response = xmlResponse $ \h x -> HsmClientCertificateMessage
        <$> x %| "HsmClientCertificates"
        <*> x %| "Marker"
