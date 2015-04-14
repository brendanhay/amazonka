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

-- Module      : Network.AWS.RDS.DescribeCertificates
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

-- | Lists the set of CA certificates provided by Amazon RDS for this AWS
-- account.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DescribeCertificates.html>
module Network.AWS.RDS.DescribeCertificates
    (
    -- * Request
      DescribeCertificates
    -- ** Request constructor
    , describeCertificates
    -- ** Request lenses
    , dcCertificateIdentifier
    , dcFilters
    , dcMarker
    , dcMaxRecords

    -- * Response
    , DescribeCertificatesResponse
    -- ** Response constructor
    , describeCertificatesResponse
    -- ** Response lenses
    , dcrCertificates
    , dcrMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import qualified GHC.Exts

data DescribeCertificates = DescribeCertificates
    { _dcCertificateIdentifier :: Maybe Text
    , _dcFilters               :: List "member" Filter
    , _dcMarker                :: Maybe Text
    , _dcMaxRecords            :: Maybe Int
    } deriving (Eq, Read, Show)

-- | 'DescribeCertificates' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcCertificateIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'dcFilters' @::@ ['Filter']
--
-- * 'dcMarker' @::@ 'Maybe' 'Text'
--
-- * 'dcMaxRecords' @::@ 'Maybe' 'Int'
--
describeCertificates :: DescribeCertificates
describeCertificates = DescribeCertificates
    { _dcCertificateIdentifier = Nothing
    , _dcFilters               = mempty
    , _dcMaxRecords            = Nothing
    , _dcMarker                = Nothing
    }

-- | The user-supplied certificate identifier. If this parameter is specified,
-- information for only the identified certificate is returned. This parameter
-- isn't case-sensitive.
--
-- Constraints:
--
-- Must contain from 1 to 63 alphanumeric characters or hyphens First
-- character must be a letter Cannot end with a hyphen or contain two
-- consecutive hyphens
dcCertificateIdentifier :: Lens' DescribeCertificates (Maybe Text)
dcCertificateIdentifier =
    lens _dcCertificateIdentifier (\s a -> s { _dcCertificateIdentifier = a })

-- | This parameter is not currently supported.
dcFilters :: Lens' DescribeCertificates [Filter]
dcFilters = lens _dcFilters (\s a -> s { _dcFilters = a }) . _List

-- | An optional pagination token provided by a previous 'DescribeCertificates'
-- request. If this parameter is specified, the response includes only records
-- beyond the marker, up to the value specified by 'MaxRecords'.
dcMarker :: Lens' DescribeCertificates (Maybe Text)
dcMarker = lens _dcMarker (\s a -> s { _dcMarker = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified 'MaxRecords' value, a pagination token called a marker
-- is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100
dcMaxRecords :: Lens' DescribeCertificates (Maybe Int)
dcMaxRecords = lens _dcMaxRecords (\s a -> s { _dcMaxRecords = a })

data DescribeCertificatesResponse = DescribeCertificatesResponse
    { _dcrCertificates :: List "member" Certificate
    , _dcrMarker       :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'DescribeCertificatesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcrCertificates' @::@ ['Certificate']
--
-- * 'dcrMarker' @::@ 'Maybe' 'Text'
--
describeCertificatesResponse :: DescribeCertificatesResponse
describeCertificatesResponse = DescribeCertificatesResponse
    { _dcrCertificates = mempty
    , _dcrMarker       = Nothing
    }

-- | The list of 'Certificate' objects for the AWS account.
dcrCertificates :: Lens' DescribeCertificatesResponse [Certificate]
dcrCertificates = lens _dcrCertificates (\s a -> s { _dcrCertificates = a }) . _List

-- | An optional pagination token provided by a previous 'DescribeCertificates'
-- request. If this parameter is specified, the response includes only records
-- beyond the marker, up to the value specified by 'MaxRecords' .
dcrMarker :: Lens' DescribeCertificatesResponse (Maybe Text)
dcrMarker = lens _dcrMarker (\s a -> s { _dcrMarker = a })

instance ToPath DescribeCertificates where
    toPath = const "/"

instance ToQuery DescribeCertificates where
    toQuery DescribeCertificates{..} = mconcat
        [ "CertificateIdentifier" =? _dcCertificateIdentifier
        , "Filters"               =? _dcFilters
        , "Marker"                =? _dcMarker
        , "MaxRecords"            =? _dcMaxRecords
        ]

instance ToHeaders DescribeCertificates

instance AWSRequest DescribeCertificates where
    type Sv DescribeCertificates = RDS
    type Rs DescribeCertificates = DescribeCertificatesResponse

    request  = post "DescribeCertificates"
    response = xmlResponse

instance FromXML DescribeCertificatesResponse where
    parseXML = withElement "DescribeCertificatesResult" $ \x -> DescribeCertificatesResponse
        <$> x .@? "Certificates" .!@ mempty
        <*> x .@? "Marker"
