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
    , describeHsmClientCertificates
    -- ** Request lenses
    , dhccnMaxRecords
    , dhccnHsmClientCertificateIdentifier
    , dhccnMarker

    -- * Response
    , DescribeHsmClientCertificatesResponse
    -- ** Response lenses
    , hccmHsmClientCertificates
    , hccmMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeHsmClientCertificates' request.
describeHsmClientCertificates :: DescribeHsmClientCertificates
describeHsmClientCertificates = DescribeHsmClientCertificates
    { _dhccnMaxRecords = Nothing
    , _dhccnHsmClientCertificateIdentifier = Nothing
    , _dhccnMarker = Nothing
    }

data DescribeHsmClientCertificates = DescribeHsmClientCertificates
    { _dhccnMaxRecords :: Maybe Integer
      -- ^ The maximum number of response records to return in each call. If
      -- the number of remaining response records exceeds the specified
      -- MaxRecords value, a value is returned in a marker field of the
      -- response. You can retrieve the next set of records by retrying
      -- the command with the returned marker value. Default: 100
      -- Constraints: minimum 20, maximum 100.
    , _dhccnHsmClientCertificateIdentifier :: Maybe Text
      -- ^ The identifier of a specific HSM client certificate for which you
      -- want information. If no identifier is specified, information is
      -- returned for all HSM client certificates owned by your AWS
      -- customer account.
    , _dhccnMarker :: Maybe Text
      -- ^ An optional parameter that specifies the starting point to return
      -- a set of response records. When the results of a
      -- DescribeHsmClientCertificates request exceed the value specified
      -- in MaxRecords, AWS returns a value in the Marker field of the
      -- response. You can retrieve the next set of response records by
      -- providing the returned marker value in the Marker parameter and
      -- retrying the request.
    } deriving (Show, Generic)

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified MaxRecords
-- value, a value is returned in a marker field of the response. You can
-- retrieve the next set of records by retrying the command with the returned
-- marker value. Default: 100 Constraints: minimum 20, maximum 100.
dhccnMaxRecords
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DescribeHsmClientCertificates
    -> f DescribeHsmClientCertificates
dhccnMaxRecords f x =
    (\y -> x { _dhccnMaxRecords = y })
       <$> f (_dhccnMaxRecords x)
{-# INLINE dhccnMaxRecords #-}

-- | The identifier of a specific HSM client certificate for which you want
-- information. If no identifier is specified, information is returned for all
-- HSM client certificates owned by your AWS customer account.
dhccnHsmClientCertificateIdentifier
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeHsmClientCertificates
    -> f DescribeHsmClientCertificates
dhccnHsmClientCertificateIdentifier f x =
    (\y -> x { _dhccnHsmClientCertificateIdentifier = y })
       <$> f (_dhccnHsmClientCertificateIdentifier x)
{-# INLINE dhccnHsmClientCertificateIdentifier #-}

-- | An optional parameter that specifies the starting point to return a set of
-- response records. When the results of a DescribeHsmClientCertificates
-- request exceed the value specified in MaxRecords, AWS returns a value in
-- the Marker field of the response. You can retrieve the next set of response
-- records by providing the returned marker value in the Marker parameter and
-- retrying the request.
dhccnMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeHsmClientCertificates
    -> f DescribeHsmClientCertificates
dhccnMarker f x =
    (\y -> x { _dhccnMarker = y })
       <$> f (_dhccnMarker x)
{-# INLINE dhccnMarker #-}

instance ToQuery DescribeHsmClientCertificates where
    toQuery = genericQuery def

data DescribeHsmClientCertificatesResponse = DescribeHsmClientCertificatesResponse
    { _hccmHsmClientCertificates :: [HsmClientCertificate]
      -- ^ A list of the identifiers for one or more HSM client certificates
      -- used by Amazon Redshift clusters to store and retrieve database
      -- encryption keys in an HSM.
    , _hccmMarker :: Maybe Text
      -- ^ A value that indicates the starting point for the next set of
      -- response records in a subsequent request. If a value is returned
      -- in a response, you can retrieve the next set of records by
      -- providing this returned marker value in the Marker parameter and
      -- retrying the command. If the Marker field is empty, all response
      -- records have been retrieved for the request.
    } deriving (Show, Generic)

-- | A list of the identifiers for one or more HSM client certificates used by
-- Amazon Redshift clusters to store and retrieve database encryption keys in
-- an HSM.
hccmHsmClientCertificates
    :: Functor f
    => ([HsmClientCertificate]
    -> f ([HsmClientCertificate]))
    -> DescribeHsmClientCertificatesResponse
    -> f DescribeHsmClientCertificatesResponse
hccmHsmClientCertificates f x =
    (\y -> x { _hccmHsmClientCertificates = y })
       <$> f (_hccmHsmClientCertificates x)
{-# INLINE hccmHsmClientCertificates #-}

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response, you
-- can retrieve the next set of records by providing this returned marker
-- value in the Marker parameter and retrying the command. If the Marker field
-- is empty, all response records have been retrieved for the request.
hccmMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeHsmClientCertificatesResponse
    -> f DescribeHsmClientCertificatesResponse
hccmMarker f x =
    (\y -> x { _hccmMarker = y })
       <$> f (_hccmMarker x)
{-# INLINE hccmMarker #-}

instance FromXML DescribeHsmClientCertificatesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeHsmClientCertificates where
    type Sv DescribeHsmClientCertificates = Redshift
    type Rs DescribeHsmClientCertificates = DescribeHsmClientCertificatesResponse

    request = post "DescribeHsmClientCertificates"
    response _ = xmlResponse

instance AWSPager DescribeHsmClientCertificates where
    next rq rs = (\x -> rq { _dhccnMarker = Just x })
        <$> (_hccmMarker rs)
