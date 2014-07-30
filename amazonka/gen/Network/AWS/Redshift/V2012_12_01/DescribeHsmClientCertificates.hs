{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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
module Network.AWS.Redshift.V2012_12_01.DescribeHsmClientCertificates where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error, Endpoint, Region)
import           Network.AWS.Request.Query
import           Network.AWS.Redshift.V2012_12_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

-- | Minimum specification for a 'DescribeHsmClientCertificates' request.
describeHsmClientCertificates :: DescribeHsmClientCertificates
describeHsmClientCertificates = DescribeHsmClientCertificates
    { _dhccmMaxRecords = Nothing
    , _dhccmHsmClientCertificateIdentifier = Nothing
    , _dhccmMarker = Nothing
    }

data DescribeHsmClientCertificates = DescribeHsmClientCertificates
    { _dhccmMaxRecords :: Maybe Integer
      -- ^ The maximum number of response records to return in each call. If
      -- the number of remaining response records exceeds the specified
      -- MaxRecords value, a value is returned in a marker field of the
      -- response. You can retrieve the next set of records by retrying
      -- the command with the returned marker value. Default: 100
      -- Constraints: minimum 20, maximum 100.
    , _dhccmHsmClientCertificateIdentifier :: Maybe Text
      -- ^ The identifier of a specific HSM client certificate for which you
      -- want information. If no identifier is specified, information is
      -- returned for all HSM client certificates owned by your AWS
      -- customer account.
    , _dhccmMarker :: Maybe Text
      -- ^ An optional parameter that specifies the starting point to return
      -- a set of response records. When the results of a
      -- DescribeHsmClientCertificates request exceed the value specified
      -- in MaxRecords, AWS returns a value in the Marker field of the
      -- response. You can retrieve the next set of response records by
      -- providing the returned marker value in the Marker parameter and
      -- retrying the request.
    } deriving (Generic)

instance ToQuery DescribeHsmClientCertificates where
    toQuery = genericToQuery def

instance AWSRequest DescribeHsmClientCertificates where
    type Sv DescribeHsmClientCertificates = Redshift
    type Rs DescribeHsmClientCertificates = DescribeHsmClientCertificatesResponse

    request = post "DescribeHsmClientCertificates"
    response _ = xmlResponse

instance AWSPager DescribeHsmClientCertificates where
    next rq rs = (\x -> rq { _dhccmMarker = Just x })
        <$> _hccmMarker rs

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
    } deriving (Generic)

instance FromXML DescribeHsmClientCertificatesResponse where
    fromXMLOptions = xmlOptions
