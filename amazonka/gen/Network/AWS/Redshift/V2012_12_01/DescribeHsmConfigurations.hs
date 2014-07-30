{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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
module Network.AWS.Redshift.V2012_12_01.DescribeHsmConfigurations where

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
    } deriving (Generic)

instance ToQuery DescribeHsmConfigurations where
    toQuery = genericToQuery def

instance AWSRequest DescribeHsmConfigurations where
    type Sv DescribeHsmConfigurations = Redshift
    type Rs DescribeHsmConfigurations = DescribeHsmConfigurationsResponse

    request = post "DescribeHsmConfigurations"
    response _ = xmlResponse

instance AWSPager DescribeHsmConfigurations where
    next rq rs = (\x -> rq { _dhcnMarker = Just x })
        <$> _hcmMarker rs

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
    } deriving (Generic)

instance FromXML DescribeHsmConfigurationsResponse where
    fromXMLOptions = xmlOptions
