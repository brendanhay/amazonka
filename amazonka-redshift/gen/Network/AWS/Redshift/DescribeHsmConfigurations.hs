{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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

-- | Returns information about the specified Amazon Redshift HSM configuration.
-- If no configuration ID is specified, returns information about all the HSM
-- configurations owned by your AWS customer account.
module Network.AWS.Redshift.DescribeHsmConfigurations
    (
    -- * Request
      DescribeHsmConfigurations
    -- ** Request constructor
    , describeHsmConfigurations
    -- ** Request lenses
    , dhc1HsmConfigurationIdentifier
    , dhc1MaxRecords
    , dhc1Marker

    -- * Response
    , DescribeHsmConfigurationsResponse
    -- ** Response constructor
    , describeHsmConfigurationsResponse
    -- ** Response lenses
    , dhcrMarker
    , dhcrHsmConfigurations
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import Network.AWS.Prelude

-- | 
data DescribeHsmConfigurations = DescribeHsmConfigurations
    { _dhc1HsmConfigurationIdentifier :: Maybe Text
    , _dhc1MaxRecords :: Maybe Integer
    , _dhc1Marker :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeHsmConfigurations' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @HsmConfigurationIdentifier ::@ @Maybe Text@
--
-- * @MaxRecords ::@ @Maybe Integer@
--
-- * @Marker ::@ @Maybe Text@
--
describeHsmConfigurations :: DescribeHsmConfigurations
describeHsmConfigurations = DescribeHsmConfigurations
    { _dhc1HsmConfigurationIdentifier = Nothing
    , _dhc1MaxRecords = Nothing
    , _dhc1Marker = Nothing
    }

-- | The identifier of a specific Amazon Redshift HSM configuration to be
-- described. If no identifier is specified, information is returned for all
-- HSM configurations owned by your AWS customer account.
dhc1HsmConfigurationIdentifier :: Lens' DescribeHsmConfigurations (Maybe Text)
dhc1HsmConfigurationIdentifier =
    lens _dhc1HsmConfigurationIdentifier
         (\s a -> s { _dhc1HsmConfigurationIdentifier = a })

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified MaxRecords
-- value, a value is returned in a marker field of the response. You can
-- retrieve the next set of records by retrying the command with the returned
-- marker value. Default: 100 Constraints: minimum 20, maximum 100.
dhc1MaxRecords :: Lens' DescribeHsmConfigurations (Maybe Integer)
dhc1MaxRecords = lens _dhc1MaxRecords (\s a -> s { _dhc1MaxRecords = a })

-- | An optional parameter that specifies the starting point to return a set of
-- response records. When the results of a DescribeHsmConfigurations request
-- exceed the value specified in MaxRecords, AWS returns a value in the Marker
-- field of the response. You can retrieve the next set of response records by
-- providing the returned marker value in the Marker parameter and retrying
-- the request.
dhc1Marker :: Lens' DescribeHsmConfigurations (Maybe Text)
dhc1Marker = lens _dhc1Marker (\s a -> s { _dhc1Marker = a })

instance ToQuery DescribeHsmConfigurations where
    toQuery = genericQuery def

-- | 
data DescribeHsmConfigurationsResponse = DescribeHsmConfigurationsResponse
    { _dhcrMarker :: Maybe Text
    , _dhcrHsmConfigurations :: [HsmConfiguration]
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeHsmConfigurationsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Marker ::@ @Maybe Text@
--
-- * @HsmConfigurations ::@ @[HsmConfiguration]@
--
describeHsmConfigurationsResponse :: DescribeHsmConfigurationsResponse
describeHsmConfigurationsResponse = DescribeHsmConfigurationsResponse
    { _dhcrMarker = Nothing
    , _dhcrHsmConfigurations = mempty
    }

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response, you
-- can retrieve the next set of records by providing this returned marker
-- value in the Marker parameter and retrying the command. If the Marker field
-- is empty, all response records have been retrieved for the request.
dhcrMarker :: Lens' DescribeHsmConfigurationsResponse (Maybe Text)
dhcrMarker = lens _dhcrMarker (\s a -> s { _dhcrMarker = a })

-- | A list of Amazon Redshift HSM configurations.
dhcrHsmConfigurations :: Lens' DescribeHsmConfigurationsResponse [HsmConfiguration]
dhcrHsmConfigurations =
    lens _dhcrHsmConfigurations (\s a -> s { _dhcrHsmConfigurations = a })

instance FromXML DescribeHsmConfigurationsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeHsmConfigurations where
    type Sv DescribeHsmConfigurations = Redshift
    type Rs DescribeHsmConfigurations = DescribeHsmConfigurationsResponse

    request = post "DescribeHsmConfigurations"
    response _ = xmlResponse

instance AWSPager DescribeHsmConfigurations where
    next rq rs = (\x -> rq & dhc1Marker ?~ x)
        <$> (rs ^. dhcrMarker)
