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
      DescribeHsmConfigurationsMessage
    -- ** Request constructor
    , describeHsmConfigurationsMessage
    -- ** Request lenses
    , dhcmHsmConfigurationIdentifier
    , dhcmMarker
    , dhcmMaxRecords

    -- * Response
    , HsmConfigurationMessage
    -- ** Response constructor
    , hsmConfigurationMessage
    -- ** Response lenses
    , hcmHsmConfigurations
    , hcmMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types

data DescribeHsmConfigurationsMessage = DescribeHsmConfigurationsMessage
    { _dhcmHsmConfigurationIdentifier :: Maybe Text
    , _dhcmMarker                     :: Maybe Text
    , _dhcmMaxRecords                 :: Maybe Int
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeHsmConfigurationsMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dhcmHsmConfigurationIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'dhcmMarker' @::@ 'Maybe' 'Text'
--
-- * 'dhcmMaxRecords' @::@ 'Maybe' 'Int'
--
describeHsmConfigurationsMessage :: DescribeHsmConfigurationsMessage
describeHsmConfigurationsMessage = DescribeHsmConfigurationsMessage
    { _dhcmHsmConfigurationIdentifier = Nothing
    , _dhcmMaxRecords                 = Nothing
    , _dhcmMarker                     = Nothing
    }

-- | The identifier of a specific Amazon Redshift HSM configuration to be
-- described. If no identifier is specified, information is returned for all
-- HSM configurations owned by your AWS customer account.
dhcmHsmConfigurationIdentifier :: Lens' DescribeHsmConfigurationsMessage (Maybe Text)
dhcmHsmConfigurationIdentifier =
    lens _dhcmHsmConfigurationIdentifier
        (\s a -> s { _dhcmHsmConfigurationIdentifier = a })

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeHsmConfigurations
-- request exceed the value specified in MaxRecords, AWS returns a value in
-- the Marker field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the Marker
-- parameter and retrying the request.
dhcmMarker :: Lens' DescribeHsmConfigurationsMessage (Maybe Text)
dhcmMarker = lens _dhcmMarker (\s a -> s { _dhcmMarker = a })

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified MaxRecords
-- value, a value is returned in a marker field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value. Default: 100 Constraints: minimum 20, maximum 100.
dhcmMaxRecords :: Lens' DescribeHsmConfigurationsMessage (Maybe Int)
dhcmMaxRecords = lens _dhcmMaxRecords (\s a -> s { _dhcmMaxRecords = a })

instance ToPath DescribeHsmConfigurationsMessage where
    toPath = const "/"

instance ToQuery DescribeHsmConfigurationsMessage

data HsmConfigurationMessage = HsmConfigurationMessage
    { _hcmHsmConfigurations :: [HsmConfiguration]
    , _hcmMarker            :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'HsmConfigurationMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'hcmHsmConfigurations' @::@ ['HsmConfiguration']
--
-- * 'hcmMarker' @::@ 'Maybe' 'Text'
--
hsmConfigurationMessage :: HsmConfigurationMessage
hsmConfigurationMessage = HsmConfigurationMessage
    { _hcmMarker            = Nothing
    , _hcmHsmConfigurations = mempty
    }

-- | A list of Amazon Redshift HSM configurations.
hcmHsmConfigurations :: Lens' HsmConfigurationMessage [HsmConfiguration]
hcmHsmConfigurations =
    lens _hcmHsmConfigurations (\s a -> s { _hcmHsmConfigurations = a })

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the Marker parameter and retrying the command. If the
-- Marker field is empty, all response records have been retrieved for the
-- request.
hcmMarker :: Lens' HsmConfigurationMessage (Maybe Text)
hcmMarker = lens _hcmMarker (\s a -> s { _hcmMarker = a })

instance AWSRequest DescribeHsmConfigurationsMessage where
    type Sv DescribeHsmConfigurationsMessage = Redshift
    type Rs DescribeHsmConfigurationsMessage = HsmConfigurationMessage

    request  = post "DescribeHsmConfigurations"
    response = const . xmlResponse $ \h x -> HsmConfigurationMessage
record
