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

-- Module      : Network.AWS.EC2.DescribeDhcpOptions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes one or more of your DHCP options sets. For more information about
-- DHCP options sets, see DHCP Options Sets in the Amazon Virtual Private
-- Cloud User Guide.
module Network.AWS.EC2.DescribeDhcpOptions
    (
    -- * Request
      DescribeDhcpOptions
    -- ** Request constructor
    , describeDhcpOptions
    -- ** Request lenses
    , ddo1DhcpOptionsIds
    , ddo1DryRun
    , ddo1Filters

    -- * Response
    , DescribeDhcpOptionsResult
    -- ** Response constructor
    , describeDhcpOptionsResult
    -- ** Response lenses
    , ddorDhcpOptions
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data DescribeDhcpOptions = DescribeDhcpOptions
    { _ddo1DhcpOptionsIds :: [Text]
    , _ddo1DryRun         :: Maybe Bool
    , _ddo1Filters        :: [Filter]
    } deriving (Eq, Show, Generic)

-- | 'DescribeDhcpOptions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddo1DhcpOptionsIds' @::@ ['Text']
--
-- * 'ddo1DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'ddo1Filters' @::@ ['Filter']
--
describeDhcpOptions :: DescribeDhcpOptions
describeDhcpOptions = DescribeDhcpOptions
    { _ddo1DryRun         = Nothing
    , _ddo1DhcpOptionsIds = mempty
    , _ddo1Filters        = mempty
    }

-- | The IDs of one or more DHCP options sets. Default: Describes all your
-- DHCP options sets.
ddo1DhcpOptionsIds :: Lens' DescribeDhcpOptions [Text]
ddo1DhcpOptionsIds =
    lens _ddo1DhcpOptionsIds (\s a -> s { _ddo1DhcpOptionsIds = a })

ddo1DryRun :: Lens' DescribeDhcpOptions (Maybe Bool)
ddo1DryRun = lens _ddo1DryRun (\s a -> s { _ddo1DryRun = a })

-- | One or more filters. dhcp-options-id - The ID of a set of DHCP options.
-- key - The key for one of the options (for example, domain-name). value -
-- The value for one of the options. tag:key=value - The key/value
-- combination of a tag assigned to the resource. tag-key - The key of a tag
-- assigned to the resource. This filter is independent of the tag-value
-- filter. For example, if you use both the filter "tag-key=Purpose" and the
-- filter "tag-value=X", you get any resources assigned both the tag key
-- Purpose (regardless of what the tag's value is), and the tag value X
-- (regardless of what the tag's key is). If you want to list only resources
-- where Purpose is X, see the tag:key=value filter. tag-value - The value
-- of a tag assigned to the resource. This filter is independent of the
-- tag-key filter.
ddo1Filters :: Lens' DescribeDhcpOptions [Filter]
ddo1Filters = lens _ddo1Filters (\s a -> s { _ddo1Filters = a })

instance ToQuery DescribeDhcpOptions

instance ToPath DescribeDhcpOptions where
    toPath = const "/"

newtype DescribeDhcpOptionsResult = DescribeDhcpOptionsResult
    { _ddorDhcpOptions :: [DhcpOptions]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance IsList DescribeDhcpOptionsResult
    type Item DescribeDhcpOptionsResult = DhcpOptions

    fromList = DescribeDhcpOptionsResult . fromList
    toList   = toList . _ddorDhcpOptions

-- | 'DescribeDhcpOptionsResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddorDhcpOptions' @::@ ['DhcpOptions']
--
describeDhcpOptionsResult :: DescribeDhcpOptionsResult
describeDhcpOptionsResult = DescribeDhcpOptionsResult
    { _ddorDhcpOptions = mempty
    }

-- | Information about one or more DHCP options sets.
ddorDhcpOptions :: Lens' DescribeDhcpOptionsResult [DhcpOptions]
ddorDhcpOptions = lens _ddorDhcpOptions (\s a -> s { _ddorDhcpOptions = a })

instance FromXML DescribeDhcpOptionsResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DescribeDhcpOptionsResult"

instance AWSRequest DescribeDhcpOptions where
    type Sv DescribeDhcpOptions = EC2
    type Rs DescribeDhcpOptions = DescribeDhcpOptionsResult

    request  = post "DescribeDhcpOptions"
    response = xmlResponse $ \h x -> DescribeDhcpOptionsResult
        <$> x %| "dhcpOptionsSet"
