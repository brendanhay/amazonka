{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53.GetCheckerIpRanges
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | To retrieve a list of the IP ranges used by Amazon Route 53 health checkers
-- to check the health of your resources, send a GET request to the
-- 2013-04-01/checkeripranges resource. You can use these IP addresses to
-- configure router and firewall rules to allow health checkers to check the
-- health of your resources.
module Network.AWS.Route53.GetCheckerIpRanges
    (
    -- * Request
      GetCheckerIpRanges
    -- ** Request constructor
    , getCheckerIpRanges
    -- * Response
    , GetCheckerIpRangesResponse
    -- ** Response constructor
    , getCheckerIpRangesResponse
    -- ** Response lenses
    , gcirrCheckerIpRanges
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.Route53.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

-- | Empty request.
data GetCheckerIpRanges = GetCheckerIpRanges
    deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetCheckerIpRanges' request.
getCheckerIpRanges :: GetCheckerIpRanges
getCheckerIpRanges = GetCheckerIpRanges

instance ToPath GetCheckerIpRanges

instance ToQuery GetCheckerIpRanges

instance ToHeaders GetCheckerIpRanges

instance ToXML GetCheckerIpRanges where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "GetCheckerIpRangesRequest"

-- | A complex type that contains the CheckerIpRanges element.
newtype GetCheckerIpRangesResponse = GetCheckerIpRangesResponse
    { _gcirrCheckerIpRanges :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetCheckerIpRangesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CheckerIpRanges ::@ @[Text]@
--
getCheckerIpRangesResponse :: [Text] -- ^ 'gcirrCheckerIpRanges'
                           -> GetCheckerIpRangesResponse
getCheckerIpRangesResponse p1 = GetCheckerIpRangesResponse
    { _gcirrCheckerIpRanges = p1
    }

-- | A complex type that contains sorted list of IP ranges in CIDR format for
-- Amazon Route 53 health checkers.
gcirrCheckerIpRanges :: Lens' GetCheckerIpRangesResponse [Text]
gcirrCheckerIpRanges =
    lens _gcirrCheckerIpRanges (\s a -> s { _gcirrCheckerIpRanges = a })

instance FromXML GetCheckerIpRangesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetCheckerIpRanges where
    type Sv GetCheckerIpRanges = Route53
    type Rs GetCheckerIpRanges = GetCheckerIpRangesResponse

    request = get
    response _ = xmlResponse
