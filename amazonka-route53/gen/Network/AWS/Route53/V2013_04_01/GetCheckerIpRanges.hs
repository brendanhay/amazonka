{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53.V2013_04_01.GetCheckerIpRanges
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
module Network.AWS.Route53.V2013_04_01.GetCheckerIpRanges
    (
    -- * Request
      GetCheckerIpRanges
    -- ** Request constructor
    , getCheckerIpRanges
    -- * Response
    , GetCheckerIpRangesResponse
    -- ** Response lenses
    , gcirsCheckerIpRanges
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.Route53.V2013_04_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'GetCheckerIpRanges' request.
getCheckerIpRanges :: GetCheckerIpRanges
getCheckerIpRanges = GetCheckerIpRanges

data GetCheckerIpRanges = GetCheckerIpRanges
    deriving (Eq, Show, Generic)

instance ToPath GetCheckerIpRanges where
    toPath = const "/2013-04-01/checkeripranges"

instance ToQuery GetCheckerIpRanges

instance ToHeaders GetCheckerIpRanges

instance ToXML GetCheckerIpRanges where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "GetCheckerIpRangesRequest"

data GetCheckerIpRangesResponse = GetCheckerIpRangesResponse
    { _gcirsCheckerIpRanges :: [Text]
      -- ^ A complex type that contains sorted list of IP ranges in CIDR
      -- format for Amazon Route 53 health checkers.
    } deriving (Show, Generic)

-- | A complex type that contains sorted list of IP ranges in CIDR format for
-- Amazon Route 53 health checkers.
gcirsCheckerIpRanges
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> GetCheckerIpRangesResponse
    -> f GetCheckerIpRangesResponse
gcirsCheckerIpRanges f x =
    (\y -> x { _gcirsCheckerIpRanges = y })
       <$> f (_gcirsCheckerIpRanges x)
{-# INLINE gcirsCheckerIpRanges #-}

instance FromXML GetCheckerIpRangesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetCheckerIpRanges where
    type Sv GetCheckerIpRanges = Route53
    type Rs GetCheckerIpRanges = GetCheckerIpRangesResponse

    request = get
    response _ = xmlResponse
