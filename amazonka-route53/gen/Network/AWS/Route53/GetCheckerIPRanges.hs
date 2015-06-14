{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Route53.GetCheckerIPRanges
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | To retrieve a list of the IP ranges used by Amazon Route 53 health
-- checkers to check the health of your resources, send a @GET@ request to
-- the @2013-04-01\/checkeripranges@ resource. You can use these IP
-- addresses to configure router and firewall rules to allow health
-- checkers to check the health of your resources.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_GetCheckerIPRanges.html>
module Network.AWS.Route53.GetCheckerIPRanges
    (
    -- * Request
      GetCheckerIPRanges
    -- ** Request constructor
    , getCheckerIPRanges

    -- * Response
    , GetCheckerIPRangesResponse
    -- ** Response constructor
    , getCheckerIPRangesResponse
    -- ** Response lenses
    , gcirrCheckerIPRanges
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.Route53.Types

-- | /See:/ 'getCheckerIPRanges' smart constructor.
data GetCheckerIPRanges = GetCheckerIPRanges' deriving (Eq, Read, Show)

-- | 'GetCheckerIPRanges' smart constructor.
getCheckerIPRanges :: GetCheckerIPRanges
getCheckerIPRanges = GetCheckerIPRanges';

instance AWSRequest GetCheckerIPRanges where
        type Sv GetCheckerIPRanges = Route53
        type Rs GetCheckerIPRanges =
             GetCheckerIPRangesResponse
        request = get
        response
          = receiveXML
              (\ s h x ->
                 GetCheckerIPRangesResponse' <$>
                   (x .@? "CheckerIpRanges" .!@ mempty >>=
                      parseXMLList "member"))

instance ToHeaders GetCheckerIPRanges where
        toHeaders = const mempty

instance ToPath GetCheckerIPRanges where
        toPath = const "/2013-04-01/checkeripranges"

instance ToQuery GetCheckerIPRanges where
        toQuery = const mempty

-- | /See:/ 'getCheckerIPRangesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gcirrCheckerIPRanges'
newtype GetCheckerIPRangesResponse = GetCheckerIPRangesResponse'{_gcirrCheckerIPRanges :: [Text]} deriving (Eq, Read, Show)

-- | 'GetCheckerIPRangesResponse' smart constructor.
getCheckerIPRangesResponse :: [Text] -> GetCheckerIPRangesResponse
getCheckerIPRangesResponse pCheckerIPRanges = GetCheckerIPRangesResponse'{_gcirrCheckerIPRanges = pCheckerIPRanges};

-- | A complex type that contains sorted list of IP ranges in CIDR format for
-- Amazon Route 53 health checkers.
gcirrCheckerIPRanges :: Lens' GetCheckerIPRangesResponse [Text]
gcirrCheckerIPRanges = lens _gcirrCheckerIPRanges (\ s a -> s{_gcirrCheckerIPRanges = a});
