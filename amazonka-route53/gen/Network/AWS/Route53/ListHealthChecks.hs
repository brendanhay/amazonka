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

-- Module      : Network.AWS.Route53.ListHealthChecks
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | To retrieve a list of your health checks, send a GET request to the
-- 2013-04-01/healthcheck resource. The response to this request includes a
-- HealthChecks element with zero, one, or multiple HealthCheck child
-- elements. By default, the list of health checks is displayed on a single
-- page. You can control the length of the page that is displayed by using the
-- MaxItems parameter. You can use the Marker parameter to control the health
-- check that the list begins with. Amazon Route 53 returns a maximum of 100
-- items. If you set MaxItems to a value greater than 100, Amazon Route 53
-- returns only the first 100.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_ListHealthChecks.html>
module Network.AWS.Route53.ListHealthChecks
    (
    -- * Request
      ListHealthChecks
    -- ** Request constructor
    , listHealthChecks
    -- ** Request lenses
    , lhcMarker
    , lhcMaxItems

    -- * Response
    , ListHealthChecksResponse
    -- ** Response constructor
    , listHealthChecksResponse
    -- ** Response lenses
    , lhcrHealthChecks
    , lhcrIsTruncated
    , lhcrMarker
    , lhcrMaxItems
    , lhcrNextMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestXML
import Network.AWS.Route53.Types
import qualified GHC.Exts

data ListHealthChecks = ListHealthChecks
    { _lhcMarker   :: Maybe Text
    , _lhcMaxItems :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'ListHealthChecks' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lhcMarker' @::@ 'Maybe' 'Text'
--
-- * 'lhcMaxItems' @::@ 'Maybe' 'Text'
--
listHealthChecks :: ListHealthChecks
listHealthChecks = ListHealthChecks
    { _lhcMarker   = Nothing
    , _lhcMaxItems = Nothing
    }

-- | If the request returned more than one page of results, submit another
-- request and specify the value of NextMarker from the last response in the
-- marker parameter to get the next page of results.
lhcMarker :: Lens' ListHealthChecks (Maybe Text)
lhcMarker = lens _lhcMarker (\s a -> s { _lhcMarker = a })

-- | Specify the maximum number of health checks to return per page of
-- results.
lhcMaxItems :: Lens' ListHealthChecks (Maybe Text)
lhcMaxItems = lens _lhcMaxItems (\s a -> s { _lhcMaxItems = a })

data ListHealthChecksResponse = ListHealthChecksResponse
    { _lhcrHealthChecks :: List "HealthCheck" HealthCheck
    , _lhcrIsTruncated  :: Bool
    , _lhcrMarker       :: Text
    , _lhcrMaxItems     :: Text
    , _lhcrNextMarker   :: Maybe Text
    } deriving (Eq, Show)

-- | 'ListHealthChecksResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lhcrHealthChecks' @::@ ['HealthCheck']
--
-- * 'lhcrIsTruncated' @::@ 'Bool'
--
-- * 'lhcrMarker' @::@ 'Text'
--
-- * 'lhcrMaxItems' @::@ 'Text'
--
-- * 'lhcrNextMarker' @::@ 'Maybe' 'Text'
--
listHealthChecksResponse :: Text -- ^ 'lhcrMarker'
                         -> Bool -- ^ 'lhcrIsTruncated'
                         -> Text -- ^ 'lhcrMaxItems'
                         -> ListHealthChecksResponse
listHealthChecksResponse p1 p2 p3 = ListHealthChecksResponse
    { _lhcrMarker       = p1
    , _lhcrIsTruncated  = p2
    , _lhcrMaxItems     = p3
    , _lhcrHealthChecks = mempty
    , _lhcrNextMarker   = Nothing
    }

-- | A complex type that contains information about the health checks
-- associated with the current AWS account.
lhcrHealthChecks :: Lens' ListHealthChecksResponse [HealthCheck]
lhcrHealthChecks = lens _lhcrHealthChecks (\s a -> s { _lhcrHealthChecks = a }) . _List

-- | A flag indicating whether there are more health checks to be listed. If
-- your results were truncated, you can make a follow-up request for the
-- next page of results by using the Marker element. Valid Values: true |
-- false.
lhcrIsTruncated :: Lens' ListHealthChecksResponse Bool
lhcrIsTruncated = lens _lhcrIsTruncated (\s a -> s { _lhcrIsTruncated = a })

-- | If the request returned more than one page of results, submit another
-- request and specify the value of NextMarker from the last response in the
-- marker parameter to get the next page of results.
lhcrMarker :: Lens' ListHealthChecksResponse Text
lhcrMarker = lens _lhcrMarker (\s a -> s { _lhcrMarker = a })

-- | The maximum number of health checks to be included in the response body.
-- If the number of health checks associated with this AWS account exceeds
-- MaxItems, the value of ListHealthChecksResponse$IsTruncated in the
-- response is true. Call ListHealthChecks again and specify the value of
-- ListHealthChecksResponse$NextMarker in the ListHostedZonesRequest$Marker
-- element to get the next page of results.
lhcrMaxItems :: Lens' ListHealthChecksResponse Text
lhcrMaxItems = lens _lhcrMaxItems (\s a -> s { _lhcrMaxItems = a })

-- | Indicates where to continue listing health checks. If
-- ListHealthChecksResponse$IsTruncated is true, make another request to
-- ListHealthChecks and include the value of the NextMarker element in the
-- Marker element to get the next page of results.
lhcrNextMarker :: Lens' ListHealthChecksResponse (Maybe Text)
lhcrNextMarker = lens _lhcrNextMarker (\s a -> s { _lhcrNextMarker = a })

instance ToPath ListHealthChecks where
    toPath = const "/2013-04-01/healthcheck"

instance ToQuery ListHealthChecks where
    toQuery ListHealthChecks{..} = mconcat
        [ "marker"   =? _lhcMarker
        , "maxitems" =? _lhcMaxItems
        ]

instance ToHeaders ListHealthChecks

instance ToXMLRoot ListHealthChecks where
    toXMLRoot = const (namespaced ns "ListHealthChecks" [])

instance ToXML ListHealthChecks

instance AWSRequest ListHealthChecks where
    type Sv ListHealthChecks = Route53
    type Rs ListHealthChecks = ListHealthChecksResponse

    request  = get
    response = xmlResponse

instance FromXML ListHealthChecksResponse where
    parseXML x = ListHealthChecksResponse
        <$> x .@? "HealthChecks"
        <*> x .@  "IsTruncated"
        <*> x .@  "Marker"
        <*> x .@  "MaxItems"
        <*> x .@? "NextMarker"

instance AWSPager ListHealthChecks where
    page rq rs
        | stop (rs ^. lhcrIsTruncated) = Nothing
        | otherwise = Just $ rq
            & lhcMarker .~ rs ^. lhcrNextMarker
