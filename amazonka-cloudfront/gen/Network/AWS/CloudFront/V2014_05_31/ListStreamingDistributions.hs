{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.V2014_05_31.ListStreamingDistributions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | List streaming distributions.
module Network.AWS.CloudFront.V2014_05_31.ListStreamingDistributions where

import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.V2014_05_31.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ListStreamingDistributions' request.
listStreamingDistributions :: ListStreamingDistributions
listStreamingDistributions = ListStreamingDistributions
    { _lsdrMarker = Nothing
    , _lsdrMaxItems = Nothing
    }

data ListStreamingDistributions = ListStreamingDistributions
    { _lsdrMarker :: Maybe Text
      -- ^ Use this when paginating results to indicate where to begin in
      -- your list of streaming distributions. The results include
      -- distributions in the list that occur after the marker. To get the
      -- next page of results, set the Marker to the value of the
      -- NextMarker from the current page's response (which is also the ID
      -- of the last distribution on that page).
    , _lsdrMaxItems :: Maybe Text
      -- ^ The maximum number of streaming distributions you want in the
      -- response body.
    } deriving (Show, Generic)

makeLenses ''ListStreamingDistributions

instance ToPath ListStreamingDistributions where
    toPath = const "/2014-05-31/streaming-distribution"

instance ToQuery ListStreamingDistributions where
    toQuery ListStreamingDistributions{..} = mconcat
        [ "Marker" =? _lsdrMarker
        , "MaxItems" =? _lsdrMaxItems
        ]

instance ToHeaders ListStreamingDistributions

instance ToXML ListStreamingDistributions where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ListStreamingDistributionsRequest"

data ListStreamingDistributionsResponse = ListStreamingDistributionsResponse
    { _lsdsStreamingDistributionList :: StreamingDistributionList
      -- ^ The StreamingDistributionList type.
    } deriving (Show, Generic)

makeLenses ''ListStreamingDistributionsResponse

instance FromXML ListStreamingDistributionsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListStreamingDistributions where
    type Sv ListStreamingDistributions = CloudFront
    type Rs ListStreamingDistributions = ListStreamingDistributionsResponse

    request = get
    response _ = xmlResponse

instance AWSPager ListStreamingDistributions where
    next rq rs
        | not (_sdlIsTruncated $ _lsdsStreamingDistributionList rs) = Nothing
        | otherwise = Just $ rq
            { _lsdrMarker = _sdlNextMarker $ _lsdsStreamingDistributionList rs
            }
