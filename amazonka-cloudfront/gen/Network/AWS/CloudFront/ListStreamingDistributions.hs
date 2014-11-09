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

-- Module      : Network.AWS.CloudFront.ListStreamingDistributions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | List streaming distributions.
module Network.AWS.CloudFront.ListStreamingDistributions
    (
    -- * Request
      ListStreamingDistributions
    -- ** Request constructor
    , listStreamingDistributions
    -- ** Request lenses
    , lsdMarker
    , lsdMaxItems

    -- * Response
    , ListStreamingDistributionsResult
    -- ** Response constructor
    , listStreamingDistributionsResult
    -- ** Response lenses
    , lsdrStreamingDistributionList
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CloudFront.Types

data ListStreamingDistributions = ListStreamingDistributions
    { _lsdMarker   :: Maybe Text
    , _lsdMaxItems :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListStreamingDistributions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsdMarker' @::@ 'Maybe' 'Text'
--
-- * 'lsdMaxItems' @::@ 'Maybe' 'Text'
--
listStreamingDistributions :: ListStreamingDistributions
listStreamingDistributions = ListStreamingDistributions
    { _lsdMarker   = Nothing
    , _lsdMaxItems = Nothing
    }

-- | Use this when paginating results to indicate where to begin in your list
-- of streaming distributions. The results include distributions in the list
-- that occur after the marker. To get the next page of results, set the
-- Marker to the value of the NextMarker from the current page's response
-- (which is also the ID of the last distribution on that page).
lsdMarker :: Lens' ListStreamingDistributions (Maybe Text)
lsdMarker = lens _lsdMarker (\s a -> s { _lsdMarker = a })

-- | The maximum number of streaming distributions you want in the response
-- body.
lsdMaxItems :: Lens' ListStreamingDistributions (Maybe Text)
lsdMaxItems = lens _lsdMaxItems (\s a -> s { _lsdMaxItems = a })

instance ToPath ListStreamingDistributions where
    toPath = const "/2014-05-31/streaming-distribution"

instance ToQuery ListStreamingDistributions where
    toQuery ListStreamingDistributions{..} = mconcat
        [ "Marker"   =? _lsdMarker
        , "MaxItems" =? _lsdMaxItems
        ]

instance ToHeaders ListStreamingDistributions

newtype ListStreamingDistributionsResult = ListStreamingDistributionsResult
    { _lsdrStreamingDistributionList :: Maybe StreamingDistributionList
    } deriving (Eq, Show, Generic)

-- | 'ListStreamingDistributionsResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsdrStreamingDistributionList' @::@ 'Maybe' 'StreamingDistributionList'
--
listStreamingDistributionsResult :: ListStreamingDistributionsResult
listStreamingDistributionsResult = ListStreamingDistributionsResult
    { _lsdrStreamingDistributionList = Nothing
    }

-- | The StreamingDistributionList type.
lsdrStreamingDistributionList :: Lens' ListStreamingDistributionsResult (Maybe StreamingDistributionList)
lsdrStreamingDistributionList =
    lens _lsdrStreamingDistributionList
        (\s a -> s { _lsdrStreamingDistributionList = a })

instance AWSRequest ListStreamingDistributions where
    type Sv ListStreamingDistributions = CloudFront
    type Rs ListStreamingDistributions = ListStreamingDistributionsResult

    request  = get
    response = const . xmlResponse $ \h x -> ListStreamingDistributionsResult
newtype
