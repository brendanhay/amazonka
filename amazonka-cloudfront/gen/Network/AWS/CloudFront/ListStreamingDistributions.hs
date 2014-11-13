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
      ListStreamingDistributions2014_05_31
    -- ** Request constructor
    , listStreamingDistributions2014_05_31
    -- ** Request lenses
    , lsdMarker
    , lsdMaxItems

    -- * Response
    , ListStreamingDistributions2014_05_31Response
    -- ** Response constructor
    , listStreamingDistributions2014_05_31Response
    -- ** Response lenses
    , lsdrStreamingDistributionList
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CloudFront.Types
import qualified GHC.Exts

data ListStreamingDistributions2014_05_31 = ListStreamingDistributions2014_05_31
    { _lsdMarker   :: Maybe Text
    , _lsdMaxItems :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListStreamingDistributions2014_05_31' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsdMarker' @::@ 'Maybe' 'Text'
--
-- * 'lsdMaxItems' @::@ 'Maybe' 'Text'
--
listStreamingDistributions2014_05_31 :: ListStreamingDistributions2014_05_31
listStreamingDistributions2014_05_31 = ListStreamingDistributions2014_05_31
    { _lsdMarker   = Nothing
    , _lsdMaxItems = Nothing
    }

-- | Use this when paginating results to indicate where to begin in your list
-- of streaming distributions. The results include distributions in the list
-- that occur after the marker. To get the next page of results, set the
-- Marker to the value of the NextMarker from the current page's response
-- (which is also the ID of the last distribution on that page).
lsdMarker :: Lens' ListStreamingDistributions2014_05_31 (Maybe Text)
lsdMarker = lens _lsdMarker (\s a -> s { _lsdMarker = a })

-- | The maximum number of streaming distributions you want in the response
-- body.
lsdMaxItems :: Lens' ListStreamingDistributions2014_05_31 (Maybe Text)
lsdMaxItems = lens _lsdMaxItems (\s a -> s { _lsdMaxItems = a })

instance ToPath ListStreamingDistributions2014_05_31 where
    toPath = const "/2014-05-31/streaming-distribution"

instance ToQuery ListStreamingDistributions2014_05_31 where
    toQuery ListStreamingDistributions2014_05_31{..} = mconcat
        [ "Marker"   =? _lsdMarker
        , "MaxItems" =? _lsdMaxItems
        ]

instance ToHeaders ListStreamingDistributions2014_05_31

newtype ListStreamingDistributions2014_05_31Response = ListStreamingDistributions2014_05_31Response
    { _lsdrStreamingDistributionList :: Maybe StreamingDistributionList
    } deriving (Eq, Show, Generic)

-- | 'ListStreamingDistributions2014_05_31Response' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsdrStreamingDistributionList' @::@ 'Maybe' 'StreamingDistributionList'
--
listStreamingDistributions2014_05_31Response :: ListStreamingDistributions2014_05_31Response
listStreamingDistributions2014_05_31Response = ListStreamingDistributions2014_05_31Response
    { _lsdrStreamingDistributionList = Nothing
    }

-- | The StreamingDistributionList type.
lsdrStreamingDistributionList :: Lens' ListStreamingDistributions2014_05_31Response (Maybe StreamingDistributionList)
lsdrStreamingDistributionList =
    lens _lsdrStreamingDistributionList
        (\s a -> s { _lsdrStreamingDistributionList = a })

instance AWSRequest ListStreamingDistributions2014_05_31 where
    type Sv ListStreamingDistributions2014_05_31 = CloudFront
    type Rs ListStreamingDistributions2014_05_31 = ListStreamingDistributions2014_05_31Response

    request  = get
    response = xmlResponse $ \h x -> ListStreamingDistributions2014_05_31Response
        <$> x %| "StreamingDistributionList"
