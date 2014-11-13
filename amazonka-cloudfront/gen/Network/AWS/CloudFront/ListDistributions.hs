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

-- Module      : Network.AWS.CloudFront.ListDistributions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | List distributions.
module Network.AWS.CloudFront.ListDistributions
    (
    -- * Request
      ListDistributions2014_05_31
    -- ** Request constructor
    , listDistributions2014_05_31
    -- ** Request lenses
    , ldMarker
    , ldMaxItems

    -- * Response
    , ListDistributions2014_05_31Response
    -- ** Response constructor
    , listDistributions2014_05_31Response
    -- ** Response lenses
    , ldrDistributionList
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CloudFront.Types

data ListDistributions2014_05_31 = ListDistributions2014_05_31
    { _ldMarker   :: Maybe Text
    , _ldMaxItems :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListDistributions2014_05_31' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldMarker' @::@ 'Maybe' 'Text'
--
-- * 'ldMaxItems' @::@ 'Maybe' 'Text'
--
listDistributions2014_05_31 :: ListDistributions2014_05_31
listDistributions2014_05_31 = ListDistributions2014_05_31
    { _ldMarker   = Nothing
    , _ldMaxItems = Nothing
    }

-- | Use this when paginating results to indicate where to begin in your list
-- of distributions. The results include distributions in the list that
-- occur after the marker. To get the next page of results, set the Marker
-- to the value of the NextMarker from the current page's response (which is
-- also the ID of the last distribution on that page).
ldMarker :: Lens' ListDistributions2014_05_31 (Maybe Text)
ldMarker = lens _ldMarker (\s a -> s { _ldMarker = a })

-- | The maximum number of distributions you want in the response body.
ldMaxItems :: Lens' ListDistributions2014_05_31 (Maybe Text)
ldMaxItems = lens _ldMaxItems (\s a -> s { _ldMaxItems = a })

instance ToPath ListDistributions2014_05_31 where
    toPath = const "/2014-05-31/distribution"

instance ToQuery ListDistributions2014_05_31 where
    toQuery ListDistributions2014_05_31{..} = mconcat
        [ "Marker"   =? _ldMarker
        , "MaxItems" =? _ldMaxItems
        ]

instance ToHeaders ListDistributions2014_05_31

newtype ListDistributions2014_05_31Response = ListDistributions2014_05_31Response
    { _ldrDistributionList :: Maybe DistributionList
    } deriving (Eq, Show, Generic)

-- | 'ListDistributions2014_05_31Response' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldrDistributionList' @::@ 'Maybe' 'DistributionList'
--
listDistributions2014_05_31Response :: ListDistributions2014_05_31Response
listDistributions2014_05_31Response = ListDistributions2014_05_31Response
    { _ldrDistributionList = Nothing
    }

-- | The DistributionList type.
ldrDistributionList :: Lens' ListDistributions2014_05_31Response (Maybe DistributionList)
ldrDistributionList =
    lens _ldrDistributionList (\s a -> s { _ldrDistributionList = a })

instance AWSRequest ListDistributions2014_05_31 where
    type Sv ListDistributions2014_05_31 = CloudFront
    type Rs ListDistributions2014_05_31 = ListDistributions2014_05_31Response

    request  = get
    response = xmlResponse $ \h x -> ListDistributions2014_05_31Response
        <$> x %| "DistributionList"
