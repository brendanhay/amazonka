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

-- Module      : Network.AWS.CloudFront.ListInvalidations2014_05_31
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | List invalidation batches.
module Network.AWS.CloudFront.ListInvalidations2014_05_31
    (
    -- * Request
      ListInvalidations
    -- ** Request constructor
    , listInvalidations
    -- ** Request lenses
    , liDistributionId
    , liMarker
    , liMaxItems

    -- * Response
    , ListInvalidationsResult
    -- ** Response constructor
    , listInvalidationsResult
    -- ** Response lenses
    , lirInvalidationList
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CloudFront.Types

data ListInvalidations = ListInvalidations
    { _liDistributionId :: Text
    , _liMarker         :: Maybe Text
    , _liMaxItems       :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListInvalidations' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'liDistributionId' @::@ 'Text'
--
-- * 'liMarker' @::@ 'Maybe' 'Text'
--
-- * 'liMaxItems' @::@ 'Maybe' 'Text'
--
listInvalidations :: Text -- ^ 'liDistributionId'
                  -> ListInvalidations
listInvalidations p1 = ListInvalidations
    { _liDistributionId = p1
    , _liMarker         = Nothing
    , _liMaxItems       = Nothing
    }

-- | The distribution's id.
liDistributionId :: Lens' ListInvalidations Text
liDistributionId = lens _liDistributionId (\s a -> s { _liDistributionId = a })

-- | Use this parameter when paginating results to indicate where to begin in
-- your list of invalidation batches. Because the results are returned in
-- decreasing order from most recent to oldest, the most recent results are
-- on the first page, the second page will contain earlier results, and so
-- on. To get the next page of results, set the Marker to the value of the
-- NextMarker from the current page's response. This value is the same as
-- the ID of the last invalidation batch on that page.
liMarker :: Lens' ListInvalidations (Maybe Text)
liMarker = lens _liMarker (\s a -> s { _liMarker = a })

-- | The maximum number of invalidation batches you want in the response body.
liMaxItems :: Lens' ListInvalidations (Maybe Text)
liMaxItems = lens _liMaxItems (\s a -> s { _liMaxItems = a })

instance ToPath ListInvalidations where
    toPath ListInvalidations{..} = mconcat
        [ "/2014-05-31/distribution/"
        , toText _liDistributionId
        , "/invalidation"
        ]

instance ToQuery ListInvalidations where
    toQuery ListInvalidations{..} = mconcat
        [ "Marker"   =? _liMarker
        , "MaxItems" =? _liMaxItems
        ]

instance ToHeaders ListInvalidations

newtype ListInvalidationsResult = ListInvalidationsResult
    { _lirInvalidationList :: Maybe InvalidationList
    } deriving (Eq, Show, Generic)

-- | 'ListInvalidationsResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lirInvalidationList' @::@ 'Maybe' 'InvalidationList'
--
listInvalidationsResult :: ListInvalidationsResult
listInvalidationsResult = ListInvalidationsResult
    { _lirInvalidationList = Nothing
    }

-- | Information about invalidation batches.
lirInvalidationList :: Lens' ListInvalidationsResult (Maybe InvalidationList)
lirInvalidationList =
    lens _lirInvalidationList (\s a -> s { _lirInvalidationList = a })

instance AWSRequest ListInvalidations where
    type Sv ListInvalidations = CloudFront
    type Rs ListInvalidations = ListInvalidationsResult

    request  = get
    response = const . xmlResponse $ \h x -> ListInvalidationsResult
        <$> x %| "InvalidationList"
