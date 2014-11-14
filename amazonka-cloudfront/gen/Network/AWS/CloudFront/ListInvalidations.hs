{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.CloudFront.ListInvalidations
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | List invalidation batches.
module Network.AWS.CloudFront.ListInvalidations
    (
    -- * Request
      ListInvalidations2014_05_31
    -- ** Request constructor
    , listInvalidations2014_05_31
    -- ** Request lenses
    , liDistributionId
    , liMarker
    , liMaxItems

    -- * Response
    , ListInvalidations2014_05_31Response
    -- ** Response constructor
    , listInvalidations2014_05_31Response
    -- ** Response lenses
    , lirInvalidationList
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CloudFront.Types
import qualified GHC.Exts

data ListInvalidations2014_05_31 = ListInvalidations2014_05_31
    { _liDistributionId :: Text
    , _liMarker         :: Maybe Text
    , _liMaxItems       :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListInvalidations2014_05_31' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'liDistributionId' @::@ 'Text'
--
-- * 'liMarker' @::@ 'Maybe' 'Text'
--
-- * 'liMaxItems' @::@ 'Maybe' 'Text'
--
listInvalidations2014_05_31 :: Text -- ^ 'liDistributionId'
                            -> ListInvalidations2014_05_31
listInvalidations2014_05_31 p1 = ListInvalidations2014_05_31
    { _liDistributionId = p1
    , _liMarker         = Nothing
    , _liMaxItems       = Nothing
    }

-- | The distribution's id.
liDistributionId :: Lens' ListInvalidations2014_05_31 Text
liDistributionId = lens _liDistributionId (\s a -> s { _liDistributionId = a })

-- | Use this parameter when paginating results to indicate where to begin in
-- your list of invalidation batches. Because the results are returned in
-- decreasing order from most recent to oldest, the most recent results are
-- on the first page, the second page will contain earlier results, and so
-- on. To get the next page of results, set the Marker to the value of the
-- NextMarker from the current page's response. This value is the same as
-- the ID of the last invalidation batch on that page.
liMarker :: Lens' ListInvalidations2014_05_31 (Maybe Text)
liMarker = lens _liMarker (\s a -> s { _liMarker = a })

-- | The maximum number of invalidation batches you want in the response body.
liMaxItems :: Lens' ListInvalidations2014_05_31 (Maybe Text)
liMaxItems = lens _liMaxItems (\s a -> s { _liMaxItems = a })

instance ToPath ListInvalidations2014_05_31 where
    toPath ListInvalidations2014_05_31{..} = mconcat
        [ "/2014-05-31/distribution/"
        , toText _liDistributionId
        , "/invalidation"
        ]

instance ToQuery ListInvalidations2014_05_31 where
    toQuery ListInvalidations2014_05_31{..} = mconcat
        [ "Marker"   =? _liMarker
        , "MaxItems" =? _liMaxItems
        ]

instance ToHeaders ListInvalidations2014_05_31

newtype ListInvalidations2014_05_31Response = ListInvalidations2014_05_31Response
    { _lirInvalidationList :: Maybe InvalidationList
    } deriving (Eq, Show, Generic)

-- | 'ListInvalidations2014_05_31Response' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lirInvalidationList' @::@ 'Maybe' 'InvalidationList'
--
listInvalidations2014_05_31Response :: ListInvalidations2014_05_31Response
listInvalidations2014_05_31Response = ListInvalidations2014_05_31Response
    { _lirInvalidationList = Nothing
    }

-- | Information about invalidation batches.
lirInvalidationList :: Lens' ListInvalidations2014_05_31Response (Maybe InvalidationList)
lirInvalidationList =
    lens _lirInvalidationList (\s a -> s { _lirInvalidationList = a })

instance AWSRequest ListInvalidations2014_05_31 where
    type Sv ListInvalidations2014_05_31 = CloudFront
    type Rs ListInvalidations2014_05_31 = ListInvalidations2014_05_31Response

    request  = get
    response = xmlResponse $ \h x -> ListInvalidations2014_05_31Response
        <$> x %| "InvalidationList"
