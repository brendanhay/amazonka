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

-- Module      : Network.AWS.CloudFront.ListCloudFrontOriginAccessIdentities
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | List origin access identities.
module Network.AWS.CloudFront.ListCloudFrontOriginAccessIdentities
    (
    -- * Request
      ListCloudFrontOriginAccessIdentities
    -- ** Request constructor
    , listCloudFrontOriginAccessIdentities2014_05_31
    -- ** Request lenses
    , lcfoaiMarker
    , lcfoaiMaxItems

    -- * Response
    , ListCloudFrontOriginAccessIdentitiesResult
    -- ** Response constructor
    , listCloudFrontOriginAccessIdentities2014_05_31Response
    -- ** Response lenses
    , lcfoairCloudFrontOriginAccessIdentityList
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CloudFront.Types

data ListCloudFrontOriginAccessIdentities = ListCloudFrontOriginAccessIdentities
    { _lcfoaiMarker   :: Maybe Text
    , _lcfoaiMaxItems :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListCloudFrontOriginAccessIdentities' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lcfoaiMarker' @::@ 'Maybe' 'Text'
--
-- * 'lcfoaiMaxItems' @::@ 'Maybe' 'Text'
--
listCloudFrontOriginAccessIdentities2014_05_31 :: ListCloudFrontOriginAccessIdentities
listCloudFrontOriginAccessIdentities2014_05_31 = ListCloudFrontOriginAccessIdentities
    { _lcfoaiMarker   = Nothing
    , _lcfoaiMaxItems = Nothing
    }

-- | Use this when paginating results to indicate where to begin in your list
-- of origin access identities. The results include identities in the list
-- that occur after the marker. To get the next page of results, set the
-- Marker to the value of the NextMarker from the current page's response
-- (which is also the ID of the last identity on that page).
lcfoaiMarker :: Lens' ListCloudFrontOriginAccessIdentities (Maybe Text)
lcfoaiMarker = lens _lcfoaiMarker (\s a -> s { _lcfoaiMarker = a })

-- | The maximum number of origin access identities you want in the response
-- body.
lcfoaiMaxItems :: Lens' ListCloudFrontOriginAccessIdentities (Maybe Text)
lcfoaiMaxItems = lens _lcfoaiMaxItems (\s a -> s { _lcfoaiMaxItems = a })

instance ToPath ListCloudFrontOriginAccessIdentities where
    toPath = const "/2014-05-31/origin-access-identity/cloudfront"

instance ToQuery ListCloudFrontOriginAccessIdentities where
    toQuery ListCloudFrontOriginAccessIdentities{..} = mconcat
        [ "Marker"   =? _lcfoaiMarker
        , "MaxItems" =? _lcfoaiMaxItems
        ]

instance ToHeaders ListCloudFrontOriginAccessIdentities

newtype ListCloudFrontOriginAccessIdentitiesResult = ListCloudFrontOriginAccessIdentitiesResult
    { _lcfoairCloudFrontOriginAccessIdentityList :: Maybe CloudFrontOriginAccessIdentityList
    } deriving (Eq, Show, Generic)

-- | 'ListCloudFrontOriginAccessIdentitiesResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lcfoairCloudFrontOriginAccessIdentityList' @::@ 'Maybe' 'CloudFrontOriginAccessIdentityList'
--
listCloudFrontOriginAccessIdentities2014_05_31Response :: ListCloudFrontOriginAccessIdentitiesResult
listCloudFrontOriginAccessIdentities2014_05_31Response = ListCloudFrontOriginAccessIdentitiesResult
    { _lcfoairCloudFrontOriginAccessIdentityList = Nothing
    }

-- | The CloudFrontOriginAccessIdentityList type.
lcfoairCloudFrontOriginAccessIdentityList :: Lens' ListCloudFrontOriginAccessIdentitiesResult (Maybe CloudFrontOriginAccessIdentityList)
lcfoairCloudFrontOriginAccessIdentityList =
    lens _lcfoairCloudFrontOriginAccessIdentityList
        (\s a -> s { _lcfoairCloudFrontOriginAccessIdentityList = a })

instance FromXML ListCloudFrontOriginAccessIdentitiesResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ListCloudFrontOriginAccessIdentitiesResult"
instance AWSRequest ListCloudFrontOriginAccessIdentities where
    type Sv ListCloudFrontOriginAccessIdentities = CloudFront
    type Rs ListCloudFrontOriginAccessIdentities = ListCloudFrontOriginAccessIdentitiesResult

    request  = get
    response = xmlResponse $ \h x -> ListCloudFrontOriginAccessIdentitiesResult
        <$> x %| "CloudFrontOriginAccessIdentityList"
