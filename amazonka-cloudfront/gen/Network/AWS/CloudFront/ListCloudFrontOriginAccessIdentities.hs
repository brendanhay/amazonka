{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

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
      ListCloudFrontOriginAccessIdentities2014_05_31
    -- ** Request constructor
    , listCloudFrontOriginAccessIdentities2014_05_31
    -- ** Request lenses
    , lcfoaiMarker
    , lcfoaiMaxItems

    -- * Response
    , ListCloudFrontOriginAccessIdentities2014_05_31Response
    -- ** Response constructor
    , listCloudFrontOriginAccessIdentities2014_05_31Response
    -- ** Response lenses
    , lcfoairCloudFrontOriginAccessIdentityList
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CloudFront.Types
import qualified GHC.Exts

data ListCloudFrontOriginAccessIdentities2014_05_31 = ListCloudFrontOriginAccessIdentities2014_05_31
    { _lcfoaiMarker   :: Maybe Text
    , _lcfoaiMaxItems :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListCloudFrontOriginAccessIdentities2014_05_31' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lcfoaiMarker' @::@ 'Maybe' 'Text'
--
-- * 'lcfoaiMaxItems' @::@ 'Maybe' 'Text'
--
listCloudFrontOriginAccessIdentities2014_05_31 :: ListCloudFrontOriginAccessIdentities2014_05_31
listCloudFrontOriginAccessIdentities2014_05_31 = ListCloudFrontOriginAccessIdentities2014_05_31
    { _lcfoaiMarker   = Nothing
    , _lcfoaiMaxItems = Nothing
    }

-- | Use this when paginating results to indicate where to begin in your list
-- of origin access identities. The results include identities in the list
-- that occur after the marker. To get the next page of results, set the
-- Marker to the value of the NextMarker from the current page's response
-- (which is also the ID of the last identity on that page).
lcfoaiMarker :: Lens' ListCloudFrontOriginAccessIdentities2014_05_31 (Maybe Text)
lcfoaiMarker = lens _lcfoaiMarker (\s a -> s { _lcfoaiMarker = a })

-- | The maximum number of origin access identities you want in the response
-- body.
lcfoaiMaxItems :: Lens' ListCloudFrontOriginAccessIdentities2014_05_31 (Maybe Text)
lcfoaiMaxItems = lens _lcfoaiMaxItems (\s a -> s { _lcfoaiMaxItems = a })

instance ToPath ListCloudFrontOriginAccessIdentities2014_05_31 where
    toPath = const "/2014-05-31/origin-access-identity/cloudfront"

instance ToQuery ListCloudFrontOriginAccessIdentities2014_05_31 where
    toQuery ListCloudFrontOriginAccessIdentities2014_05_31{..} = mconcat
        [ "Marker"   =? _lcfoaiMarker
        , "MaxItems" =? _lcfoaiMaxItems
        ]

instance ToHeaders ListCloudFrontOriginAccessIdentities2014_05_31

newtype ListCloudFrontOriginAccessIdentities2014_05_31Response = ListCloudFrontOriginAccessIdentities2014_05_31Response
    { _lcfoairCloudFrontOriginAccessIdentityList :: Maybe CloudFrontOriginAccessIdentityList
    } deriving (Eq, Show, Generic)

-- | 'ListCloudFrontOriginAccessIdentities2014_05_31Response' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lcfoairCloudFrontOriginAccessIdentityList' @::@ 'Maybe' 'CloudFrontOriginAccessIdentityList'
--
listCloudFrontOriginAccessIdentities2014_05_31Response :: ListCloudFrontOriginAccessIdentities2014_05_31Response
listCloudFrontOriginAccessIdentities2014_05_31Response = ListCloudFrontOriginAccessIdentities2014_05_31Response
    { _lcfoairCloudFrontOriginAccessIdentityList = Nothing
    }

-- | The CloudFrontOriginAccessIdentityList type.
lcfoairCloudFrontOriginAccessIdentityList :: Lens' ListCloudFrontOriginAccessIdentities2014_05_31Response (Maybe CloudFrontOriginAccessIdentityList)
lcfoairCloudFrontOriginAccessIdentityList =
    lens _lcfoairCloudFrontOriginAccessIdentityList
        (\s a -> s { _lcfoairCloudFrontOriginAccessIdentityList = a })

instance AWSRequest ListCloudFrontOriginAccessIdentities2014_05_31 where
    type Sv ListCloudFrontOriginAccessIdentities2014_05_31 = CloudFront
    type Rs ListCloudFrontOriginAccessIdentities2014_05_31 = ListCloudFrontOriginAccessIdentities2014_05_31Response

    request  = get
    response = xmlResponse $ \h x -> ListCloudFrontOriginAccessIdentities2014_05_31Response
        <$> x %| "CloudFrontOriginAccessIdentityList"
