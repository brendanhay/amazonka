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

-- Module      : Network.AWS.SNS.ListEndpointsByPlatformApplication
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists the endpoints and endpoint attributes for devices in a supported push
-- notification service, such as GCM and APNS. The results for
-- ListEndpointsByPlatformApplication are paginated and return a limited list
-- of endpoints, up to 100. If additional records are available after the
-- first page results, then a NextToken string will be returned. To receive
-- the next page, you call ListEndpointsByPlatformApplication again using the
-- NextToken string received from the previous call. When there are no more
-- records to return, NextToken will be null. For more information, see Using
-- Amazon SNS Mobile Push Notifications.
module Network.AWS.SNS.ListEndpointsByPlatformApplication
    (
    -- * Request
      ListEndpointsByPlatformApplicationInput
    -- ** Request constructor
    , listEndpointsByPlatformApplicationInput
    -- ** Request lenses
    , lebpaiNextToken
    , lebpaiPlatformApplicationArn

    -- * Response
    , ListEndpointsByPlatformApplicationResponse
    -- ** Response constructor
    , listEndpointsByPlatformApplicationResponse
    -- ** Response lenses
    , lebparEndpoints
    , lebparNextToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SNS.Types

data ListEndpointsByPlatformApplicationInput = ListEndpointsByPlatformApplicationInput
    { _lebpaiNextToken              :: Maybe Text
    , _lebpaiPlatformApplicationArn :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListEndpointsByPlatformApplicationInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lebpaiNextToken' @::@ 'Maybe' 'Text'
--
-- * 'lebpaiPlatformApplicationArn' @::@ 'Text'
--
listEndpointsByPlatformApplicationInput :: Text -- ^ 'lebpaiPlatformApplicationArn'
                                        -> ListEndpointsByPlatformApplicationInput
listEndpointsByPlatformApplicationInput p1 = ListEndpointsByPlatformApplicationInput
    { _lebpaiPlatformApplicationArn = p1
    , _lebpaiNextToken              = Nothing
    }

-- | NextToken string is used when calling ListEndpointsByPlatformApplication
-- action to retrieve additional records that are available after the first
-- page results.
lebpaiNextToken :: Lens' ListEndpointsByPlatformApplicationInput (Maybe Text)
lebpaiNextToken = lens _lebpaiNextToken (\s a -> s { _lebpaiNextToken = a })

-- | PlatformApplicationArn for ListEndpointsByPlatformApplicationInput
-- action.
lebpaiPlatformApplicationArn :: Lens' ListEndpointsByPlatformApplicationInput Text
lebpaiPlatformApplicationArn =
    lens _lebpaiPlatformApplicationArn
        (\s a -> s { _lebpaiPlatformApplicationArn = a })
instance ToQuery ListEndpointsByPlatformApplicationInput

instance ToPath ListEndpointsByPlatformApplicationInput where
    toPath = const "/"

data ListEndpointsByPlatformApplicationResponse = ListEndpointsByPlatformApplicationResponse
    { _lebparEndpoints :: [Endpoint]
    , _lebparNextToken :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'ListEndpointsByPlatformApplicationResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lebparEndpoints' @::@ ['Endpoint']
--
-- * 'lebparNextToken' @::@ 'Maybe' 'Text'
--
listEndpointsByPlatformApplicationResponse :: ListEndpointsByPlatformApplicationResponse
listEndpointsByPlatformApplicationResponse = ListEndpointsByPlatformApplicationResponse
    { _lebparEndpoints = mempty
    , _lebparNextToken = Nothing
    }

-- | Endpoints returned for ListEndpointsByPlatformApplication action.
lebparEndpoints :: Lens' ListEndpointsByPlatformApplicationResponse [Endpoint]
lebparEndpoints = lens _lebparEndpoints (\s a -> s { _lebparEndpoints = a })

-- | NextToken string is returned when calling
-- ListEndpointsByPlatformApplication action if additional records are
-- available after the first page results.
lebparNextToken :: Lens' ListEndpointsByPlatformApplicationResponse (Maybe Text)
lebparNextToken = lens _lebparNextToken (\s a -> s { _lebparNextToken = a })
instance FromXML ListEndpointsByPlatformApplicationResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ListEndpointsByPlatformApplicationResponse"

instance AWSRequest ListEndpointsByPlatformApplicationInput where
    type Sv ListEndpointsByPlatformApplicationInput = SNS
    type Rs ListEndpointsByPlatformApplicationInput = ListEndpointsByPlatformApplicationResponse

    request  = post "ListEndpointsByPlatformApplication"
    response = xmlResponse $ \h x -> ListEndpointsByPlatformApplicationResponse
        <$> x %| "Endpoints"
        <*> x %| "NextToken"
