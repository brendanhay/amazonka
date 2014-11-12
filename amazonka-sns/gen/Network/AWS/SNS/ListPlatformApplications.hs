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

-- Module      : Network.AWS.SNS.ListPlatformApplications
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists the platform application objects for the supported push notification
-- services, such as APNS and GCM. The results for ListPlatformApplications
-- are paginated and return a limited list of applications, up to 100. If
-- additional records are available after the first page results, then a
-- NextToken string will be returned. To receive the next page, you call
-- ListPlatformApplications using the NextToken string received from the
-- previous call. When there are no more records to return, NextToken will be
-- null. For more information, see Using Amazon SNS Mobile Push Notifications.
module Network.AWS.SNS.ListPlatformApplications
    (
    -- * Request
      ListPlatformApplicationsInput
    -- ** Request constructor
    , listPlatformApplicationsInput
    -- ** Request lenses
    , lpaiNextToken

    -- * Response
    , ListPlatformApplicationsResponse
    -- ** Response constructor
    , listPlatformApplicationsResponse
    -- ** Response lenses
    , lparNextToken
    , lparPlatformApplications
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SNS.Types

newtype ListPlatformApplicationsInput = ListPlatformApplicationsInput
    { _lpaiNextToken :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'ListPlatformApplicationsInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lpaiNextToken' @::@ 'Maybe' 'Text'
--
listPlatformApplicationsInput :: ListPlatformApplicationsInput
listPlatformApplicationsInput = ListPlatformApplicationsInput
    { _lpaiNextToken = Nothing
    }

-- | NextToken string is used when calling ListPlatformApplications action to
-- retrieve additional records that are available after the first page
-- results.
lpaiNextToken :: Lens' ListPlatformApplicationsInput (Maybe Text)
lpaiNextToken = lens _lpaiNextToken (\s a -> s { _lpaiNextToken = a })

instance ToQuery ListPlatformApplicationsInput

instance ToPath ListPlatformApplicationsInput where
    toPath = const "/"

data ListPlatformApplicationsResponse = ListPlatformApplicationsResponse
    { _lparNextToken            :: Maybe Text
    , _lparPlatformApplications :: [PlatformApplication]
    } deriving (Eq, Show, Generic)

-- | 'ListPlatformApplicationsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lparNextToken' @::@ 'Maybe' 'Text'
--
-- * 'lparPlatformApplications' @::@ ['PlatformApplication']
--
listPlatformApplicationsResponse :: ListPlatformApplicationsResponse
listPlatformApplicationsResponse = ListPlatformApplicationsResponse
    { _lparPlatformApplications = mempty
    , _lparNextToken            = Nothing
    }

-- | NextToken string is returned when calling ListPlatformApplications action
-- if additional records are available after the first page results.
lparNextToken :: Lens' ListPlatformApplicationsResponse (Maybe Text)
lparNextToken = lens _lparNextToken (\s a -> s { _lparNextToken = a })

-- | Platform applications returned when calling ListPlatformApplications
-- action.
lparPlatformApplications :: Lens' ListPlatformApplicationsResponse [PlatformApplication]
lparPlatformApplications =
    lens _lparPlatformApplications
        (\s a -> s { _lparPlatformApplications = a })

instance FromXML ListPlatformApplicationsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ListPlatformApplicationsResponse"

instance AWSRequest ListPlatformApplicationsInput where
    type Sv ListPlatformApplicationsInput = SNS
    type Rs ListPlatformApplicationsInput = ListPlatformApplicationsResponse

    request  = post "ListPlatformApplications"
    response = xmlResponse $ \h x -> ListPlatformApplicationsResponse
        <$> x %| "NextToken"
        <*> x %| "PlatformApplications"
