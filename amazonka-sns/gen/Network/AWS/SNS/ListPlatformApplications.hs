{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
--
-- <http://docs.aws.amazon.com/sns/latest/api/API_ListPlatformApplications.html>
module Network.AWS.SNS.ListPlatformApplications
    (
    -- * Request
      ListPlatformApplications
    -- ** Request constructor
    , listPlatformApplications
    -- ** Request lenses
    , lpaNextToken

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
import qualified GHC.Exts

newtype ListPlatformApplications = ListPlatformApplications
    { _lpaNextToken :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'ListPlatformApplications' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lpaNextToken' @::@ 'Maybe' 'Text'
--
listPlatformApplications :: ListPlatformApplications
listPlatformApplications = ListPlatformApplications
    { _lpaNextToken = Nothing
    }

-- | NextToken string is used when calling ListPlatformApplications action to
-- retrieve additional records that are available after the first page
-- results.
lpaNextToken :: Lens' ListPlatformApplications (Maybe Text)
lpaNextToken = lens _lpaNextToken (\s a -> s { _lpaNextToken = a })

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

instance ToPath ListPlatformApplications where
    toPath = const "/"

instance ToQuery ListPlatformApplications

instance ToHeaders ListPlatformApplications

instance AWSRequest ListPlatformApplications where
    type Sv ListPlatformApplications = SNS
    type Rs ListPlatformApplications = ListPlatformApplicationsResponse

    request  = post "ListPlatformApplications"
    response = xmlResponse

instance FromXML ListPlatformApplicationsResponse where
    parseXML c = ListPlatformApplicationsResponse
        <$> c .:? "NextToken"
        <*> c .: "PlatformApplications"
