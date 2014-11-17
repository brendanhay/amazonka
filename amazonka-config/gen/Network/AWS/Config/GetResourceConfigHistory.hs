{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Config.GetResourceConfigHistory
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of configuration items for the specified resource. The list
-- contains details about each state of the resource during the specified time
-- interval. You can specify a limit on the number of results returned on the
-- page. If a limit is specified, a nextToken is returned as part of the
-- result that you can use to continue this request.
module Network.AWS.Config.GetResourceConfigHistory
    (
    -- * Request
      GetResourceConfigHistory
    -- ** Request constructor
    , getResourceConfigHistory
    -- ** Request lenses
    , grchChronologicalOrder
    , grchEarlierTime
    , grchLaterTime
    , grchLimit
    , grchNextToken
    , grchResourceId
    , grchResourceType

    -- * Response
    , GetResourceConfigHistoryResponse
    -- ** Response constructor
    , getResourceConfigHistoryResponse
    -- ** Response lenses
    , grchrConfigurationItems
    , grchrNextToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.Config.Types
import qualified GHC.Exts

data GetResourceConfigHistory = GetResourceConfigHistory
    { _grchChronologicalOrder :: Maybe Text
    , _grchEarlierTime        :: Maybe RFC822
    , _grchLaterTime          :: Maybe RFC822
    , _grchLimit              :: Maybe Nat
    , _grchNextToken          :: Maybe Text
    , _grchResourceId         :: Text
    , _grchResourceType       :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'GetResourceConfigHistory' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'grchChronologicalOrder' @::@ 'Maybe' 'Text'
--
-- * 'grchEarlierTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'grchLaterTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'grchLimit' @::@ 'Maybe' 'Natural'
--
-- * 'grchNextToken' @::@ 'Maybe' 'Text'
--
-- * 'grchResourceId' @::@ 'Text'
--
-- * 'grchResourceType' @::@ 'Text'
--
getResourceConfigHistory :: Text -- ^ 'grchResourceType'
                         -> Text -- ^ 'grchResourceId'
                         -> GetResourceConfigHistory
getResourceConfigHistory p1 p2 = GetResourceConfigHistory
    { _grchResourceType       = p1
    , _grchResourceId         = p2
    , _grchLaterTime          = Nothing
    , _grchEarlierTime        = Nothing
    , _grchChronologicalOrder = Nothing
    , _grchLimit              = Nothing
    , _grchNextToken          = Nothing
    }

-- | The chronological order for configuration items listed. By default the
-- results are listed in reverse chronological order.
grchChronologicalOrder :: Lens' GetResourceConfigHistory (Maybe Text)
grchChronologicalOrder =
    lens _grchChronologicalOrder (\s a -> s { _grchChronologicalOrder = a })

-- | The time stamp that indicates an earlier time. If not specified, the
-- action returns paginated results that contain configuration items that
-- start from when the first configuration item was recorded.
grchEarlierTime :: Lens' GetResourceConfigHistory (Maybe UTCTime)
grchEarlierTime = lens _grchEarlierTime (\s a -> s { _grchEarlierTime = a })
    . mapping _Time

-- | The time stamp that indicates a later time. If not specified, current
-- time is taken.
grchLaterTime :: Lens' GetResourceConfigHistory (Maybe UTCTime)
grchLaterTime = lens _grchLaterTime (\s a -> s { _grchLaterTime = a })
    . mapping _Time

-- | The maximum number of configuration items returned in each page. The
-- default is 10. You cannot specify a limit greater than 100.
grchLimit :: Lens' GetResourceConfigHistory (Maybe Natural)
grchLimit = lens _grchLimit (\s a -> s { _grchLimit = a })
    . mapping _Nat

-- | An optional parameter used for pagination of the results.
grchNextToken :: Lens' GetResourceConfigHistory (Maybe Text)
grchNextToken = lens _grchNextToken (\s a -> s { _grchNextToken = a })

-- | The ID of the resource (for example., sg-xxxxxx).
grchResourceId :: Lens' GetResourceConfigHistory Text
grchResourceId = lens _grchResourceId (\s a -> s { _grchResourceId = a })

-- | The resource type.
grchResourceType :: Lens' GetResourceConfigHistory Text
grchResourceType = lens _grchResourceType (\s a -> s { _grchResourceType = a })

data GetResourceConfigHistoryResponse = GetResourceConfigHistoryResponse
    { _grchrConfigurationItems :: [ConfigurationItem]
    , _grchrNextToken          :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'GetResourceConfigHistoryResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'grchrConfigurationItems' @::@ ['ConfigurationItem']
--
-- * 'grchrNextToken' @::@ 'Maybe' 'Text'
--
getResourceConfigHistoryResponse :: GetResourceConfigHistoryResponse
getResourceConfigHistoryResponse = GetResourceConfigHistoryResponse
    { _grchrConfigurationItems = mempty
    , _grchrNextToken          = Nothing
    }

-- | A list that contains the configuration history of one or more resources.
grchrConfigurationItems :: Lens' GetResourceConfigHistoryResponse [ConfigurationItem]
grchrConfigurationItems =
    lens _grchrConfigurationItems (\s a -> s { _grchrConfigurationItems = a })

-- | A token used for pagination of results.
grchrNextToken :: Lens' GetResourceConfigHistoryResponse (Maybe Text)
grchrNextToken = lens _grchrNextToken (\s a -> s { _grchrNextToken = a })

instance AWSRequest GetResourceConfigHistory where
    type Sv GetResourceConfigHistory = Config
    type Rs GetResourceConfigHistory = GetResourceConfigHistoryResponse

    request  = post
    response = jsonResponse

instance FromJSON GetResourceConfigHistoryResponse where
    parseJSON = genericParseJSON jsonOptions

instance ToPath GetResourceConfigHistory where
    toPath = const "/"

instance ToHeaders GetResourceConfigHistory

instance ToQuery GetResourceConfigHistory where
    toQuery = const mempty

instance ToJSON GetResourceConfigHistory where
    toJSON = genericToJSON jsonOptions
