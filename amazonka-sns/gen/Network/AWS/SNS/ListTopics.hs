{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.ListTopics
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns a list of the requester's topics. Each call returns a limited list of
-- topics, up to 100. If there are more topics, a 'NextToken' is also returned.
-- Use the 'NextToken' parameter in a new 'ListTopics' call to get further results.
--
-- <http://docs.aws.amazon.com/sns/latest/api/API_ListTopics.html>
module Network.AWS.SNS.ListTopics
    (
    -- * Request
      ListTopics
    -- ** Request constructor
    , listTopics
    -- ** Request lenses
    , ltNextToken

    -- * Response
    , ListTopicsResponse
    -- ** Response constructor
    , listTopicsResponse
    -- ** Response lenses
    , ltrNextToken
    , ltrTopics
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SNS.Types
import qualified GHC.Exts

newtype ListTopics = ListTopics
    { _ltNextToken :: Maybe Text
    } deriving (Eq, Ord, Show, Monoid)

-- | 'ListTopics' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ltNextToken' @::@ 'Maybe' 'Text'
--
listTopics :: ListTopics
listTopics = ListTopics
    { _ltNextToken = Nothing
    }

-- | Token returned by the previous 'ListTopics' request.
ltNextToken :: Lens' ListTopics (Maybe Text)
ltNextToken = lens _ltNextToken (\s a -> s { _ltNextToken = a })

data ListTopicsResponse = ListTopicsResponse
    { _ltrNextToken :: Maybe Text
    , _ltrTopics    :: List "member" Topic
    } deriving (Eq, Show)

-- | 'ListTopicsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ltrNextToken' @::@ 'Maybe' 'Text'
--
-- * 'ltrTopics' @::@ ['Topic']
--
listTopicsResponse :: ListTopicsResponse
listTopicsResponse = ListTopicsResponse
    { _ltrTopics    = mempty
    , _ltrNextToken = Nothing
    }

-- | Token to pass along to the next 'ListTopics' request. This element is returned
-- if there are additional topics to retrieve.
ltrNextToken :: Lens' ListTopicsResponse (Maybe Text)
ltrNextToken = lens _ltrNextToken (\s a -> s { _ltrNextToken = a })

-- | A list of topic ARNs.
ltrTopics :: Lens' ListTopicsResponse [Topic]
ltrTopics = lens _ltrTopics (\s a -> s { _ltrTopics = a }) . _List

instance ToPath ListTopics where
    toPath = const "/"

instance ToQuery ListTopics where
    toQuery ListTopics{..} = mconcat
        [ "NextToken" =? _ltNextToken
        ]

instance ToHeaders ListTopics

instance AWSRequest ListTopics where
    type Sv ListTopics = SNS
    type Rs ListTopics = ListTopicsResponse

    request  = post "ListTopics"
    response = xmlResponse

instance FromXML ListTopicsResponse where
    parseXML = withElement "ListTopicsResult" $ \x -> ListTopicsResponse
        <$> x .@? "NextToken"
        <*> x .@  "Topics"

instance AWSPager ListTopics where
    page rq rs
        | stop (rq ^. ltNextToken) = Nothing
        | otherwise = (\x -> rq & ltNextToken ?~ x)
            <$> (rs ^. ltrNextToken)
