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

-- Module      : Network.AWS.ECS.ListTaskDefinitions
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

-- | Returns a list of task definitions that are registered to your account. You
-- can filter the results by family name with the 'familyPrefix' parameter.
--
-- <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_ListTaskDefinitions.html>
module Network.AWS.ECS.ListTaskDefinitions
    (
    -- * Request
      ListTaskDefinitions
    -- ** Request constructor
    , listTaskDefinitions
    -- ** Request lenses
    , ltdFamilyPrefix
    , ltdMaxResults
    , ltdNextToken

    -- * Response
    , ListTaskDefinitionsResponse
    -- ** Response constructor
    , listTaskDefinitionsResponse
    -- ** Response lenses
    , ltdrNextToken
    , ltdrTaskDefinitionArns
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.ECS.Types
import qualified GHC.Exts

data ListTaskDefinitions = ListTaskDefinitions
    { _ltdFamilyPrefix :: Maybe Text
    , _ltdMaxResults   :: Maybe Int
    , _ltdNextToken    :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'ListTaskDefinitions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ltdFamilyPrefix' @::@ 'Maybe' 'Text'
--
-- * 'ltdMaxResults' @::@ 'Maybe' 'Int'
--
-- * 'ltdNextToken' @::@ 'Maybe' 'Text'
--
listTaskDefinitions :: ListTaskDefinitions
listTaskDefinitions = ListTaskDefinitions
    { _ltdFamilyPrefix = Nothing
    , _ltdNextToken    = Nothing
    , _ltdMaxResults   = Nothing
    }

-- | The full family name that you want to filter the 'ListTaskDefinitions' results
-- with. Specifying a 'familyPrefix' will limit the listed task definitions to
-- task definition revisions that belong to that family.
ltdFamilyPrefix :: Lens' ListTaskDefinitions (Maybe Text)
ltdFamilyPrefix = lens _ltdFamilyPrefix (\s a -> s { _ltdFamilyPrefix = a })

-- | The maximum number of task definition results returned by 'ListTaskDefinitions'
-- in paginated output. When this parameter is used, 'ListTaskDefinitions' only
-- returns 'maxResults' results in a single page along with a 'nextToken' response
-- element. The remaining results of the initial request can be seen by sending
-- another 'ListTaskDefinitions' request with the returned 'nextToken' value. This
-- value can be between 1 and 100. If this parameter is not used, then 'ListTaskDefinitions' returns up to 100 results and a 'nextToken' value if applicable.
ltdMaxResults :: Lens' ListTaskDefinitions (Maybe Int)
ltdMaxResults = lens _ltdMaxResults (\s a -> s { _ltdMaxResults = a })

-- | The 'nextToken' value returned from a previous paginated 'ListTaskDefinitions'
-- request where 'maxResults' was used and the results exceeded the value of that
-- parameter. Pagination continues from the end of the previous results that
-- returned the 'nextToken' value. This value is 'null' when there are no more
-- results to return.
ltdNextToken :: Lens' ListTaskDefinitions (Maybe Text)
ltdNextToken = lens _ltdNextToken (\s a -> s { _ltdNextToken = a })

data ListTaskDefinitionsResponse = ListTaskDefinitionsResponse
    { _ltdrNextToken          :: Maybe Text
    , _ltdrTaskDefinitionArns :: List "taskDefinitionArns" Text
    } deriving (Eq, Ord, Read, Show)

-- | 'ListTaskDefinitionsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ltdrNextToken' @::@ 'Maybe' 'Text'
--
-- * 'ltdrTaskDefinitionArns' @::@ ['Text']
--
listTaskDefinitionsResponse :: ListTaskDefinitionsResponse
listTaskDefinitionsResponse = ListTaskDefinitionsResponse
    { _ltdrTaskDefinitionArns = mempty
    , _ltdrNextToken          = Nothing
    }

-- | The 'nextToken' value to include in a future 'ListTaskDefinitions' request. When
-- the results of a 'ListTaskDefinitions' request exceed 'maxResults', this value
-- can be used to retrieve the next page of results. This value is 'null' when
-- there are no more results to return.
ltdrNextToken :: Lens' ListTaskDefinitionsResponse (Maybe Text)
ltdrNextToken = lens _ltdrNextToken (\s a -> s { _ltdrNextToken = a })

-- | The list of task definition Amazon Resource Name (ARN) entries for the 'ListTaskDefintions' request.
ltdrTaskDefinitionArns :: Lens' ListTaskDefinitionsResponse [Text]
ltdrTaskDefinitionArns =
    lens _ltdrTaskDefinitionArns (\s a -> s { _ltdrTaskDefinitionArns = a })
        . _List

instance ToPath ListTaskDefinitions where
    toPath = const "/"

instance ToQuery ListTaskDefinitions where
    toQuery = const mempty

instance ToHeaders ListTaskDefinitions

instance ToJSON ListTaskDefinitions where
    toJSON ListTaskDefinitions{..} = object
        [ "familyPrefix" .= _ltdFamilyPrefix
        , "nextToken"    .= _ltdNextToken
        , "maxResults"   .= _ltdMaxResults
        ]

instance AWSRequest ListTaskDefinitions where
    type Sv ListTaskDefinitions = ECS
    type Rs ListTaskDefinitions = ListTaskDefinitionsResponse

    request  = post "ListTaskDefinitions"
    response = jsonResponse

instance FromJSON ListTaskDefinitionsResponse where
    parseJSON = withObject "ListTaskDefinitionsResponse" $ \o -> ListTaskDefinitionsResponse
        <$> o .:? "nextToken"
        <*> o .:? "taskDefinitionArns" .!= mempty
