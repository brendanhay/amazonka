{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFormation.ListStackResources
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns descriptions of all resources of the specified stack. For deleted
-- stacks, ListStackResources returns resource information for up to 90 days
-- after the stack has been deleted.
--
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_ListStackResources.html>
module Network.AWS.CloudFormation.ListStackResources
    (
    -- * Request
      ListStackResources
    -- ** Request constructor
    , listStackResources
    -- ** Request lenses
    , lsrNextToken
    , lsrStackName

    -- * Response
    , ListStackResourcesResponse
    -- ** Response constructor
    , listStackResourcesResponse
    -- ** Response lenses
    , lsrrNextToken
    , lsrrStackResourceSummaries
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudFormation.Types
import qualified GHC.Exts

data ListStackResources = ListStackResources
    { _lsrNextToken :: Maybe Text
    , _lsrStackName :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListStackResources' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsrNextToken' @::@ 'Maybe' 'Text'
--
-- * 'lsrStackName' @::@ 'Text'
--
listStackResources :: Text -- ^ 'lsrStackName'
                   -> ListStackResources
listStackResources p1 = ListStackResources
    { _lsrStackName = p1
    , _lsrNextToken = Nothing
    }

-- | String that identifies the start of the next list of stack resource
-- summaries, if there is one. Default: There is no default value.
lsrNextToken :: Lens' ListStackResources (Maybe Text)
lsrNextToken = lens _lsrNextToken (\s a -> s { _lsrNextToken = a })

-- | The name or the unique identifier associated with the stack, which are
-- not always interchangeable: Running stacks: You can specify either the
-- stack's name or its unique stack ID. Deleted stacks: You must specify the
-- unique stack ID. Default: There is no default value.
lsrStackName :: Lens' ListStackResources Text
lsrStackName = lens _lsrStackName (\s a -> s { _lsrStackName = a })

data ListStackResourcesResponse = ListStackResourcesResponse
    { _lsrrNextToken              :: Maybe Text
    , _lsrrStackResourceSummaries :: [StackResourceSummary]
    } deriving (Eq, Show, Generic)

-- | 'ListStackResourcesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsrrNextToken' @::@ 'Maybe' 'Text'
--
-- * 'lsrrStackResourceSummaries' @::@ ['StackResourceSummary']
--
listStackResourcesResponse :: ListStackResourcesResponse
listStackResourcesResponse = ListStackResourcesResponse
    { _lsrrStackResourceSummaries = mempty
    , _lsrrNextToken              = Nothing
    }

-- | String that identifies the start of the next list of stack resources, if
-- there is one.
lsrrNextToken :: Lens' ListStackResourcesResponse (Maybe Text)
lsrrNextToken = lens _lsrrNextToken (\s a -> s { _lsrrNextToken = a })

-- | A list of StackResourceSummary structures.
lsrrStackResourceSummaries :: Lens' ListStackResourcesResponse [StackResourceSummary]
lsrrStackResourceSummaries =
    lens _lsrrStackResourceSummaries
        (\s a -> s { _lsrrStackResourceSummaries = a })

instance ToPath ListStackResources where
    toPath = const "/"

instance ToQuery ListStackResources

instance ToHeaders ListStackResources

instance AWSRequest ListStackResources where
    type Sv ListStackResources = CloudFormation
    type Rs ListStackResources = ListStackResourcesResponse

    request  = post "ListStackResources"
    response = xmlResponse

instance FromXML ListStackResourcesResponse where
    parseXML c = ListStackResourcesResponse
        <$> c .:? "NextToken"
        <*> c .: "StackResourceSummaries"
