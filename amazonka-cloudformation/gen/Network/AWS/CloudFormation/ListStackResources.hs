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
module Network.AWS.CloudFormation.ListStackResources
    (
    -- * Request
      ListStackResourcesInput
    -- ** Request constructor
    , listStackResourcesInput
    -- ** Request lenses
    , lsriNextToken
    , lsriStackName

    -- * Response
    , ListStackResourcesOutput
    -- ** Response constructor
    , listStackResourcesOutput
    -- ** Response lenses
    , lsroNextToken
    , lsroStackResourceSummaries
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudFormation.Types

data ListStackResourcesInput = ListStackResourcesInput
    { _lsriNextToken :: Maybe Text
    , _lsriStackName :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListStackResourcesInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsriNextToken' @::@ 'Maybe' 'Text'
--
-- * 'lsriStackName' @::@ 'Text'
--
listStackResourcesInput :: Text -- ^ 'lsriStackName'
                        -> ListStackResourcesInput
listStackResourcesInput p1 = ListStackResourcesInput
    { _lsriStackName = p1
    , _lsriNextToken = Nothing
    }

-- | String that identifies the start of the next list of stack resource
-- summaries, if there is one. Default: There is no default value.
lsriNextToken :: Lens' ListStackResourcesInput (Maybe Text)
lsriNextToken = lens _lsriNextToken (\s a -> s { _lsriNextToken = a })

-- | The name or the unique identifier associated with the stack, which are
-- not always interchangeable: Running stacks: You can specify either the
-- stack's name or its unique stack ID. Deleted stacks: You must specify the
-- unique stack ID. Default: There is no default value.
lsriStackName :: Lens' ListStackResourcesInput Text
lsriStackName = lens _lsriStackName (\s a -> s { _lsriStackName = a })

instance ToQuery ListStackResourcesInput

instance ToPath ListStackResourcesInput where
    toPath = const "/"

data ListStackResourcesOutput = ListStackResourcesOutput
    { _lsroNextToken              :: Maybe Text
    , _lsroStackResourceSummaries :: [StackResourceSummary]
    } deriving (Eq, Show, Generic)

-- | 'ListStackResourcesOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsroNextToken' @::@ 'Maybe' 'Text'
--
-- * 'lsroStackResourceSummaries' @::@ ['StackResourceSummary']
--
listStackResourcesOutput :: ListStackResourcesOutput
listStackResourcesOutput = ListStackResourcesOutput
    { _lsroStackResourceSummaries = mempty
    , _lsroNextToken              = Nothing
    }

-- | String that identifies the start of the next list of stack resources, if
-- there is one.
lsroNextToken :: Lens' ListStackResourcesOutput (Maybe Text)
lsroNextToken = lens _lsroNextToken (\s a -> s { _lsroNextToken = a })

-- | A list of StackResourceSummary structures.
lsroStackResourceSummaries :: Lens' ListStackResourcesOutput [StackResourceSummary]
lsroStackResourceSummaries =
    lens _lsroStackResourceSummaries
        (\s a -> s { _lsroStackResourceSummaries = a })

instance FromXML ListStackResourcesOutput where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ListStackResourcesOutput"

instance AWSRequest ListStackResourcesInput where
    type Sv ListStackResourcesInput = CloudFormation
    type Rs ListStackResourcesInput = ListStackResourcesOutput

    request  = post "ListStackResources"
    response = xmlResponse $ \h x -> ListStackResourcesOutput
        <$> x %| "NextToken"
        <*> x %| "StackResourceSummaries"
