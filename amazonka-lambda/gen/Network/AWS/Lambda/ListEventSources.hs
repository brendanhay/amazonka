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

-- Module      : Network.AWS.Lambda.ListEventSources
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of event source mappings. For each mapping, the API returns
-- configuration information (see AddEventSource). You can optionally specify
-- filters to retrieve specific event source mappings. This operation requires
-- permission for the lambda:ListEventSources action.
--
-- <http://docs.aws.amazon.com/lambda/latest/dg/API_ListEventSources.html>
module Network.AWS.Lambda.ListEventSources
    (
    -- * Request
      ListEventSources
    -- ** Request constructor
    , listEventSources
    -- ** Request lenses
    , lesEventSourceArn
    , lesFunctionName
    , lesMarker
    , lesMaxItems

    -- * Response
    , ListEventSourcesResponse
    -- ** Response constructor
    , listEventSourcesResponse
    -- ** Response lenses
    , lesrEventSources
    , lesrNextMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.Lambda.Types
import qualified GHC.Exts

data ListEventSources = ListEventSources
    { _lesEventSourceArn :: Maybe Text
    , _lesFunctionName   :: Maybe Text
    , _lesMarker         :: Maybe Text
    , _lesMaxItems       :: Maybe Nat
    } deriving (Eq, Ord, Show)

-- | 'ListEventSources' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lesEventSourceArn' @::@ 'Maybe' 'Text'
--
-- * 'lesFunctionName' @::@ 'Maybe' 'Text'
--
-- * 'lesMarker' @::@ 'Maybe' 'Text'
--
-- * 'lesMaxItems' @::@ 'Maybe' 'Natural'
--
listEventSources :: ListEventSources
listEventSources = ListEventSources
    { _lesEventSourceArn = Nothing
    , _lesFunctionName   = Nothing
    , _lesMarker         = Nothing
    , _lesMaxItems       = Nothing
    }

-- | The Amazon Resource Name (ARN) of the Amazon Kinesis stream.
lesEventSourceArn :: Lens' ListEventSources (Maybe Text)
lesEventSourceArn =
    lens _lesEventSourceArn (\s a -> s { _lesEventSourceArn = a })

-- | The name of the AWS Lambda function.
lesFunctionName :: Lens' ListEventSources (Maybe Text)
lesFunctionName = lens _lesFunctionName (\s a -> s { _lesFunctionName = a })

-- | Optional string. An opaque pagination token returned from a previous
-- ListEventSources operation. If present, specifies to continue the list
-- from where the returning call left off.
lesMarker :: Lens' ListEventSources (Maybe Text)
lesMarker = lens _lesMarker (\s a -> s { _lesMarker = a })

-- | Optional integer. Specifies the maximum number of event sources to return
-- in response. This value must be greater than 0.
lesMaxItems :: Lens' ListEventSources (Maybe Natural)
lesMaxItems = lens _lesMaxItems (\s a -> s { _lesMaxItems = a }) . mapping _Nat

data ListEventSourcesResponse = ListEventSourcesResponse
    { _lesrEventSources :: List "EventSources" EventSourceConfiguration
    , _lesrNextMarker   :: Maybe Text
    } deriving (Eq, Show)

-- | 'ListEventSourcesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lesrEventSources' @::@ ['EventSourceConfiguration']
--
-- * 'lesrNextMarker' @::@ 'Maybe' 'Text'
--
listEventSourcesResponse :: ListEventSourcesResponse
listEventSourcesResponse = ListEventSourcesResponse
    { _lesrNextMarker   = Nothing
    , _lesrEventSources = mempty
    }

-- | An arrary of EventSourceConfiguration objects.
lesrEventSources :: Lens' ListEventSourcesResponse [EventSourceConfiguration]
lesrEventSources = lens _lesrEventSources (\s a -> s { _lesrEventSources = a }) . _List

-- | A string, present if there are more event source mappings.
lesrNextMarker :: Lens' ListEventSourcesResponse (Maybe Text)
lesrNextMarker = lens _lesrNextMarker (\s a -> s { _lesrNextMarker = a })

instance ToPath ListEventSources where
    toPath = const "/2014-11-13/event-source-mappings/"

instance ToQuery ListEventSources where
    toQuery ListEventSources{..} = mconcat
        [ "EventSource"  =? _lesEventSourceArn
        , "FunctionName" =? _lesFunctionName
        , "Marker"       =? _lesMarker
        , "MaxItems"     =? _lesMaxItems
        ]

instance ToHeaders ListEventSources

instance ToJSON ListEventSources where
    toJSON = const (toJSON Empty)

instance AWSRequest ListEventSources where
    type Sv ListEventSources = Lambda
    type Rs ListEventSources = ListEventSourcesResponse

    request  = get
    response = jsonResponse

instance FromJSON ListEventSourcesResponse where
    parseJSON = withObject "ListEventSourcesResponse" $ \o -> ListEventSourcesResponse
        <$> o .:  "EventSources"
        <*> o .:? "NextMarker"


Some kind of operator / class to check the types whether to continue?
