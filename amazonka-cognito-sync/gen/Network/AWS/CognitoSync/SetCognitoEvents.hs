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

-- Module      : Network.AWS.CognitoSync.SetCognitoEvents
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

-- | Sets the AWS Lambda function for a given event type for an identity pool.
-- This request only updates the key/value pair specified. Other key/values
-- pairs are not updated. To remove a key value pair, pass a empty value for the
-- particular key.
--
-- <http://docs.aws.amazon.com/cognitosync/latest/APIReference/API_SetCognitoEvents.html>
module Network.AWS.CognitoSync.SetCognitoEvents
    (
    -- * Request
      SetCognitoEvents
    -- ** Request constructor
    , setCognitoEvents
    -- ** Request lenses
    , sceEvents
    , sceIdentityPoolId

    -- * Response
    , SetCognitoEventsResponse
    -- ** Response constructor
    , setCognitoEventsResponse
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.CognitoSync.Types
import qualified GHC.Exts

data SetCognitoEvents = SetCognitoEvents
    { _sceEvents         :: Map Text Text
    , _sceIdentityPoolId :: Text
    } deriving (Eq, Read, Show)

-- | 'SetCognitoEvents' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sceEvents' @::@ 'HashMap' 'Text' 'Text'
--
-- * 'sceIdentityPoolId' @::@ 'Text'
--
setCognitoEvents :: Text -- ^ 'sceIdentityPoolId'
                 -> SetCognitoEvents
setCognitoEvents p1 = SetCognitoEvents
    { _sceIdentityPoolId = p1
    , _sceEvents         = mempty
    }

-- | The events to configure
sceEvents :: Lens' SetCognitoEvents (HashMap Text Text)
sceEvents = lens _sceEvents (\s a -> s { _sceEvents = a }) . _Map

-- | The Cognito Identity Pool to use when configuring Cognito Events
sceIdentityPoolId :: Lens' SetCognitoEvents Text
sceIdentityPoolId =
    lens _sceIdentityPoolId (\s a -> s { _sceIdentityPoolId = a })

data SetCognitoEventsResponse = SetCognitoEventsResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'SetCognitoEventsResponse' constructor.
setCognitoEventsResponse :: SetCognitoEventsResponse
setCognitoEventsResponse = SetCognitoEventsResponse

instance ToPath SetCognitoEvents where
    toPath SetCognitoEvents{..} = mconcat
        [ "/identitypools/"
        , toText _sceIdentityPoolId
        , "/events"
        ]

instance ToQuery SetCognitoEvents where
    toQuery = const mempty

instance ToHeaders SetCognitoEvents

instance ToJSON SetCognitoEvents where
    toJSON SetCognitoEvents{..} = object
        [ "Events" .= _sceEvents
        ]

instance AWSRequest SetCognitoEvents where
    type Sv SetCognitoEvents = CognitoSync
    type Rs SetCognitoEvents = SetCognitoEventsResponse

    request  = post
    response = nullResponse SetCognitoEventsResponse
