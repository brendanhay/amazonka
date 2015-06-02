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

-- Module      : Network.AWS.CognitoSync.GetCognitoEvents
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

-- | Gets the events and the corresponding Lambda functions associated with an
-- identity pool
--
-- <http://docs.aws.amazon.com/cognitosync/latest/APIReference/API_GetCognitoEvents.html>
module Network.AWS.CognitoSync.GetCognitoEvents
    (
    -- * Request
      GetCognitoEvents
    -- ** Request constructor
    , getCognitoEvents
    -- ** Request lenses
    , gceIdentityPoolId

    -- * Response
    , GetCognitoEventsResponse
    -- ** Response constructor
    , getCognitoEventsResponse
    -- ** Response lenses
    , gcerEvents
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.CognitoSync.Types
import qualified GHC.Exts

newtype GetCognitoEvents = GetCognitoEvents
    { _gceIdentityPoolId :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'GetCognitoEvents' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gceIdentityPoolId' @::@ 'Text'
--
getCognitoEvents :: Text -- ^ 'gceIdentityPoolId'
                 -> GetCognitoEvents
getCognitoEvents p1 = GetCognitoEvents
    { _gceIdentityPoolId = p1
    }

-- | The Cognito Identity Pool ID for the request
gceIdentityPoolId :: Lens' GetCognitoEvents Text
gceIdentityPoolId =
    lens _gceIdentityPoolId (\s a -> s { _gceIdentityPoolId = a })

newtype GetCognitoEventsResponse = GetCognitoEventsResponse
    { _gcerEvents :: Map Text Text
    } deriving (Eq, Read, Show, Monoid, Semigroup)

-- | 'GetCognitoEventsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gcerEvents' @::@ 'HashMap' 'Text' 'Text'
--
getCognitoEventsResponse :: GetCognitoEventsResponse
getCognitoEventsResponse = GetCognitoEventsResponse
    { _gcerEvents = mempty
    }

-- | The Cognito Events returned from the GetCognitoEvents request
gcerEvents :: Lens' GetCognitoEventsResponse (HashMap Text Text)
gcerEvents = lens _gcerEvents (\s a -> s { _gcerEvents = a }) . _Map

instance ToPath GetCognitoEvents where
    toPath GetCognitoEvents{..} = mconcat
        [ "/identitypools/"
        , toText _gceIdentityPoolId
        , "/events"
        ]

instance ToQuery GetCognitoEvents where
    toQuery = const mempty

instance ToHeaders GetCognitoEvents

instance ToJSON GetCognitoEvents where
    toJSON = const (toJSON Empty)

instance AWSRequest GetCognitoEvents where
    type Sv GetCognitoEvents = CognitoSync
    type Rs GetCognitoEvents = GetCognitoEventsResponse

    request  = get
    response = jsonResponse

instance FromJSON GetCognitoEventsResponse where
    parseJSON = withObject "GetCognitoEventsResponse" $ \o -> GetCognitoEventsResponse
        <$> o .:? "Events" .!= mempty
