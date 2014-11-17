{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CognitoIdentity.GetId
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Generates (or retrieves) a Cognito ID. Supplying multiple logins will
-- create an implicit linked account.
--
-- <http://docs.aws.amazon.com/cognitoidentity/latest/APIReference/API_GetId.html>
module Network.AWS.CognitoIdentity.GetId
    (
    -- * Request
      GetId
    -- ** Request constructor
    , getId
    -- ** Request lenses
    , giAccountId
    , giIdentityPoolId
    , giLogins

    -- * Response
    , GetIdResponse
    -- ** Response constructor
    , getIdResponse
    -- ** Response lenses
    , girIdentityId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CognitoIdentity.Types
import qualified GHC.Exts

data GetId = GetId
    { _giAccountId      :: Text
    , _giIdentityPoolId :: Text
    , _giLogins         :: Map Text Text
    } deriving (Eq, Show, Generic)

-- | 'GetId' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'giAccountId' @::@ 'Text'
--
-- * 'giIdentityPoolId' @::@ 'Text'
--
-- * 'giLogins' @::@ 'HashMap' 'Text' 'Text'
--
getId :: Text -- ^ 'giAccountId'
      -> Text -- ^ 'giIdentityPoolId'
      -> GetId
getId p1 p2 = GetId
    { _giAccountId      = p1
    , _giIdentityPoolId = p2
    , _giLogins         = mempty
    }

-- | A standard AWS account ID (9+ digits).
giAccountId :: Lens' GetId Text
giAccountId = lens _giAccountId (\s a -> s { _giAccountId = a })

-- | An identity pool ID in the format REGION:GUID.
giIdentityPoolId :: Lens' GetId Text
giIdentityPoolId = lens _giIdentityPoolId (\s a -> s { _giIdentityPoolId = a })

-- | A set of optional name-value pairs that map provider names to provider
-- tokens. The available provider names for Logins are as follows: Facebook:
-- graph.facebook.com Google: accounts.google.com Amazon: www.amazon.com.
giLogins :: Lens' GetId (HashMap Text Text)
giLogins = lens _giLogins (\s a -> s { _giLogins = a })
    . _Map

newtype GetIdResponse = GetIdResponse
    { _girIdentityId :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'GetIdResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'girIdentityId' @::@ 'Maybe' 'Text'
--
getIdResponse :: GetIdResponse
getIdResponse = GetIdResponse
    { _girIdentityId = Nothing
    }

-- | A unique identifier in the format REGION:GUID.
girIdentityId :: Lens' GetIdResponse (Maybe Text)
girIdentityId = lens _girIdentityId (\s a -> s { _girIdentityId = a })

instance ToPath GetId where
    toPath = const "/"

instance ToQuery GetId where
    toQuery = const mempty

instance ToHeaders GetId
instance ToJSON GetId where
    toJSON = genericToJSON jsonOptions

instance AWSRequest GetId where
    type Sv GetId = CognitoIdentity
    type Rs GetId = GetIdResponse

    request  = post
    response = jsonResponse

instance FromJSON GetIdResponse where
    parseJSON = genericParseJSON jsonOptions
