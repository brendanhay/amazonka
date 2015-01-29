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

-- Module      : Network.AWS.CognitoIdentity.UnlinkDeveloperIdentity
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

-- | Unlinks a 'DeveloperUserIdentifier' from an existing identity. Unlinked
-- developer users will be considered new identities next time they are seen.
-- If, for a given Cognito identity, you remove all federated identities as well
-- as the developer user identifier, the Cognito identity becomes inaccessible.
--
-- <http://docs.aws.amazon.com/cognitoidentity/latest/APIReference/API_UnlinkDeveloperIdentity.html>
module Network.AWS.CognitoIdentity.UnlinkDeveloperIdentity
    (
    -- * Request
      UnlinkDeveloperIdentity
    -- ** Request constructor
    , unlinkDeveloperIdentity
    -- ** Request lenses
    , udiDeveloperProviderName
    , udiDeveloperUserIdentifier
    , udiIdentityId
    , udiIdentityPoolId

    -- * Response
    , UnlinkDeveloperIdentityResponse
    -- ** Response constructor
    , unlinkDeveloperIdentityResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CognitoIdentity.Types
import qualified GHC.Exts

data UnlinkDeveloperIdentity = UnlinkDeveloperIdentity
    { _udiDeveloperProviderName   :: Text
    , _udiDeveloperUserIdentifier :: Text
    , _udiIdentityId              :: Text
    , _udiIdentityPoolId          :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'UnlinkDeveloperIdentity' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'udiDeveloperProviderName' @::@ 'Text'
--
-- * 'udiDeveloperUserIdentifier' @::@ 'Text'
--
-- * 'udiIdentityId' @::@ 'Text'
--
-- * 'udiIdentityPoolId' @::@ 'Text'
--
unlinkDeveloperIdentity :: Text -- ^ 'udiIdentityId'
                        -> Text -- ^ 'udiIdentityPoolId'
                        -> Text -- ^ 'udiDeveloperProviderName'
                        -> Text -- ^ 'udiDeveloperUserIdentifier'
                        -> UnlinkDeveloperIdentity
unlinkDeveloperIdentity p1 p2 p3 p4 = UnlinkDeveloperIdentity
    { _udiIdentityId              = p1
    , _udiIdentityPoolId          = p2
    , _udiDeveloperProviderName   = p3
    , _udiDeveloperUserIdentifier = p4
    }

-- | The "domain" by which Cognito will refer to your users.
udiDeveloperProviderName :: Lens' UnlinkDeveloperIdentity Text
udiDeveloperProviderName =
    lens _udiDeveloperProviderName
        (\s a -> s { _udiDeveloperProviderName = a })

-- | A unique ID used by your backend authentication process to identify a user.
udiDeveloperUserIdentifier :: Lens' UnlinkDeveloperIdentity Text
udiDeveloperUserIdentifier =
    lens _udiDeveloperUserIdentifier
        (\s a -> s { _udiDeveloperUserIdentifier = a })

-- | A unique identifier in the format REGION:GUID.
udiIdentityId :: Lens' UnlinkDeveloperIdentity Text
udiIdentityId = lens _udiIdentityId (\s a -> s { _udiIdentityId = a })

-- | An identity pool ID in the format REGION:GUID.
udiIdentityPoolId :: Lens' UnlinkDeveloperIdentity Text
udiIdentityPoolId =
    lens _udiIdentityPoolId (\s a -> s { _udiIdentityPoolId = a })

data UnlinkDeveloperIdentityResponse = UnlinkDeveloperIdentityResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'UnlinkDeveloperIdentityResponse' constructor.
unlinkDeveloperIdentityResponse :: UnlinkDeveloperIdentityResponse
unlinkDeveloperIdentityResponse = UnlinkDeveloperIdentityResponse

instance ToPath UnlinkDeveloperIdentity where
    toPath = const "/"

instance ToQuery UnlinkDeveloperIdentity where
    toQuery = const mempty

instance ToHeaders UnlinkDeveloperIdentity

instance ToJSON UnlinkDeveloperIdentity where
    toJSON UnlinkDeveloperIdentity{..} = object
        [ "IdentityId"              .= _udiIdentityId
        , "IdentityPoolId"          .= _udiIdentityPoolId
        , "DeveloperProviderName"   .= _udiDeveloperProviderName
        , "DeveloperUserIdentifier" .= _udiDeveloperUserIdentifier
        ]

instance AWSRequest UnlinkDeveloperIdentity where
    type Sv UnlinkDeveloperIdentity = CognitoIdentity
    type Rs UnlinkDeveloperIdentity = UnlinkDeveloperIdentityResponse

    request  = post "UnlinkDeveloperIdentity"
    response = nullResponse UnlinkDeveloperIdentityResponse
