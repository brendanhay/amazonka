{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CognitoIdentity.UnlinkIdentity
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Unlinks a federated identity from an existing account. Unlinked logins will
-- be considered new identities next time they are seen. Removing the last
-- linked login will make this identity inaccessible.
module Network.AWS.CognitoIdentity.UnlinkIdentity
    (
    -- * Request
      UnlinkIdentity
    -- ** Request constructor
    , unlinkIdentity
    -- ** Request lenses
    , uiIdentityId
    , uiLogins
    , uiLoginsToRemove

    -- * Response
    , UnlinkIdentityResponse
    -- ** Response constructor
    , unlinkIdentityResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CognitoIdentity.Types
import qualified GHC.Exts

data UnlinkIdentity = UnlinkIdentity
    { _uiIdentityId     :: Text
    , _uiLogins         :: Map Text Text
    , _uiLoginsToRemove :: [Text]
    } deriving (Eq, Show, Generic)

-- | 'UnlinkIdentity' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uiIdentityId' @::@ 'Text'
--
-- * 'uiLogins' @::@ 'HashMap' 'Text' 'Text'
--
-- * 'uiLoginsToRemove' @::@ ['Text']
--
unlinkIdentity :: Text -- ^ 'uiIdentityId'
               -> UnlinkIdentity
unlinkIdentity p1 = UnlinkIdentity
    { _uiIdentityId     = p1
    , _uiLogins         = mempty
    , _uiLoginsToRemove = mempty
    }

-- | A unique identifier in the format REGION:GUID.
uiIdentityId :: Lens' UnlinkIdentity Text
uiIdentityId = lens _uiIdentityId (\s a -> s { _uiIdentityId = a })

-- | A set of optional name-value pairs that map provider names to provider
-- tokens.
uiLogins :: Lens' UnlinkIdentity (HashMap Text Text)
uiLogins = lens _uiLogins (\s a -> s { _uiLogins = a })
    . _Map

-- | Provider names to unlink from this identity.
uiLoginsToRemove :: Lens' UnlinkIdentity [Text]
uiLoginsToRemove = lens _uiLoginsToRemove (\s a -> s { _uiLoginsToRemove = a })

data UnlinkIdentityResponse = UnlinkIdentityResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'UnlinkIdentityResponse' constructor.
unlinkIdentityResponse :: UnlinkIdentityResponse
unlinkIdentityResponse = UnlinkIdentityResponse

instance AWSRequest UnlinkIdentity where
    type Sv UnlinkIdentity = CognitoIdentity
    type Rs UnlinkIdentity = UnlinkIdentityResponse

    request  = post
    response = nullResponse UnlinkIdentityResponse

instance ToPath UnlinkIdentity where
    toPath = const "/"

instance ToHeaders UnlinkIdentity

instance ToQuery UnlinkIdentity where
    toQuery = const mempty

instance ToJSON UnlinkIdentity where
    toJSON = genericToJSON jsonOptions
