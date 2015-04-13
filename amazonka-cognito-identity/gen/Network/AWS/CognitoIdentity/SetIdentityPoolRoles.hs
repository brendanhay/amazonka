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

-- Module      : Network.AWS.CognitoIdentity.SetIdentityPoolRoles
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

-- | Sets the roles for an identity pool. These roles are used when making calls
-- to 'GetCredentialsForIdentity' action.
--
-- <http://docs.aws.amazon.com/cognitoidentity/latest/APIReference/API_SetIdentityPoolRoles.html>
module Network.AWS.CognitoIdentity.SetIdentityPoolRoles
    (
    -- * Request
      SetIdentityPoolRoles
    -- ** Request constructor
    , setIdentityPoolRoles
    -- ** Request lenses
    , siprIdentityPoolId
    , siprRoles

    -- * Response
    , SetIdentityPoolRolesResponse
    -- ** Response constructor
    , setIdentityPoolRolesResponse
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CognitoIdentity.Types
import qualified GHC.Exts

data SetIdentityPoolRoles = SetIdentityPoolRoles
    { _siprIdentityPoolId :: Text
    , _siprRoles          :: Map Text Text
    } deriving (Eq, Read, Show)

-- | 'SetIdentityPoolRoles' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'siprIdentityPoolId' @::@ 'Text'
--
-- * 'siprRoles' @::@ 'HashMap' 'Text' 'Text'
--
setIdentityPoolRoles :: Text -- ^ 'siprIdentityPoolId'
                     -> SetIdentityPoolRoles
setIdentityPoolRoles p1 = SetIdentityPoolRoles
    { _siprIdentityPoolId = p1
    , _siprRoles          = mempty
    }

-- | An identity pool ID in the format REGION:GUID.
siprIdentityPoolId :: Lens' SetIdentityPoolRoles Text
siprIdentityPoolId =
    lens _siprIdentityPoolId (\s a -> s { _siprIdentityPoolId = a })

-- | The map of roles associated with this pool. Currently only authenticated and
-- unauthenticated roles are supported.
siprRoles :: Lens' SetIdentityPoolRoles (HashMap Text Text)
siprRoles = lens _siprRoles (\s a -> s { _siprRoles = a }) . _Map

data SetIdentityPoolRolesResponse = SetIdentityPoolRolesResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'SetIdentityPoolRolesResponse' constructor.
setIdentityPoolRolesResponse :: SetIdentityPoolRolesResponse
setIdentityPoolRolesResponse = SetIdentityPoolRolesResponse

instance ToPath SetIdentityPoolRoles where
    toPath = const "/"

instance ToQuery SetIdentityPoolRoles where
    toQuery = const mempty

instance ToHeaders SetIdentityPoolRoles

instance ToJSON SetIdentityPoolRoles where
    toJSON SetIdentityPoolRoles{..} = object
        [ "IdentityPoolId" .= _siprIdentityPoolId
        , "Roles"          .= _siprRoles
        ]

instance AWSRequest SetIdentityPoolRoles where
    type Sv SetIdentityPoolRoles = CognitoIdentity
    type Rs SetIdentityPoolRoles = SetIdentityPoolRolesResponse

    request  = post "SetIdentityPoolRoles"
    response = nullResponse SetIdentityPoolRolesResponse
