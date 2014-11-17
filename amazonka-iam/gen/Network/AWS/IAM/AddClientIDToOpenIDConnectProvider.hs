{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.AddClientIDToOpenIDConnectProvider
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds a new client ID (also known as audience) to the list of client IDs
-- already registered for the specified IAM OpenID Connect provider. This
-- action is idempotent; it does not fail or return an error if you add an
-- existing client ID to the provider.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_AddClientIDToOpenIDConnectProvider.html>
module Network.AWS.IAM.AddClientIDToOpenIDConnectProvider
    (
    -- * Request
      AddClientIDToOpenIDConnectProvider
    -- ** Request constructor
    , addClientIDToOpenIDConnectProvider
    -- ** Request lenses
    , acidtoidcpClientID
    , acidtoidcpOpenIDConnectProviderArn

    -- * Response
    , AddClientIDToOpenIDConnectProviderResponse
    -- ** Response constructor
    , addClientIDToOpenIDConnectProviderResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data AddClientIDToOpenIDConnectProvider = AddClientIDToOpenIDConnectProvider
    { _acidtoidcpClientID                 :: Text
    , _acidtoidcpOpenIDConnectProviderArn :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'AddClientIDToOpenIDConnectProvider' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'acidtoidcpClientID' @::@ 'Text'
--
-- * 'acidtoidcpOpenIDConnectProviderArn' @::@ 'Text'
--
addClientIDToOpenIDConnectProvider :: Text -- ^ 'acidtoidcpOpenIDConnectProviderArn'
                                   -> Text -- ^ 'acidtoidcpClientID'
                                   -> AddClientIDToOpenIDConnectProvider
addClientIDToOpenIDConnectProvider p1 p2 = AddClientIDToOpenIDConnectProvider
    { _acidtoidcpOpenIDConnectProviderArn = p1
    , _acidtoidcpClientID                 = p2
    }

-- | The client ID (also known as audience) to add to the IAM OpenID Connect
-- provider.
acidtoidcpClientID :: Lens' AddClientIDToOpenIDConnectProvider Text
acidtoidcpClientID =
    lens _acidtoidcpClientID (\s a -> s { _acidtoidcpClientID = a })

-- | The Amazon Resource Name (ARN) of the IAM OpenID Connect (OIDC) provider
-- to add the client ID to. You can get a list of OIDC provider ARNs by
-- using the ListOpenIDConnectProviders action.
acidtoidcpOpenIDConnectProviderArn :: Lens' AddClientIDToOpenIDConnectProvider Text
acidtoidcpOpenIDConnectProviderArn =
    lens _acidtoidcpOpenIDConnectProviderArn
        (\s a -> s { _acidtoidcpOpenIDConnectProviderArn = a })

data AddClientIDToOpenIDConnectProviderResponse = AddClientIDToOpenIDConnectProviderResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'AddClientIDToOpenIDConnectProviderResponse' constructor.
addClientIDToOpenIDConnectProviderResponse :: AddClientIDToOpenIDConnectProviderResponse
addClientIDToOpenIDConnectProviderResponse = AddClientIDToOpenIDConnectProviderResponse

instance AWSRequest AddClientIDToOpenIDConnectProvider where
    type Sv AddClientIDToOpenIDConnectProvider = IAM
    type Rs AddClientIDToOpenIDConnectProvider = AddClientIDToOpenIDConnectProviderResponse

    request  = post "AddClientIDToOpenIDConnectProvider"
    response = nullResponse AddClientIDToOpenIDConnectProviderResponse

instance ToPath AddClientIDToOpenIDConnectProvider where
    toPath = const "/"

instance ToHeaders AddClientIDToOpenIDConnectProvider

instance ToQuery AddClientIDToOpenIDConnectProvider
