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

-- Module      : Network.AWS.IAM.RemoveClientIDFromOpenIDConnectProvider
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Removes the specified client ID (also known as audience) from the list of
-- client IDs registered for the specified IAM OpenID Connect provider. This
-- action is idempotent; it does not fail or return an error if you try to
-- remove a client ID that was removed previously.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_RemoveClientIDFromOpenIDConnectProvider.html>
module Network.AWS.IAM.RemoveClientIDFromOpenIDConnectProvider
    (
    -- * Request
      RemoveClientIDFromOpenIDConnectProvider
    -- ** Request constructor
    , removeClientIDFromOpenIDConnectProvider
    -- ** Request lenses
    , rcidfoidcpClientID
    , rcidfoidcpOpenIDConnectProviderArn

    -- * Response
    , RemoveClientIDFromOpenIDConnectProviderResponse
    -- ** Response constructor
    , removeClientIDFromOpenIDConnectProviderResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data RemoveClientIDFromOpenIDConnectProvider = RemoveClientIDFromOpenIDConnectProvider
    { _rcidfoidcpClientID                 :: Text
    , _rcidfoidcpOpenIDConnectProviderArn :: Text
    } deriving (Eq, Ord, Show)

-- | 'RemoveClientIDFromOpenIDConnectProvider' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcidfoidcpClientID' @::@ 'Text'
--
-- * 'rcidfoidcpOpenIDConnectProviderArn' @::@ 'Text'
--
removeClientIDFromOpenIDConnectProvider :: Text -- ^ 'rcidfoidcpOpenIDConnectProviderArn'
                                        -> Text -- ^ 'rcidfoidcpClientID'
                                        -> RemoveClientIDFromOpenIDConnectProvider
removeClientIDFromOpenIDConnectProvider p1 p2 = RemoveClientIDFromOpenIDConnectProvider
    { _rcidfoidcpOpenIDConnectProviderArn = p1
    , _rcidfoidcpClientID                 = p2
    }

-- | The client ID (also known as audience) to remove from the IAM OpenID
-- Connect provider. For more information about client IDs, see
-- CreateOpenIDConnectProvider>.
rcidfoidcpClientID :: Lens' RemoveClientIDFromOpenIDConnectProvider Text
rcidfoidcpClientID =
    lens _rcidfoidcpClientID (\s a -> s { _rcidfoidcpClientID = a })

-- | The Amazon Resource Name (ARN) of the IAM OpenID Connect (OIDC) provider
-- to remove the client ID from. You can get a list of OIDC provider ARNs by
-- using the ListOpenIDConnectProviders> action.
rcidfoidcpOpenIDConnectProviderArn :: Lens' RemoveClientIDFromOpenIDConnectProvider Text
rcidfoidcpOpenIDConnectProviderArn =
    lens _rcidfoidcpOpenIDConnectProviderArn
        (\s a -> s { _rcidfoidcpOpenIDConnectProviderArn = a })

data RemoveClientIDFromOpenIDConnectProviderResponse = RemoveClientIDFromOpenIDConnectProviderResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'RemoveClientIDFromOpenIDConnectProviderResponse' constructor.
removeClientIDFromOpenIDConnectProviderResponse :: RemoveClientIDFromOpenIDConnectProviderResponse
removeClientIDFromOpenIDConnectProviderResponse = RemoveClientIDFromOpenIDConnectProviderResponse

instance ToPath RemoveClientIDFromOpenIDConnectProvider where
    toPath = const "/"

instance ToQuery RemoveClientIDFromOpenIDConnectProvider where
    toQuery RemoveClientIDFromOpenIDConnectProvider{..} = mconcat
        [ "ClientID"                 =? _rcidfoidcpClientID
        , "OpenIDConnectProviderArn" =? _rcidfoidcpOpenIDConnectProviderArn
        ]

instance ToHeaders RemoveClientIDFromOpenIDConnectProvider

instance AWSRequest RemoveClientIDFromOpenIDConnectProvider where
    type Sv RemoveClientIDFromOpenIDConnectProvider = IAM
    type Rs RemoveClientIDFromOpenIDConnectProvider = RemoveClientIDFromOpenIDConnectProviderResponse

    request  = post "RemoveClientIDFromOpenIDConnectProvider"
    response = nullResponse RemoveClientIDFromOpenIDConnectProviderResponse
