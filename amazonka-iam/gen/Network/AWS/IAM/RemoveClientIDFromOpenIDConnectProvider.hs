{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.IAM.RemoveClientIDFromOpenIDConnectProvider
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Removes the specified client ID (also known as audience) from the list
-- of client IDs registered for the specified IAM OpenID Connect provider.
--
-- This action is idempotent; it does not fail or return an error if you
-- try to remove a client ID that was removed previously.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_RemoveClientIDFromOpenIDConnectProvider.html>
module Network.AWS.IAM.RemoveClientIDFromOpenIDConnectProvider
    (
    -- * Request
      RemoveClientIDFromOpenIDConnectProvider
    -- ** Request constructor
    , removeClientIDFromOpenIDConnectProvider
    -- ** Request lenses
    , rcidfoidcpOpenIDConnectProviderARN
    , rcidfoidcpClientID

    -- * Response
    , RemoveClientIDFromOpenIDConnectProviderResponse
    -- ** Response constructor
    , removeClientIDFromOpenIDConnectProviderResponse
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.IAM.Types

-- | /See:/ 'removeClientIDFromOpenIDConnectProvider' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcidfoidcpOpenIDConnectProviderARN'
--
-- * 'rcidfoidcpClientID'
data RemoveClientIDFromOpenIDConnectProvider = RemoveClientIDFromOpenIDConnectProvider'{_rcidfoidcpOpenIDConnectProviderARN :: Text, _rcidfoidcpClientID :: Text} deriving (Eq, Read, Show)

-- | 'RemoveClientIDFromOpenIDConnectProvider' smart constructor.
removeClientIDFromOpenIDConnectProvider :: Text -> Text -> RemoveClientIDFromOpenIDConnectProvider
removeClientIDFromOpenIDConnectProvider pOpenIDConnectProviderARN pClientID = RemoveClientIDFromOpenIDConnectProvider'{_rcidfoidcpOpenIDConnectProviderARN = pOpenIDConnectProviderARN, _rcidfoidcpClientID = pClientID};

-- | The Amazon Resource Name (ARN) of the IAM OpenID Connect (OIDC) provider
-- to remove the client ID from. You can get a list of OIDC provider ARNs
-- by using the ListOpenIDConnectProviders action.
rcidfoidcpOpenIDConnectProviderARN :: Lens' RemoveClientIDFromOpenIDConnectProvider Text
rcidfoidcpOpenIDConnectProviderARN = lens _rcidfoidcpOpenIDConnectProviderARN (\ s a -> s{_rcidfoidcpOpenIDConnectProviderARN = a});

-- | The client ID (also known as audience) to remove from the IAM OpenID
-- Connect provider. For more information about client IDs, see
-- CreateOpenIDConnectProvider.
rcidfoidcpClientID :: Lens' RemoveClientIDFromOpenIDConnectProvider Text
rcidfoidcpClientID = lens _rcidfoidcpClientID (\ s a -> s{_rcidfoidcpClientID = a});

instance AWSRequest
         RemoveClientIDFromOpenIDConnectProvider where
        type Sv RemoveClientIDFromOpenIDConnectProvider = IAM
        type Rs RemoveClientIDFromOpenIDConnectProvider =
             RemoveClientIDFromOpenIDConnectProviderResponse
        request = post
        response
          = receiveNull
              RemoveClientIDFromOpenIDConnectProviderResponse'

instance ToHeaders
         RemoveClientIDFromOpenIDConnectProvider where
        toHeaders = const mempty

instance ToPath
         RemoveClientIDFromOpenIDConnectProvider where
        toPath = const "/"

instance ToQuery
         RemoveClientIDFromOpenIDConnectProvider where
        toQuery RemoveClientIDFromOpenIDConnectProvider'{..}
          = mconcat
              ["Action" =:
                 ("RemoveClientIDFromOpenIDConnectProvider" ::
                    ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "OpenIDConnectProviderArn" =:
                 _rcidfoidcpOpenIDConnectProviderARN,
               "ClientID" =: _rcidfoidcpClientID]

-- | /See:/ 'removeClientIDFromOpenIDConnectProviderResponse' smart constructor.
data RemoveClientIDFromOpenIDConnectProviderResponse = RemoveClientIDFromOpenIDConnectProviderResponse' deriving (Eq, Read, Show)

-- | 'RemoveClientIDFromOpenIDConnectProviderResponse' smart constructor.
removeClientIDFromOpenIDConnectProviderResponse :: RemoveClientIDFromOpenIDConnectProviderResponse
removeClientIDFromOpenIDConnectProviderResponse = RemoveClientIDFromOpenIDConnectProviderResponse';
