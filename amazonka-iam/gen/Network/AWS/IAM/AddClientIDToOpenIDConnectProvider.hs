{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.IAM.AddClientIDToOpenIDConnectProvider
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Adds a new client ID (also known as audience) to the list of client IDs
-- already registered for the specified IAM OpenID Connect provider.
--
-- This action is idempotent; it does not fail or return an error if you
-- add an existing client ID to the provider.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_AddClientIDToOpenIDConnectProvider.html>
module Network.AWS.IAM.AddClientIDToOpenIDConnectProvider
    (
    -- * Request
      AddClientIDToOpenIDConnectProvider
    -- ** Request constructor
    , addClientIDToOpenIDConnectProvider
    -- ** Request lenses
    , acidtoidcpOpenIDConnectProviderARN
    , acidtoidcpClientID

    -- * Response
    , AddClientIDToOpenIDConnectProviderResponse
    -- ** Response constructor
    , addClientIDToOpenIDConnectProviderResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'addClientIDToOpenIDConnectProvider' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'acidtoidcpOpenIDConnectProviderARN'
--
-- * 'acidtoidcpClientID'
data AddClientIDToOpenIDConnectProvider = AddClientIDToOpenIDConnectProvider'
    { _acidtoidcpOpenIDConnectProviderARN :: !Text
    , _acidtoidcpClientID                 :: !Text
    } deriving (Eq,Read,Show)

-- | 'AddClientIDToOpenIDConnectProvider' smart constructor.
addClientIDToOpenIDConnectProvider :: Text -> Text -> AddClientIDToOpenIDConnectProvider
addClientIDToOpenIDConnectProvider pOpenIDConnectProviderARN pClientID =
    AddClientIDToOpenIDConnectProvider'
    { _acidtoidcpOpenIDConnectProviderARN = pOpenIDConnectProviderARN
    , _acidtoidcpClientID = pClientID
    }

-- | The Amazon Resource Name (ARN) of the IAM OpenID Connect (OIDC) provider
-- to add the client ID to. You can get a list of OIDC provider ARNs by
-- using the ListOpenIDConnectProviders action.
acidtoidcpOpenIDConnectProviderARN :: Lens' AddClientIDToOpenIDConnectProvider Text
acidtoidcpOpenIDConnectProviderARN = lens _acidtoidcpOpenIDConnectProviderARN (\ s a -> s{_acidtoidcpOpenIDConnectProviderARN = a});

-- | The client ID (also known as audience) to add to the IAM OpenID Connect
-- provider.
acidtoidcpClientID :: Lens' AddClientIDToOpenIDConnectProvider Text
acidtoidcpClientID = lens _acidtoidcpClientID (\ s a -> s{_acidtoidcpClientID = a});

instance AWSRequest
         AddClientIDToOpenIDConnectProvider where
        type Sv AddClientIDToOpenIDConnectProvider = IAM
        type Rs AddClientIDToOpenIDConnectProvider =
             AddClientIDToOpenIDConnectProviderResponse
        request = post
        response
          = receiveNull
              AddClientIDToOpenIDConnectProviderResponse'

instance ToHeaders AddClientIDToOpenIDConnectProvider
         where
        toHeaders = const mempty

instance ToPath AddClientIDToOpenIDConnectProvider
         where
        toPath = const "/"

instance ToQuery AddClientIDToOpenIDConnectProvider
         where
        toQuery AddClientIDToOpenIDConnectProvider'{..}
          = mconcat
              ["Action" =:
                 ("AddClientIDToOpenIDConnectProvider" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "OpenIDConnectProviderArn" =:
                 _acidtoidcpOpenIDConnectProviderARN,
               "ClientID" =: _acidtoidcpClientID]

-- | /See:/ 'addClientIDToOpenIDConnectProviderResponse' smart constructor.
data AddClientIDToOpenIDConnectProviderResponse =
    AddClientIDToOpenIDConnectProviderResponse'
    deriving (Eq,Read,Show)

-- | 'AddClientIDToOpenIDConnectProviderResponse' smart constructor.
addClientIDToOpenIDConnectProviderResponse :: AddClientIDToOpenIDConnectProviderResponse
addClientIDToOpenIDConnectProviderResponse =
    AddClientIDToOpenIDConnectProviderResponse'
