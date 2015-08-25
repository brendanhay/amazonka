{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.AddClientIdToOpenIdConnectProvider
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a new client ID (also known as audience) to the list of client IDs
-- already registered for the specified IAM OpenID Connect provider.
--
-- This action is idempotent; it does not fail or return an error if you
-- add an existing client ID to the provider.
--
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_AddClientIdToOpenIdConnectProvider.html AWS API Reference> for AddClientIdToOpenIdConnectProvider.
module Network.AWS.IAM.AddClientIdToOpenIdConnectProvider
    (
    -- * Creating a Request
      addClientIdToOpenIdConnectProvider
    , AddClientIdToOpenIdConnectProvider
    -- * Request Lenses
    , acitoicpOpenIdConnectProviderARN
    , acitoicpClientId

    -- * Destructuring the Response
    , addClientIdToOpenIdConnectProviderResponse
    , AddClientIdToOpenIdConnectProviderResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'addClientIdToOpenIdConnectProvider' smart constructor.
data AddClientIdToOpenIdConnectProvider = AddClientIdToOpenIdConnectProvider'
    { _acitoicpOpenIdConnectProviderARN :: !Text
    , _acitoicpClientId                 :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AddClientIdToOpenIdConnectProvider' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acitoicpOpenIdConnectProviderARN'
--
-- * 'acitoicpClientId'
addClientIdToOpenIdConnectProvider
    :: Text -- ^ 'acitoicpOpenIdConnectProviderARN'
    -> Text -- ^ 'acitoicpClientId'
    -> AddClientIdToOpenIdConnectProvider
addClientIdToOpenIdConnectProvider pOpenIdConnectProviderARN_ pClientId_ =
    AddClientIdToOpenIdConnectProvider'
    { _acitoicpOpenIdConnectProviderARN = pOpenIdConnectProviderARN_
    , _acitoicpClientId = pClientId_
    }

-- | The Amazon Resource Name (ARN) of the IAM OpenID Connect (OIDC) provider
-- to add the client ID to. You can get a list of OIDC provider ARNs by
-- using the ListOpenIDConnectProviders action.
acitoicpOpenIdConnectProviderARN :: Lens' AddClientIdToOpenIdConnectProvider Text
acitoicpOpenIdConnectProviderARN = lens _acitoicpOpenIdConnectProviderARN (\ s a -> s{_acitoicpOpenIdConnectProviderARN = a});

-- | The client ID (also known as audience) to add to the IAM OpenID Connect
-- provider.
acitoicpClientId :: Lens' AddClientIdToOpenIdConnectProvider Text
acitoicpClientId = lens _acitoicpClientId (\ s a -> s{_acitoicpClientId = a});

instance AWSRequest
         AddClientIdToOpenIdConnectProvider where
        type Rs AddClientIdToOpenIdConnectProvider =
             AddClientIdToOpenIdConnectProviderResponse
        request = postQuery iAM
        response
          = receiveNull
              AddClientIdToOpenIdConnectProviderResponse'

instance ToHeaders AddClientIdToOpenIdConnectProvider
         where
        toHeaders = const mempty

instance ToPath AddClientIdToOpenIdConnectProvider
         where
        toPath = const "/"

instance ToQuery AddClientIdToOpenIdConnectProvider
         where
        toQuery AddClientIdToOpenIdConnectProvider'{..}
          = mconcat
              ["Action" =:
                 ("AddClientIDToOpenIDConnectProvider" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "OpenIDConnectProviderArn" =:
                 _acitoicpOpenIdConnectProviderARN,
               "ClientID" =: _acitoicpClientId]

-- | /See:/ 'addClientIdToOpenIdConnectProviderResponse' smart constructor.
data AddClientIdToOpenIdConnectProviderResponse =
    AddClientIdToOpenIdConnectProviderResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AddClientIdToOpenIdConnectProviderResponse' with the minimum fields required to make a request.
--
addClientIdToOpenIdConnectProviderResponse
    :: AddClientIdToOpenIdConnectProviderResponse
addClientIdToOpenIdConnectProviderResponse =
    AddClientIdToOpenIdConnectProviderResponse'
