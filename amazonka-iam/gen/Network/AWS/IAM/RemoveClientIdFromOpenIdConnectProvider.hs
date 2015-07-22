{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.RemoveClientIdFromOpenIdConnectProvider
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified client ID (also known as audience) from the list
-- of client IDs registered for the specified IAM OpenID Connect provider.
--
-- This action is idempotent; it does not fail or return an error if you
-- try to remove a client ID that was removed previously.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_RemoveClientIdFromOpenIdConnectProvider.html>
module Network.AWS.IAM.RemoveClientIdFromOpenIdConnectProvider
    (
    -- * Request
      RemoveClientIdFromOpenIdConnectProvider
    -- ** Request constructor
    , removeClientIdFromOpenIdConnectProvider
    -- ** Request lenses
    , rcifoicprqOpenIdConnectProviderARN
    , rcifoicprqClientId

    -- * Response
    , RemoveClientIdFromOpenIdConnectProviderResponse
    -- ** Response constructor
    , removeClientIdFromOpenIdConnectProviderResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'removeClientIdFromOpenIdConnectProvider' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcifoicprqOpenIdConnectProviderARN'
--
-- * 'rcifoicprqClientId'
data RemoveClientIdFromOpenIdConnectProvider = RemoveClientIdFromOpenIdConnectProvider'
    { _rcifoicprqOpenIdConnectProviderARN :: !Text
    , _rcifoicprqClientId                 :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RemoveClientIdFromOpenIdConnectProvider' smart constructor.
removeClientIdFromOpenIdConnectProvider :: Text -> Text -> RemoveClientIdFromOpenIdConnectProvider
removeClientIdFromOpenIdConnectProvider pOpenIdConnectProviderARN pClientId =
    RemoveClientIdFromOpenIdConnectProvider'
    { _rcifoicprqOpenIdConnectProviderARN = pOpenIdConnectProviderARN
    , _rcifoicprqClientId = pClientId
    }

-- | The Amazon Resource Name (ARN) of the IAM OpenID Connect (OIDC) provider
-- to remove the client ID from. You can get a list of OIDC provider ARNs
-- by using the ListOpenIDConnectProviders action.
rcifoicprqOpenIdConnectProviderARN :: Lens' RemoveClientIdFromOpenIdConnectProvider Text
rcifoicprqOpenIdConnectProviderARN = lens _rcifoicprqOpenIdConnectProviderARN (\ s a -> s{_rcifoicprqOpenIdConnectProviderARN = a});

-- | The client ID (also known as audience) to remove from the IAM OpenID
-- Connect provider. For more information about client IDs, see
-- CreateOpenIDConnectProvider.
rcifoicprqClientId :: Lens' RemoveClientIdFromOpenIdConnectProvider Text
rcifoicprqClientId = lens _rcifoicprqClientId (\ s a -> s{_rcifoicprqClientId = a});

instance AWSRequest
         RemoveClientIdFromOpenIdConnectProvider where
        type Sv RemoveClientIdFromOpenIdConnectProvider = IAM
        type Rs RemoveClientIdFromOpenIdConnectProvider =
             RemoveClientIdFromOpenIdConnectProviderResponse
        request = post
        response
          = receiveNull
              RemoveClientIdFromOpenIdConnectProviderResponse'

instance ToHeaders
         RemoveClientIdFromOpenIdConnectProvider where
        toHeaders = const mempty

instance ToPath
         RemoveClientIdFromOpenIdConnectProvider where
        toPath = const "/"

instance ToQuery
         RemoveClientIdFromOpenIdConnectProvider where
        toQuery RemoveClientIdFromOpenIdConnectProvider'{..}
          = mconcat
              ["Action" =:
                 ("RemoveClientIdFromOpenIdConnectProvider" ::
                    ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "OpenIDConnectProviderArn" =:
                 _rcifoicprqOpenIdConnectProviderARN,
               "ClientID" =: _rcifoicprqClientId]

-- | /See:/ 'removeClientIdFromOpenIdConnectProviderResponse' smart constructor.
data RemoveClientIdFromOpenIdConnectProviderResponse =
    RemoveClientIdFromOpenIdConnectProviderResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RemoveClientIdFromOpenIdConnectProviderResponse' smart constructor.
removeClientIdFromOpenIdConnectProviderResponse :: RemoveClientIdFromOpenIdConnectProviderResponse
removeClientIdFromOpenIdConnectProviderResponse =
    RemoveClientIdFromOpenIdConnectProviderResponse'
