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
-- Module      : Network.AWS.IAM.RemoveClientIdFromOpenIdConnectProvider
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified client ID (also known as audience) from the list of client IDs registered for the specified IAM OpenID Connect (OIDC) provider resource object.
--
--
-- This operation is idempotent; it does not fail or return an error if you try to remove a client ID that does not exist.
--
module Network.AWS.IAM.RemoveClientIdFromOpenIdConnectProvider
    (
    -- * Creating a Request
      removeClientIdFromOpenIdConnectProvider
    , RemoveClientIdFromOpenIdConnectProvider
    -- * Request Lenses
    , rcifoicpOpenIdConnectProviderARN
    , rcifoicpClientId

    -- * Destructuring the Response
    , removeClientIdFromOpenIdConnectProviderResponse
    , RemoveClientIdFromOpenIdConnectProviderResponse
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'removeClientIdFromOpenIdConnectProvider' smart constructor.
data RemoveClientIdFromOpenIdConnectProvider = RemoveClientIdFromOpenIdConnectProvider'
  { _rcifoicpOpenIdConnectProviderARN :: !Text
  , _rcifoicpClientId                 :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemoveClientIdFromOpenIdConnectProvider' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcifoicpOpenIdConnectProviderARN' - The Amazon Resource Name (ARN) of the IAM OIDC provider resource to remove the client ID from. You can get a list of OIDC provider ARNs by using the 'ListOpenIDConnectProviders' operation. For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- * 'rcifoicpClientId' - The client ID (also known as audience) to remove from the IAM OIDC provider resource. For more information about client IDs, see 'CreateOpenIDConnectProvider' .
removeClientIdFromOpenIdConnectProvider
    :: Text -- ^ 'rcifoicpOpenIdConnectProviderARN'
    -> Text -- ^ 'rcifoicpClientId'
    -> RemoveClientIdFromOpenIdConnectProvider
removeClientIdFromOpenIdConnectProvider pOpenIdConnectProviderARN_ pClientId_ =
  RemoveClientIdFromOpenIdConnectProvider'
    { _rcifoicpOpenIdConnectProviderARN = pOpenIdConnectProviderARN_
    , _rcifoicpClientId = pClientId_
    }


-- | The Amazon Resource Name (ARN) of the IAM OIDC provider resource to remove the client ID from. You can get a list of OIDC provider ARNs by using the 'ListOpenIDConnectProviders' operation. For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
rcifoicpOpenIdConnectProviderARN :: Lens' RemoveClientIdFromOpenIdConnectProvider Text
rcifoicpOpenIdConnectProviderARN = lens _rcifoicpOpenIdConnectProviderARN (\ s a -> s{_rcifoicpOpenIdConnectProviderARN = a})

-- | The client ID (also known as audience) to remove from the IAM OIDC provider resource. For more information about client IDs, see 'CreateOpenIDConnectProvider' .
rcifoicpClientId :: Lens' RemoveClientIdFromOpenIdConnectProvider Text
rcifoicpClientId = lens _rcifoicpClientId (\ s a -> s{_rcifoicpClientId = a})

instance AWSRequest
           RemoveClientIdFromOpenIdConnectProvider
         where
        type Rs RemoveClientIdFromOpenIdConnectProvider =
             RemoveClientIdFromOpenIdConnectProviderResponse
        request = postQuery iam
        response
          = receiveNull
              RemoveClientIdFromOpenIdConnectProviderResponse'

instance Hashable
           RemoveClientIdFromOpenIdConnectProvider
         where

instance NFData
           RemoveClientIdFromOpenIdConnectProvider
         where

instance ToHeaders
           RemoveClientIdFromOpenIdConnectProvider
         where
        toHeaders = const mempty

instance ToPath
           RemoveClientIdFromOpenIdConnectProvider
         where
        toPath = const "/"

instance ToQuery
           RemoveClientIdFromOpenIdConnectProvider
         where
        toQuery RemoveClientIdFromOpenIdConnectProvider'{..}
          = mconcat
              ["Action" =:
                 ("RemoveClientIDFromOpenIDConnectProvider" ::
                    ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "OpenIDConnectProviderArn" =:
                 _rcifoicpOpenIdConnectProviderARN,
               "ClientID" =: _rcifoicpClientId]

-- | /See:/ 'removeClientIdFromOpenIdConnectProviderResponse' smart constructor.
data RemoveClientIdFromOpenIdConnectProviderResponse =
  RemoveClientIdFromOpenIdConnectProviderResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemoveClientIdFromOpenIdConnectProviderResponse' with the minimum fields required to make a request.
--
removeClientIdFromOpenIdConnectProviderResponse
    :: RemoveClientIdFromOpenIdConnectProviderResponse
removeClientIdFromOpenIdConnectProviderResponse =
  RemoveClientIdFromOpenIdConnectProviderResponse'


instance NFData
           RemoveClientIdFromOpenIdConnectProviderResponse
         where
