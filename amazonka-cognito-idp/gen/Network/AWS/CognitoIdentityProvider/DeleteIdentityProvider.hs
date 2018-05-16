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
-- Module      : Network.AWS.CognitoIdentityProvider.DeleteIdentityProvider
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an identity provider for a user pool.
--
--
module Network.AWS.CognitoIdentityProvider.DeleteIdentityProvider
    (
    -- * Creating a Request
      deleteIdentityProvider
    , DeleteIdentityProvider
    -- * Request Lenses
    , delUserPoolId
    , delProviderName

    -- * Destructuring the Response
    , deleteIdentityProviderResponse
    , DeleteIdentityProviderResponse
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteIdentityProvider' smart constructor.
data DeleteIdentityProvider = DeleteIdentityProvider'
  { _delUserPoolId   :: !Text
  , _delProviderName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteIdentityProvider' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delUserPoolId' - The user pool ID.
--
-- * 'delProviderName' - The identity provider name.
deleteIdentityProvider
    :: Text -- ^ 'delUserPoolId'
    -> Text -- ^ 'delProviderName'
    -> DeleteIdentityProvider
deleteIdentityProvider pUserPoolId_ pProviderName_ =
  DeleteIdentityProvider'
    {_delUserPoolId = pUserPoolId_, _delProviderName = pProviderName_}


-- | The user pool ID.
delUserPoolId :: Lens' DeleteIdentityProvider Text
delUserPoolId = lens _delUserPoolId (\ s a -> s{_delUserPoolId = a})

-- | The identity provider name.
delProviderName :: Lens' DeleteIdentityProvider Text
delProviderName = lens _delProviderName (\ s a -> s{_delProviderName = a})

instance AWSRequest DeleteIdentityProvider where
        type Rs DeleteIdentityProvider =
             DeleteIdentityProviderResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveNull DeleteIdentityProviderResponse'

instance Hashable DeleteIdentityProvider where

instance NFData DeleteIdentityProvider where

instance ToHeaders DeleteIdentityProvider where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.DeleteIdentityProvider"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteIdentityProvider where
        toJSON DeleteIdentityProvider'{..}
          = object
              (catMaybes
                 [Just ("UserPoolId" .= _delUserPoolId),
                  Just ("ProviderName" .= _delProviderName)])

instance ToPath DeleteIdentityProvider where
        toPath = const "/"

instance ToQuery DeleteIdentityProvider where
        toQuery = const mempty

-- | /See:/ 'deleteIdentityProviderResponse' smart constructor.
data DeleteIdentityProviderResponse =
  DeleteIdentityProviderResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteIdentityProviderResponse' with the minimum fields required to make a request.
--
deleteIdentityProviderResponse
    :: DeleteIdentityProviderResponse
deleteIdentityProviderResponse = DeleteIdentityProviderResponse'


instance NFData DeleteIdentityProviderResponse where
