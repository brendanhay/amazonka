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
-- Module      : Network.AWS.IAM.DeleteOpenIdConnectProvider
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an OpenID Connect identity provider (IdP) resource object in IAM.
--
--
-- Deleting an IAM OIDC provider resource does not update any roles that reference the provider as a principal in their trust policies. Any attempt to assume a role that references a deleted provider fails.
--
-- This operation is idempotent; it does not fail or return an error if you call the operation for a provider that does not exist.
--
module Network.AWS.IAM.DeleteOpenIdConnectProvider
    (
    -- * Creating a Request
      deleteOpenIdConnectProvider
    , DeleteOpenIdConnectProvider
    -- * Request Lenses
    , doicpOpenIdConnectProviderARN

    -- * Destructuring the Response
    , deleteOpenIdConnectProviderResponse
    , DeleteOpenIdConnectProviderResponse
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteOpenIdConnectProvider' smart constructor.
newtype DeleteOpenIdConnectProvider = DeleteOpenIdConnectProvider'
  { _doicpOpenIdConnectProviderARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteOpenIdConnectProvider' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'doicpOpenIdConnectProviderARN' - The Amazon Resource Name (ARN) of the IAM OpenID Connect provider resource object to delete. You can get a list of OpenID Connect provider resource ARNs by using the 'ListOpenIDConnectProviders' operation.
deleteOpenIdConnectProvider
    :: Text -- ^ 'doicpOpenIdConnectProviderARN'
    -> DeleteOpenIdConnectProvider
deleteOpenIdConnectProvider pOpenIdConnectProviderARN_ =
  DeleteOpenIdConnectProvider'
    {_doicpOpenIdConnectProviderARN = pOpenIdConnectProviderARN_}


-- | The Amazon Resource Name (ARN) of the IAM OpenID Connect provider resource object to delete. You can get a list of OpenID Connect provider resource ARNs by using the 'ListOpenIDConnectProviders' operation.
doicpOpenIdConnectProviderARN :: Lens' DeleteOpenIdConnectProvider Text
doicpOpenIdConnectProviderARN = lens _doicpOpenIdConnectProviderARN (\ s a -> s{_doicpOpenIdConnectProviderARN = a})

instance AWSRequest DeleteOpenIdConnectProvider where
        type Rs DeleteOpenIdConnectProvider =
             DeleteOpenIdConnectProviderResponse
        request = postQuery iam
        response
          = receiveNull DeleteOpenIdConnectProviderResponse'

instance Hashable DeleteOpenIdConnectProvider where

instance NFData DeleteOpenIdConnectProvider where

instance ToHeaders DeleteOpenIdConnectProvider where
        toHeaders = const mempty

instance ToPath DeleteOpenIdConnectProvider where
        toPath = const "/"

instance ToQuery DeleteOpenIdConnectProvider where
        toQuery DeleteOpenIdConnectProvider'{..}
          = mconcat
              ["Action" =:
                 ("DeleteOpenIDConnectProvider" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "OpenIDConnectProviderArn" =:
                 _doicpOpenIdConnectProviderARN]

-- | /See:/ 'deleteOpenIdConnectProviderResponse' smart constructor.
data DeleteOpenIdConnectProviderResponse =
  DeleteOpenIdConnectProviderResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteOpenIdConnectProviderResponse' with the minimum fields required to make a request.
--
deleteOpenIdConnectProviderResponse
    :: DeleteOpenIdConnectProviderResponse
deleteOpenIdConnectProviderResponse = DeleteOpenIdConnectProviderResponse'


instance NFData DeleteOpenIdConnectProviderResponse
         where
