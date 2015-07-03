{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.IAM.DeleteOpenIDConnectProvider
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

-- | Deletes an IAM OpenID Connect identity provider.
--
-- Deleting an OIDC provider does not update any roles that reference the
-- provider as a principal in their trust policies. Any attempt to assume a
-- role that references a provider that has been deleted will fail.
--
-- This action is idempotent; it does not fail or return an error if you
-- call the action for a provider that was already deleted.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteOpenIDConnectProvider.html>
module Network.AWS.IAM.DeleteOpenIDConnectProvider
    (
    -- * Request
      DeleteOpenIDConnectProvider
    -- ** Request constructor
    , deleteOpenIDConnectProvider
    -- ** Request lenses
    , doidcpOpenIDConnectProviderARN

    -- * Response
    , DeleteOpenIDConnectProviderResponse
    -- ** Response constructor
    , deleteOpenIDConnectProviderResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteOpenIDConnectProvider' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'doidcpOpenIDConnectProviderARN'
newtype DeleteOpenIDConnectProvider = DeleteOpenIDConnectProvider'
    { _doidcpOpenIDConnectProviderARN :: Text
    } deriving (Eq,Read,Show)

-- | 'DeleteOpenIDConnectProvider' smart constructor.
deleteOpenIDConnectProvider :: Text -> DeleteOpenIDConnectProvider
deleteOpenIDConnectProvider pOpenIDConnectProviderARN =
    DeleteOpenIDConnectProvider'
    { _doidcpOpenIDConnectProviderARN = pOpenIDConnectProviderARN
    }

-- | The Amazon Resource Name (ARN) of the IAM OpenID Connect provider to
-- delete. You can get a list of OpenID Connect provider ARNs by using the
-- ListOpenIDConnectProviders action.
doidcpOpenIDConnectProviderARN :: Lens' DeleteOpenIDConnectProvider Text
doidcpOpenIDConnectProviderARN = lens _doidcpOpenIDConnectProviderARN (\ s a -> s{_doidcpOpenIDConnectProviderARN = a});

instance AWSRequest DeleteOpenIDConnectProvider where
        type Sv DeleteOpenIDConnectProvider = IAM
        type Rs DeleteOpenIDConnectProvider =
             DeleteOpenIDConnectProviderResponse
        request = post
        response
          = receiveNull DeleteOpenIDConnectProviderResponse'

instance ToHeaders DeleteOpenIDConnectProvider where
        toHeaders = const mempty

instance ToPath DeleteOpenIDConnectProvider where
        toPath = const "/"

instance ToQuery DeleteOpenIDConnectProvider where
        toQuery DeleteOpenIDConnectProvider'{..}
          = mconcat
              ["Action" =:
                 ("DeleteOpenIDConnectProvider" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "OpenIDConnectProviderArn" =:
                 _doidcpOpenIDConnectProviderARN]

-- | /See:/ 'deleteOpenIDConnectProviderResponse' smart constructor.
data DeleteOpenIDConnectProviderResponse =
    DeleteOpenIDConnectProviderResponse'
    deriving (Eq,Read,Show)

-- | 'DeleteOpenIDConnectProviderResponse' smart constructor.
deleteOpenIDConnectProviderResponse :: DeleteOpenIDConnectProviderResponse
deleteOpenIDConnectProviderResponse = DeleteOpenIDConnectProviderResponse'
