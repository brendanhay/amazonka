{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteOpenIdConnectProvider
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes an IAM OpenID Connect identity provider.
--
-- Deleting an OIDC provider does not update any roles that reference the
-- provider as a principal in their trust policies. Any attempt to assume a
-- role that references a provider that has been deleted will fail.
--
-- This action is idempotent; it does not fail or return an error if you
-- call the action for a provider that was already deleted.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteOpenIdConnectProvider.html>
module Network.AWS.IAM.DeleteOpenIdConnectProvider
    (
    -- * Request
      DeleteOpenIdConnectProvider
    -- ** Request constructor
    , deleteOpenIdConnectProvider
    -- ** Request lenses
    , doicpOpenIdConnectProviderARN

    -- * Response
    , DeleteOpenIdConnectProviderResponse
    -- ** Response constructor
    , deleteOpenIdConnectProviderResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteOpenIdConnectProvider' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'doicpOpenIdConnectProviderARN'
newtype DeleteOpenIdConnectProvider = DeleteOpenIdConnectProvider'
    { _doicpOpenIdConnectProviderARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteOpenIdConnectProvider' smart constructor.
deleteOpenIdConnectProvider :: Text -> DeleteOpenIdConnectProvider
deleteOpenIdConnectProvider pOpenIdConnectProviderARN_ =
    DeleteOpenIdConnectProvider'
    { _doicpOpenIdConnectProviderARN = pOpenIdConnectProviderARN_
    }

-- | The Amazon Resource Name (ARN) of the IAM OpenID Connect provider to
-- delete. You can get a list of OpenID Connect provider ARNs by using the
-- ListOpenIDConnectProviders action.
doicpOpenIdConnectProviderARN :: Lens' DeleteOpenIdConnectProvider Text
doicpOpenIdConnectProviderARN = lens _doicpOpenIdConnectProviderARN (\ s a -> s{_doicpOpenIdConnectProviderARN = a});

instance AWSRequest DeleteOpenIdConnectProvider where
        type Sv DeleteOpenIdConnectProvider = IAM
        type Rs DeleteOpenIdConnectProvider =
             DeleteOpenIdConnectProviderResponse
        request = post
        response
          = receiveNull DeleteOpenIdConnectProviderResponse'

instance ToHeaders DeleteOpenIdConnectProvider where
        toHeaders = const mempty

instance ToPath DeleteOpenIdConnectProvider where
        toPath = const "/"

instance ToQuery DeleteOpenIdConnectProvider where
        toQuery DeleteOpenIdConnectProvider'{..}
          = mconcat
              ["Action" =:
                 ("DeleteOpenIdConnectProvider" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "OpenIDConnectProviderArn" =:
                 _doicpOpenIdConnectProviderARN]

-- | /See:/ 'deleteOpenIdConnectProviderResponse' smart constructor.
data DeleteOpenIdConnectProviderResponse =
    DeleteOpenIdConnectProviderResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteOpenIdConnectProviderResponse' smart constructor.
deleteOpenIdConnectProviderResponse :: DeleteOpenIdConnectProviderResponse
deleteOpenIdConnectProviderResponse = DeleteOpenIdConnectProviderResponse'
