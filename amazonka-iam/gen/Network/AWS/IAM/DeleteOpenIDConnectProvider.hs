{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.IAM.DeleteOpenIDConnectProvider
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes an IAM OpenID Connect identity provider. Deleting an OIDC provider
-- does not update any roles that reference the provider as a principal in
-- their trust policies. Any attempt to assume a role that references a
-- provider that has been deleted will fail. This action is idempotent; it
-- does not fail or return an error if you call the action for a provider that
-- was already deleted.
module Network.AWS.IAM.DeleteOpenIDConnectProvider
    (
    -- * Request
      DeleteOpenIDConnectProvider
    -- ** Request constructor
    , deleteOpenIDConnectProvider
    -- ** Request lenses
    , doidcpOpenIDConnectProviderArn

    -- * Response
    , DeleteOpenIDConnectProviderResponse
    -- ** Response constructor
    , deleteOpenIDConnectProviderResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types

newtype DeleteOpenIDConnectProvider = DeleteOpenIDConnectProvider
    { _doidcpOpenIDConnectProviderArn :: Text
    } (Eq, Ord, Show, Generic, Monoid)

-- | 'DeleteOpenIDConnectProvider' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'doidcpOpenIDConnectProviderArn' @::@ 'Text'
--
deleteOpenIDConnectProvider :: Text -- ^ 'doidcpOpenIDConnectProviderArn'
                            -> DeleteOpenIDConnectProvider
deleteOpenIDConnectProvider p1 = DeleteOpenIDConnectProvider
    { _doidcpOpenIDConnectProviderArn = p1
    }

-- | The Amazon Resource Name (ARN) of the IAM OpenID Connect provider to
-- delete. You can get a list of OpenID Connect provider ARNs by using the
-- ListOpenIDConnectProviders action.
doidcpOpenIDConnectProviderArn :: Lens' DeleteOpenIDConnectProvider Text
doidcpOpenIDConnectProviderArn =
    lens _doidcpOpenIDConnectProviderArn
        (\s a -> s { _doidcpOpenIDConnectProviderArn = a })
instance ToQuery DeleteOpenIDConnectProvider

instance ToPath DeleteOpenIDConnectProvider where
    toPath = const "/"

data DeleteOpenIDConnectProviderResponse = DeleteOpenIDConnectProviderResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteOpenIDConnectProviderResponse' constructor.
deleteOpenIDConnectProviderResponse :: DeleteOpenIDConnectProviderResponse
deleteOpenIDConnectProviderResponse = DeleteOpenIDConnectProviderResponse

instance FromXML DeleteOpenIDConnectProviderResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteOpenIDConnectProviderResponse"

instance AWSRequest DeleteOpenIDConnectProvider where
    type Sv DeleteOpenIDConnectProvider = IAM
    type Rs DeleteOpenIDConnectProvider = DeleteOpenIDConnectProviderResponse

    request  = post "DeleteOpenIDConnectProvider"
    response = nullaryResponse DeleteOpenIDConnectProviderResponse
