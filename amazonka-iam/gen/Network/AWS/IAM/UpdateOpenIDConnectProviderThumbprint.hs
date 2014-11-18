{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.UpdateOpenIDConnectProviderThumbprint
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Replaces the existing list of server certificate thumbprints with a new
-- list. The list that you pass with this action completely replaces the
-- existing list of thumbprints. (The lists are not merged.) Typically, you
-- need to update a thumbprint only when the identity provider's certificate
-- changes, which occurs rarely. However, if the provider's certificate does
-- change, any attempt to assume an IAM role that specifies the IAM provider
-- as a principal will fail until the certificate thumbprint is updated.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_UpdateOpenIDConnectProviderThumbprint.html>
module Network.AWS.IAM.UpdateOpenIDConnectProviderThumbprint
    (
    -- * Request
      UpdateOpenIDConnectProviderThumbprint
    -- ** Request constructor
    , updateOpenIDConnectProviderThumbprint
    -- ** Request lenses
    , uoidcptOpenIDConnectProviderArn
    , uoidcptThumbprintList

    -- * Response
    , UpdateOpenIDConnectProviderThumbprintResponse
    -- ** Response constructor
    , updateOpenIDConnectProviderThumbprintResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data UpdateOpenIDConnectProviderThumbprint = UpdateOpenIDConnectProviderThumbprint
    { _uoidcptOpenIDConnectProviderArn :: Text
    , _uoidcptThumbprintList           :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'UpdateOpenIDConnectProviderThumbprint' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uoidcptOpenIDConnectProviderArn' @::@ 'Text'
--
-- * 'uoidcptThumbprintList' @::@ ['Text']
--
updateOpenIDConnectProviderThumbprint :: Text -- ^ 'uoidcptOpenIDConnectProviderArn'
                                      -> UpdateOpenIDConnectProviderThumbprint
updateOpenIDConnectProviderThumbprint p1 = UpdateOpenIDConnectProviderThumbprint
    { _uoidcptOpenIDConnectProviderArn = p1
    , _uoidcptThumbprintList           = mempty
    }

-- | The Amazon Resource Name (ARN) of the IAM OpenID Connect (OIDC) provider
-- to update the thumbprint for. You can get a list of OIDC provider ARNs by
-- using the ListOpenIDConnectProviders action.
uoidcptOpenIDConnectProviderArn :: Lens' UpdateOpenIDConnectProviderThumbprint Text
uoidcptOpenIDConnectProviderArn =
    lens _uoidcptOpenIDConnectProviderArn
        (\s a -> s { _uoidcptOpenIDConnectProviderArn = a })

-- | A list of certificate thumbprints that are associated with the specified
-- IAM OpenID Connect provider. For more information, see
-- CreateOpenIDConnectProvider.
uoidcptThumbprintList :: Lens' UpdateOpenIDConnectProviderThumbprint [Text]
uoidcptThumbprintList =
    lens _uoidcptThumbprintList (\s a -> s { _uoidcptThumbprintList = a })

data UpdateOpenIDConnectProviderThumbprintResponse = UpdateOpenIDConnectProviderThumbprintResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'UpdateOpenIDConnectProviderThumbprintResponse' constructor.
updateOpenIDConnectProviderThumbprintResponse :: UpdateOpenIDConnectProviderThumbprintResponse
updateOpenIDConnectProviderThumbprintResponse = UpdateOpenIDConnectProviderThumbprintResponse

instance ToPath UpdateOpenIDConnectProviderThumbprint where
    toPath = const "/"

instance ToQuery UpdateOpenIDConnectProviderThumbprint

instance ToHeaders UpdateOpenIDConnectProviderThumbprint

instance AWSRequest UpdateOpenIDConnectProviderThumbprint where
    type Sv UpdateOpenIDConnectProviderThumbprint = IAM
    type Rs UpdateOpenIDConnectProviderThumbprint = UpdateOpenIDConnectProviderThumbprintResponse

    request  = post "UpdateOpenIDConnectProviderThumbprint"
    response = nullResponse UpdateOpenIDConnectProviderThumbprintResponse
