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

-- Module      : Network.AWS.IAM.DeleteSAMLProvider
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a SAML provider. Deleting the provider does not update any roles
-- that reference the SAML provider as a principal in their trust policies.
-- Any attempt to assume a role that references a SAML provider that has been
-- deleted will fail.
module Network.AWS.IAM.DeleteSAMLProvider
    (
    -- * Request
      DeleteSAMLProvider
    -- ** Request constructor
    , deleteSAMLProvider
    -- ** Request lenses
    , dsamlpSAMLProviderArn

    -- * Response
    , DeleteSAMLProviderResponse
    -- ** Response constructor
    , deleteSAMLProviderResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types

newtype DeleteSAMLProvider = DeleteSAMLProvider
    { _dsamlpSAMLProviderArn :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DeleteSAMLProvider' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsamlpSAMLProviderArn' @::@ 'Text'
--
deleteSAMLProvider :: Text -- ^ 'dsamlpSAMLProviderArn'
                   -> DeleteSAMLProvider
deleteSAMLProvider p1 = DeleteSAMLProvider
    { _dsamlpSAMLProviderArn = p1
    }

-- | The Amazon Resource Name (ARN) of the SAML provider to delete.
dsamlpSAMLProviderArn :: Lens' DeleteSAMLProvider Text
dsamlpSAMLProviderArn =
    lens _dsamlpSAMLProviderArn (\s a -> s { _dsamlpSAMLProviderArn = a })

instance ToQuery DeleteSAMLProvider

instance ToPath DeleteSAMLProvider where
    toPath = const "/"

data DeleteSAMLProviderResponse = DeleteSAMLProviderResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteSAMLProviderResponse' constructor.
deleteSAMLProviderResponse :: DeleteSAMLProviderResponse
deleteSAMLProviderResponse = DeleteSAMLProviderResponse

instance FromXML DeleteSAMLProviderResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteSAMLProviderResponse"

instance AWSRequest DeleteSAMLProvider where
    type Sv DeleteSAMLProvider = IAM
    type Rs DeleteSAMLProvider = DeleteSAMLProviderResponse

    request  = post "DeleteSAMLProvider"
    response = nullaryResponse DeleteSAMLProviderResponse
