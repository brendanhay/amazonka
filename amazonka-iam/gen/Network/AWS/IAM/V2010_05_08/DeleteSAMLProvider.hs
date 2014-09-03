{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.DeleteSAMLProvider
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
-- deleted will fail. This operation requires Signature Version 4.
-- https://iam.amazonaws.com/ ?Action=DeleteSAMLProvider
-- &Name=arn:aws:iam::123456789012:saml-metadata/MyUniversity
-- &Version=2010-05-08 &AUTHPARAMS.
module Network.AWS.IAM.V2010_05_08.DeleteSAMLProvider
    (
    -- * Request
      DeleteSAMLProvider
    -- ** Request constructor
    , deleteSAMLProvider
    -- ** Request lenses
    , dsamlprSAMLProviderArn

    -- * Response
    , DeleteSAMLProviderResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeleteSAMLProvider' request.
deleteSAMLProvider :: Text -- ^ 'dsamlprSAMLProviderArn'
                   -> DeleteSAMLProvider
deleteSAMLProvider p1 = DeleteSAMLProvider
    { _dsamlprSAMLProviderArn = p1
    }

data DeleteSAMLProvider = DeleteSAMLProvider
    { _dsamlprSAMLProviderArn :: Text
      -- ^ The Amazon Resource Name (ARN) of the SAML provider to delete.
    } deriving (Show, Generic)

-- | The Amazon Resource Name (ARN) of the SAML provider to delete.
dsamlprSAMLProviderArn
    :: Functor f
    => (Text
    -> f (Text))
    -> DeleteSAMLProvider
    -> f DeleteSAMLProvider
dsamlprSAMLProviderArn f x =
    (\y -> x { _dsamlprSAMLProviderArn = y })
       <$> f (_dsamlprSAMLProviderArn x)
{-# INLINE dsamlprSAMLProviderArn #-}

instance ToQuery DeleteSAMLProvider where
    toQuery = genericQuery def

data DeleteSAMLProviderResponse = DeleteSAMLProviderResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteSAMLProvider where
    type Sv DeleteSAMLProvider = IAM
    type Rs DeleteSAMLProvider = DeleteSAMLProviderResponse

    request = post "DeleteSAMLProvider"
    response _ = nullaryResponse DeleteSAMLProviderResponse
