{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
-- deleted will fail. This operation requires Signature Version 4.
-- https://iam.amazonaws.com/ ?Action=DeleteSAMLProvider
-- &Name=arn:aws:iam::123456789012:saml-metadata/MyUniversity
-- &Version=2010-05-08 &AUTHPARAMS.
module Network.AWS.IAM.DeleteSAMLProvider
    (
    -- * Request
      DeleteSAMLProvider
    -- ** Request constructor
    , mkDeleteSAMLProvider
    -- ** Request lenses
    , dsamlpSAMLProviderArn

    -- * Response
    , DeleteSAMLProviderResponse
    -- ** Response constructor
    , mkDeleteSAMLProviderResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import Network.AWS.Prelude

newtype DeleteSAMLProvider = DeleteSAMLProvider
    { _dsamlpSAMLProviderArn :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteSAMLProvider' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SAMLProviderArn ::@ @Text@
--
mkDeleteSAMLProvider :: Text -- ^ 'dsamlpSAMLProviderArn'
                     -> DeleteSAMLProvider
mkDeleteSAMLProvider p1 = DeleteSAMLProvider
    { _dsamlpSAMLProviderArn = p1
    }

-- | The Amazon Resource Name (ARN) of the SAML provider to delete.
dsamlpSAMLProviderArn :: Lens' DeleteSAMLProvider Text
dsamlpSAMLProviderArn =
    lens _dsamlpSAMLProviderArn (\s a -> s { _dsamlpSAMLProviderArn = a })

instance ToQuery DeleteSAMLProvider where
    toQuery = genericQuery def

data DeleteSAMLProviderResponse = DeleteSAMLProviderResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteSAMLProviderResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDeleteSAMLProviderResponse :: DeleteSAMLProviderResponse
mkDeleteSAMLProviderResponse = DeleteSAMLProviderResponse

instance AWSRequest DeleteSAMLProvider where
    type Sv DeleteSAMLProvider = IAM
    type Rs DeleteSAMLProvider = DeleteSAMLProviderResponse

    request = post "DeleteSAMLProvider"
    response _ = nullaryResponse DeleteSAMLProviderResponse
