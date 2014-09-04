{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.DeleteSigningCertificate
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified signing certificate associated with the specified
-- user. If you do not specify a user name, IAM determines the user name
-- implicitly based on the AWS access key ID signing the request. Because this
-- action works for access keys under the AWS account, you can use this API to
-- manage root credentials even if the AWS account has no associated users.
-- https://iam.amazonaws.com/ ?Action=DeleteSigningCertificate &UserName=Bob
-- &CertificateId=TA7SMP42TDN5Z26OBPJE7EXAMPLE &Version=2010-05-08 &AUTHPARAMS
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.V2010_05_08.DeleteSigningCertificate
    (
    -- * Request
      DeleteSigningCertificate
    -- ** Request constructor
    , mkDeleteSigningCertificateRequest
    -- ** Request lenses
    , dscsUserName
    , dscsCertificateId

    -- * Response
    , DeleteSigningCertificateResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteSigningCertificate' request.
mkDeleteSigningCertificateRequest :: Text -- ^ 'dscsCertificateId'
                                  -> DeleteSigningCertificate
mkDeleteSigningCertificateRequest p1 = DeleteSigningCertificate
    { _dscsUserName = Nothing
    , _dscsCertificateId = p2
    }
{-# INLINE mkDeleteSigningCertificateRequest #-}

data DeleteSigningCertificate = DeleteSigningCertificate
    { _dscsUserName :: Maybe Text
      -- ^ Name of the user the signing certificate belongs to.
    , _dscsCertificateId :: Text
      -- ^ ID of the signing certificate to delete.
    } deriving (Show, Generic)

-- | Name of the user the signing certificate belongs to.
dscsUserName :: Lens' DeleteSigningCertificate (Maybe Text)
dscsUserName = lens _dscsUserName (\s a -> s { _dscsUserName = a })
{-# INLINE dscsUserName #-}

-- | ID of the signing certificate to delete.
dscsCertificateId :: Lens' DeleteSigningCertificate (Text)
dscsCertificateId = lens _dscsCertificateId (\s a -> s { _dscsCertificateId = a })
{-# INLINE dscsCertificateId #-}

instance ToQuery DeleteSigningCertificate where
    toQuery = genericQuery def

data DeleteSigningCertificateResponse = DeleteSigningCertificateResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteSigningCertificate where
    type Sv DeleteSigningCertificate = IAM
    type Rs DeleteSigningCertificate = DeleteSigningCertificateResponse

    request = post "DeleteSigningCertificate"
    response _ = nullaryResponse DeleteSigningCertificateResponse
