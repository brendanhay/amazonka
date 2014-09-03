{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.UpdateSigningCertificate
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Changes the status of the specified signing certificate from active to
-- disabled, or vice versa. This action can be used to disable a user's
-- signing certificate as part of a certificate rotation work flow. If the
-- UserName field is not specified, the UserName is determined implicitly
-- based on the AWS access key ID used to sign the request. Because this
-- action works for access keys under the AWS account, this API can be used to
-- manage root credentials even if the AWS account has no associated users.
-- For information about rotating certificates, see Managing Keys and
-- Certificates in the Using IAM guide. https://iam.amazonaws.com/
-- ?Action=UpdateSigningCertificate &UserName=Bob
-- &CertificateId=TA7SMP42TDN5Z26OBPJE7EXAMPLE &Status=Inactive
-- &Version=2010-05-08 &AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.V2010_05_08.UpdateSigningCertificate
    (
    -- * Request
      UpdateSigningCertificate
    -- ** Request constructor
    , updateSigningCertificate
    -- ** Request lenses
    , uscsCertificateId
    , uscsStatus
    , uscsUserName

    -- * Response
    , UpdateSigningCertificateResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'UpdateSigningCertificate' request.
updateSigningCertificate :: Text -- ^ 'uscsCertificateId'
                         -> StatusType -- ^ 'uscsStatus'
                         -> UpdateSigningCertificate
updateSigningCertificate p1 p2 = UpdateSigningCertificate
    { _uscsCertificateId = p1
    , _uscsStatus = p2
    , _uscsUserName = Nothing
    }

data UpdateSigningCertificate = UpdateSigningCertificate
    { _uscsCertificateId :: Text
      -- ^ The ID of the signing certificate you want to update.
    , _uscsStatus :: StatusType
      -- ^ The status you want to assign to the certificate. Active means
      -- the certificate can be used for API calls to AWS, while Inactive
      -- means the certificate cannot be used.
    , _uscsUserName :: Maybe Text
      -- ^ Name of the user the signing certificate belongs to.
    } deriving (Show, Generic)

-- | The ID of the signing certificate you want to update.
uscsCertificateId
    :: Functor f
    => (Text
    -> f (Text))
    -> UpdateSigningCertificate
    -> f UpdateSigningCertificate
uscsCertificateId f x =
    (\y -> x { _uscsCertificateId = y })
       <$> f (_uscsCertificateId x)
{-# INLINE uscsCertificateId #-}

-- | The status you want to assign to the certificate. Active means the
-- certificate can be used for API calls to AWS, while Inactive means the
-- certificate cannot be used.
uscsStatus
    :: Functor f
    => (StatusType
    -> f (StatusType))
    -> UpdateSigningCertificate
    -> f UpdateSigningCertificate
uscsStatus f x =
    (\y -> x { _uscsStatus = y })
       <$> f (_uscsStatus x)
{-# INLINE uscsStatus #-}

-- | Name of the user the signing certificate belongs to.
uscsUserName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UpdateSigningCertificate
    -> f UpdateSigningCertificate
uscsUserName f x =
    (\y -> x { _uscsUserName = y })
       <$> f (_uscsUserName x)
{-# INLINE uscsUserName #-}

instance ToQuery UpdateSigningCertificate where
    toQuery = genericQuery def

data UpdateSigningCertificateResponse = UpdateSigningCertificateResponse
    deriving (Eq, Show, Generic)

instance AWSRequest UpdateSigningCertificate where
    type Sv UpdateSigningCertificate = IAM
    type Rs UpdateSigningCertificate = UpdateSigningCertificateResponse

    request = post "UpdateSigningCertificate"
    response _ = nullaryResponse UpdateSigningCertificateResponse
