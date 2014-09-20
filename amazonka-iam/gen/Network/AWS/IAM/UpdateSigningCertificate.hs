{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.UpdateSigningCertificate
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
module Network.AWS.IAM.UpdateSigningCertificate
    (
    -- * Request
      UpdateSigningCertificate
    -- ** Request constructor
    , updateSigningCertificate
    -- ** Request lenses
    , usc1UserName
    , usc1CertificateId
    , usc1Status

    -- * Response
    , UpdateSigningCertificateResponse
    -- ** Response constructor
    , updateSigningCertificateResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import Network.AWS.Prelude

data UpdateSigningCertificate = UpdateSigningCertificate
    { _usc1UserName :: Maybe Text
    , _usc1CertificateId :: Text
    , _usc1Status :: StatusType
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateSigningCertificate' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @UserName ::@ @Maybe Text@
--
-- * @CertificateId ::@ @Text@
--
-- * @Status ::@ @StatusType@
--
updateSigningCertificate :: Text -- ^ 'usc1CertificateId'
                         -> StatusType -- ^ 'usc1Status'
                         -> UpdateSigningCertificate
updateSigningCertificate p2 p3 = UpdateSigningCertificate
    { _usc1UserName = Nothing
    , _usc1CertificateId = p2
    , _usc1Status = p3
    }

-- | Name of the user the signing certificate belongs to.
usc1UserName :: Lens' UpdateSigningCertificate (Maybe Text)
usc1UserName = lens _usc1UserName (\s a -> s { _usc1UserName = a })

-- | The ID of the signing certificate you want to update.
usc1CertificateId :: Lens' UpdateSigningCertificate Text
usc1CertificateId =
    lens _usc1CertificateId (\s a -> s { _usc1CertificateId = a })

-- | The status you want to assign to the certificate. Active means the
-- certificate can be used for API calls to AWS, while Inactive means the
-- certificate cannot be used.
usc1Status :: Lens' UpdateSigningCertificate StatusType
usc1Status = lens _usc1Status (\s a -> s { _usc1Status = a })

instance ToQuery UpdateSigningCertificate where
    toQuery = genericQuery def

data UpdateSigningCertificateResponse = UpdateSigningCertificateResponse
    deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateSigningCertificateResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
updateSigningCertificateResponse :: UpdateSigningCertificateResponse
updateSigningCertificateResponse = UpdateSigningCertificateResponse

instance AWSRequest UpdateSigningCertificate where
    type Sv UpdateSigningCertificate = IAM
    type Rs UpdateSigningCertificate = UpdateSigningCertificateResponse

    request = post "UpdateSigningCertificate"
    response _ = nullaryResponse UpdateSigningCertificateResponse
