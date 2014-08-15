{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

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
module Network.AWS.IAM.V2010_05_08.UpdateSigningCertificate where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'UpdateSigningCertificate' request.
updateSigningCertificate :: Text -- ^ '_uscrCertificateId'
                         -> StatusType -- ^ '_uscrStatus'
                         -> UpdateSigningCertificate
updateSigningCertificate p1 p2 = UpdateSigningCertificate
    { _uscrCertificateId = p1
    , _uscrStatus = p2
    , _uscrUserName = Nothing
    }

data UpdateSigningCertificate = UpdateSigningCertificate
    { _uscrCertificateId :: Text
      -- ^ The ID of the signing certificate you want to update.
    , _uscrStatus :: StatusType
      -- ^ The status you want to assign to the certificate. Active means
      -- the certificate can be used for API calls to AWS, while Inactive
      -- means the certificate cannot be used.
    , _uscrUserName :: Maybe Text
      -- ^ Name of the user the signing certificate belongs to.
    } deriving (Show, Generic)

makeLenses ''UpdateSigningCertificate

instance ToQuery UpdateSigningCertificate where
    toQuery = genericQuery def

data UpdateSigningCertificateResponse = UpdateSigningCertificateResponse
    deriving (Eq, Show, Generic)

makeLenses ''UpdateSigningCertificateResponse

instance AWSRequest UpdateSigningCertificate where
    type Sv UpdateSigningCertificate = IAM
    type Rs UpdateSigningCertificate = UpdateSigningCertificateResponse

    request = post "UpdateSigningCertificate"
    response _ = nullaryResponse UpdateSigningCertificateResponse
