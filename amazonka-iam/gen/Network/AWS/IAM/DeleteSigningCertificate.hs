{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.DeleteSigningCertificate
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
module Network.AWS.IAM.DeleteSigningCertificate
    (
    -- * Request
      DeleteSigningCertificate
    -- ** Request constructor
    , deleteSigningCertificate
    -- ** Request lenses
    , dsc1UserName
    , dsc1CertificateId

    -- * Response
    , DeleteSigningCertificateResponse
    -- ** Response constructor
    , deleteSigningCertificateResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import Network.AWS.Prelude

data DeleteSigningCertificate = DeleteSigningCertificate
    { _dsc1UserName :: Maybe Text
    , _dsc1CertificateId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteSigningCertificate' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @UserName ::@ @Maybe Text@
--
-- * @CertificateId ::@ @Text@
--
deleteSigningCertificate :: Text -- ^ 'dsc1CertificateId'
                         -> DeleteSigningCertificate
deleteSigningCertificate p2 = DeleteSigningCertificate
    { _dsc1UserName = Nothing
    , _dsc1CertificateId = p2
    }

-- | Name of the user the signing certificate belongs to.
dsc1UserName :: Lens' DeleteSigningCertificate (Maybe Text)
dsc1UserName = lens _dsc1UserName (\s a -> s { _dsc1UserName = a })

-- | ID of the signing certificate to delete.
dsc1CertificateId :: Lens' DeleteSigningCertificate Text
dsc1CertificateId =
    lens _dsc1CertificateId (\s a -> s { _dsc1CertificateId = a })

instance ToQuery DeleteSigningCertificate where
    toQuery = genericQuery def

data DeleteSigningCertificateResponse = DeleteSigningCertificateResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteSigningCertificateResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
deleteSigningCertificateResponse :: DeleteSigningCertificateResponse
deleteSigningCertificateResponse = DeleteSigningCertificateResponse

instance AWSRequest DeleteSigningCertificate where
    type Sv DeleteSigningCertificate = IAM
    type Rs DeleteSigningCertificate = DeleteSigningCertificateResponse

    request = post "DeleteSigningCertificate"
    response _ = nullaryResponse DeleteSigningCertificateResponse
