{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.DeleteSigningCertificate
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deletes the specified signing certificate associated with the specified user.
--
-- If you do not specify a user name, IAM determines the user name implicitly
-- based on the AWS access key ID signing the request. Because this action works
-- for access keys under the AWS account, you can use this action to manage root
-- credentials even if the AWS account has no associated users.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteSigningCertificate.html>
module Network.AWS.IAM.DeleteSigningCertificate
    (
    -- * Request
      DeleteSigningCertificate
    -- ** Request constructor
    , deleteSigningCertificate
    -- ** Request lenses
    , dscCertificateId
    , dscUserName

    -- * Response
    , DeleteSigningCertificateResponse
    -- ** Response constructor
    , deleteSigningCertificateResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data DeleteSigningCertificate = DeleteSigningCertificate
    { _dscCertificateId :: Text
    , _dscUserName      :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DeleteSigningCertificate' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dscCertificateId' @::@ 'Text'
--
-- * 'dscUserName' @::@ 'Maybe' 'Text'
--
deleteSigningCertificate :: Text -- ^ 'dscCertificateId'
                         -> DeleteSigningCertificate
deleteSigningCertificate p1 = DeleteSigningCertificate
    { _dscCertificateId = p1
    , _dscUserName      = Nothing
    }

-- | The ID of the signing certificate to delete.
dscCertificateId :: Lens' DeleteSigningCertificate Text
dscCertificateId = lens _dscCertificateId (\s a -> s { _dscCertificateId = a })

-- | The name of the user the signing certificate belongs to.
dscUserName :: Lens' DeleteSigningCertificate (Maybe Text)
dscUserName = lens _dscUserName (\s a -> s { _dscUserName = a })

data DeleteSigningCertificateResponse = DeleteSigningCertificateResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'DeleteSigningCertificateResponse' constructor.
deleteSigningCertificateResponse :: DeleteSigningCertificateResponse
deleteSigningCertificateResponse = DeleteSigningCertificateResponse

instance ToPath DeleteSigningCertificate where
    toPath = const "/"

instance ToQuery DeleteSigningCertificate where
    toQuery DeleteSigningCertificate{..} = mconcat
        [ "CertificateId" =? _dscCertificateId
        , "UserName"      =? _dscUserName
        ]

instance ToHeaders DeleteSigningCertificate

instance AWSRequest DeleteSigningCertificate where
    type Sv DeleteSigningCertificate = IAM
    type Rs DeleteSigningCertificate = DeleteSigningCertificateResponse

    request  = post "DeleteSigningCertificate"
    response = nullResponse DeleteSigningCertificateResponse
