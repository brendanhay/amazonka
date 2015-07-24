{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteSigningCertificate
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified signing certificate associated with the specified
-- user.
--
-- If you do not specify a user name, IAM determines the user name
-- implicitly based on the AWS access key ID signing the request. Because
-- this action works for access keys under the AWS account, you can use
-- this action to manage root credentials even if the AWS account has no
-- associated users.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteSigningCertificate.html>
module Network.AWS.IAM.DeleteSigningCertificate
    (
    -- * Request
      DeleteSigningCertificate
    -- ** Request constructor
    , deleteSigningCertificate
    -- ** Request lenses
    , dscUserName
    , dscCertificateId

    -- * Response
    , DeleteSigningCertificateResponse
    -- ** Response constructor
    , deleteSigningCertificateResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteSigningCertificate' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dscUserName'
--
-- * 'dscCertificateId'
data DeleteSigningCertificate = DeleteSigningCertificate'
    { _dscUserName      :: !(Maybe Text)
    , _dscCertificateId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteSigningCertificate' smart constructor.
deleteSigningCertificate :: Text -> DeleteSigningCertificate
deleteSigningCertificate pCertificateId_ =
    DeleteSigningCertificate'
    { _dscUserName = Nothing
    , _dscCertificateId = pCertificateId_
    }

-- | The name of the user the signing certificate belongs to.
dscUserName :: Lens' DeleteSigningCertificate (Maybe Text)
dscUserName = lens _dscUserName (\ s a -> s{_dscUserName = a});

-- | The ID of the signing certificate to delete.
dscCertificateId :: Lens' DeleteSigningCertificate Text
dscCertificateId = lens _dscCertificateId (\ s a -> s{_dscCertificateId = a});

instance AWSRequest DeleteSigningCertificate where
        type Sv DeleteSigningCertificate = IAM
        type Rs DeleteSigningCertificate =
             DeleteSigningCertificateResponse
        request = post
        response
          = receiveNull DeleteSigningCertificateResponse'

instance ToHeaders DeleteSigningCertificate where
        toHeaders = const mempty

instance ToPath DeleteSigningCertificate where
        toPath = const "/"

instance ToQuery DeleteSigningCertificate where
        toQuery DeleteSigningCertificate'{..}
          = mconcat
              ["Action" =:
                 ("DeleteSigningCertificate" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "UserName" =: _dscUserName,
               "CertificateId" =: _dscCertificateId]

-- | /See:/ 'deleteSigningCertificateResponse' smart constructor.
data DeleteSigningCertificateResponse =
    DeleteSigningCertificateResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteSigningCertificateResponse' smart constructor.
deleteSigningCertificateResponse :: DeleteSigningCertificateResponse
deleteSigningCertificateResponse = DeleteSigningCertificateResponse'
