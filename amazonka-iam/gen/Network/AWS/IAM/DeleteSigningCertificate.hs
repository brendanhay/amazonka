{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteSigningCertificate
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified signing certificate associated with the specified user.
--
-- If you do not specify a user name, IAM determines the user name implicitly based on the AWS access key ID signing the request. Because this action works for access keys under the AWS account, you can use this action to manage root credentials even if the AWS account has no associated users.
module Network.AWS.IAM.DeleteSigningCertificate
    (
    -- * Creating a Request
      deleteSigningCertificate
    , DeleteSigningCertificate
    -- * Request Lenses
    , dscUserName
    , dscCertificateId

    -- * Destructuring the Response
    , deleteSigningCertificateResponse
    , DeleteSigningCertificateResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteSigningCertificate' smart constructor.
data DeleteSigningCertificate = DeleteSigningCertificate'
    { _dscUserName      :: !(Maybe Text)
    , _dscCertificateId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteSigningCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dscUserName'
--
-- * 'dscCertificateId'
deleteSigningCertificate
    :: Text -- ^ 'dscCertificateId'
    -> DeleteSigningCertificate
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
        type Rs DeleteSigningCertificate =
             DeleteSigningCertificateResponse
        request = postQuery iam
        response
          = receiveNull DeleteSigningCertificateResponse'

instance Hashable DeleteSigningCertificate

instance NFData DeleteSigningCertificate

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

-- | Creates a value of 'DeleteSigningCertificateResponse' with the minimum fields required to make a request.
--
deleteSigningCertificateResponse
    :: DeleteSigningCertificateResponse
deleteSigningCertificateResponse = DeleteSigningCertificateResponse'

instance NFData DeleteSigningCertificateResponse
