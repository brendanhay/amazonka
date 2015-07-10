{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.UpdateSigningCertificate
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Changes the status of the specified signing certificate from active to
-- disabled, or vice versa. This action can be used to disable a user\'s
-- signing certificate as part of a certificate rotation work flow.
--
-- If the @UserName@ field is not specified, the UserName is determined
-- implicitly based on the AWS access key ID used to sign the request.
-- Because this action works for access keys under the AWS account, you can
-- use this action to manage root credentials even if the AWS account has
-- no associated users.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_UpdateSigningCertificate.html>
module Network.AWS.IAM.UpdateSigningCertificate
    (
    -- * Request
      UpdateSigningCertificate
    -- ** Request constructor
    , updateSigningCertificate
    -- ** Request lenses
    , uscUserName
    , uscCertificateId
    , uscStatus

    -- * Response
    , UpdateSigningCertificateResponse
    -- ** Response constructor
    , updateSigningCertificateResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'updateSigningCertificate' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uscUserName'
--
-- * 'uscCertificateId'
--
-- * 'uscStatus'
data UpdateSigningCertificate = UpdateSigningCertificate'
    { _uscUserName      :: !(Maybe Text)
    , _uscCertificateId :: !Text
    , _uscStatus        :: !StatusType
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateSigningCertificate' smart constructor.
updateSigningCertificate :: Text -> StatusType -> UpdateSigningCertificate
updateSigningCertificate pCertificateId pStatus =
    UpdateSigningCertificate'
    { _uscUserName = Nothing
    , _uscCertificateId = pCertificateId
    , _uscStatus = pStatus
    }

-- | The name of the user the signing certificate belongs to.
uscUserName :: Lens' UpdateSigningCertificate (Maybe Text)
uscUserName = lens _uscUserName (\ s a -> s{_uscUserName = a});

-- | The ID of the signing certificate you want to update.
uscCertificateId :: Lens' UpdateSigningCertificate Text
uscCertificateId = lens _uscCertificateId (\ s a -> s{_uscCertificateId = a});

-- | The status you want to assign to the certificate. @Active@ means the
-- certificate can be used for API calls to AWS, while @Inactive@ means the
-- certificate cannot be used.
uscStatus :: Lens' UpdateSigningCertificate StatusType
uscStatus = lens _uscStatus (\ s a -> s{_uscStatus = a});

instance AWSRequest UpdateSigningCertificate where
        type Sv UpdateSigningCertificate = IAM
        type Rs UpdateSigningCertificate =
             UpdateSigningCertificateResponse
        request = post
        response
          = receiveNull UpdateSigningCertificateResponse'

instance ToHeaders UpdateSigningCertificate where
        toHeaders = const mempty

instance ToPath UpdateSigningCertificate where
        toPath = const "/"

instance ToQuery UpdateSigningCertificate where
        toQuery UpdateSigningCertificate'{..}
          = mconcat
              ["Action" =:
                 ("UpdateSigningCertificate" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "UserName" =: _uscUserName,
               "CertificateId" =: _uscCertificateId,
               "Status" =: _uscStatus]

-- | /See:/ 'updateSigningCertificateResponse' smart constructor.
data UpdateSigningCertificateResponse =
    UpdateSigningCertificateResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateSigningCertificateResponse' smart constructor.
updateSigningCertificateResponse :: UpdateSigningCertificateResponse
updateSigningCertificateResponse = UpdateSigningCertificateResponse'
