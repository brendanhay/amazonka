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
-- Module      : Network.AWS.IAM.UpdateSigningCertificate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the status of the specified user signing certificate from active to disabled, or vice versa. This operation can be used to disable an IAM user's signing certificate as part of a certificate rotation work flow.
--
--
-- If the @UserName@ field is not specified, the user name is determined implicitly based on the AWS access key ID used to sign the request. Because this operation works for access keys under the AWS account, you can use this operation to manage AWS account root user credentials even if the AWS account has no associated users.
--
module Network.AWS.IAM.UpdateSigningCertificate
    (
    -- * Creating a Request
      updateSigningCertificate
    , UpdateSigningCertificate
    -- * Request Lenses
    , uscUserName
    , uscCertificateId
    , uscStatus

    -- * Destructuring the Response
    , updateSigningCertificateResponse
    , UpdateSigningCertificateResponse
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateSigningCertificate' smart constructor.
data UpdateSigningCertificate = UpdateSigningCertificate'
  { _uscUserName      :: !(Maybe Text)
  , _uscCertificateId :: !Text
  , _uscStatus        :: !StatusType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateSigningCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uscUserName' - The name of the IAM user the signing certificate belongs to. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- * 'uscCertificateId' - The ID of the signing certificate you want to update. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
--
-- * 'uscStatus' - The status you want to assign to the certificate. @Active@ means that the certificate can be used for API calls to AWS @Inactive@ means that the certificate cannot be used.
updateSigningCertificate
    :: Text -- ^ 'uscCertificateId'
    -> StatusType -- ^ 'uscStatus'
    -> UpdateSigningCertificate
updateSigningCertificate pCertificateId_ pStatus_ =
  UpdateSigningCertificate'
    { _uscUserName = Nothing
    , _uscCertificateId = pCertificateId_
    , _uscStatus = pStatus_
    }


-- | The name of the IAM user the signing certificate belongs to. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
uscUserName :: Lens' UpdateSigningCertificate (Maybe Text)
uscUserName = lens _uscUserName (\ s a -> s{_uscUserName = a})

-- | The ID of the signing certificate you want to update. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
uscCertificateId :: Lens' UpdateSigningCertificate Text
uscCertificateId = lens _uscCertificateId (\ s a -> s{_uscCertificateId = a})

-- | The status you want to assign to the certificate. @Active@ means that the certificate can be used for API calls to AWS @Inactive@ means that the certificate cannot be used.
uscStatus :: Lens' UpdateSigningCertificate StatusType
uscStatus = lens _uscStatus (\ s a -> s{_uscStatus = a})

instance AWSRequest UpdateSigningCertificate where
        type Rs UpdateSigningCertificate =
             UpdateSigningCertificateResponse
        request = postQuery iam
        response
          = receiveNull UpdateSigningCertificateResponse'

instance Hashable UpdateSigningCertificate where

instance NFData UpdateSigningCertificate where

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
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateSigningCertificateResponse' with the minimum fields required to make a request.
--
updateSigningCertificateResponse
    :: UpdateSigningCertificateResponse
updateSigningCertificateResponse = UpdateSigningCertificateResponse'


instance NFData UpdateSigningCertificateResponse
         where
