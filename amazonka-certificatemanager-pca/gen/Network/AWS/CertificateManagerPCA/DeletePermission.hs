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
-- Module      : Network.AWS.CertificateManagerPCA.DeletePermission
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Revokes permissions that a private CA assigned to a designated AWS service. Permissions can be created with the 'CreatePermission' operation and listed with the 'ListPermissions' operation.
--
--
module Network.AWS.CertificateManagerPCA.DeletePermission
    (
    -- * Creating a Request
      deletePermission
    , DeletePermission
    -- * Request Lenses
    , dpSourceAccount
    , dpCertificateAuthorityARN
    , dpPrincipal

    -- * Destructuring the Response
    , deletePermissionResponse
    , DeletePermissionResponse
    ) where

import Network.AWS.CertificateManagerPCA.Types
import Network.AWS.CertificateManagerPCA.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deletePermission' smart constructor.
data DeletePermission = DeletePermission'
  { _dpSourceAccount           :: !(Maybe Text)
  , _dpCertificateAuthorityARN :: !Text
  , _dpPrincipal               :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeletePermission' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpSourceAccount' - The AWS account that calls this operation.
--
-- * 'dpCertificateAuthorityARN' - The Amazon Resource Number (ARN) of the private CA that issued the permissions. You can find the CA's ARN by calling the 'ListCertificateAuthorities' operation. This must have the following form:  @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
--
-- * 'dpPrincipal' - The AWS service or identity that will have its CA permissions revoked. At this time, the only valid service principal is @acm.amazonaws.com@
deletePermission
    :: Text -- ^ 'dpCertificateAuthorityARN'
    -> Text -- ^ 'dpPrincipal'
    -> DeletePermission
deletePermission pCertificateAuthorityARN_ pPrincipal_ =
  DeletePermission'
    { _dpSourceAccount = Nothing
    , _dpCertificateAuthorityARN = pCertificateAuthorityARN_
    , _dpPrincipal = pPrincipal_
    }


-- | The AWS account that calls this operation.
dpSourceAccount :: Lens' DeletePermission (Maybe Text)
dpSourceAccount = lens _dpSourceAccount (\ s a -> s{_dpSourceAccount = a})

-- | The Amazon Resource Number (ARN) of the private CA that issued the permissions. You can find the CA's ARN by calling the 'ListCertificateAuthorities' operation. This must have the following form:  @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
dpCertificateAuthorityARN :: Lens' DeletePermission Text
dpCertificateAuthorityARN = lens _dpCertificateAuthorityARN (\ s a -> s{_dpCertificateAuthorityARN = a})

-- | The AWS service or identity that will have its CA permissions revoked. At this time, the only valid service principal is @acm.amazonaws.com@
dpPrincipal :: Lens' DeletePermission Text
dpPrincipal = lens _dpPrincipal (\ s a -> s{_dpPrincipal = a})

instance AWSRequest DeletePermission where
        type Rs DeletePermission = DeletePermissionResponse
        request = postJSON certificateManagerPCA
        response = receiveNull DeletePermissionResponse'

instance Hashable DeletePermission where

instance NFData DeletePermission where

instance ToHeaders DeletePermission where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("ACMPrivateCA.DeletePermission" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeletePermission where
        toJSON DeletePermission'{..}
          = object
              (catMaybes
                 [("SourceAccount" .=) <$> _dpSourceAccount,
                  Just
                    ("CertificateAuthorityArn" .=
                       _dpCertificateAuthorityARN),
                  Just ("Principal" .= _dpPrincipal)])

instance ToPath DeletePermission where
        toPath = const "/"

instance ToQuery DeletePermission where
        toQuery = const mempty

-- | /See:/ 'deletePermissionResponse' smart constructor.
data DeletePermissionResponse =
  DeletePermissionResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeletePermissionResponse' with the minimum fields required to make a request.
--
deletePermissionResponse
    :: DeletePermissionResponse
deletePermissionResponse = DeletePermissionResponse'


instance NFData DeletePermissionResponse where
