{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.DeletePermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Revokes permissions on a private CA granted to the AWS Certificate Manager (ACM) service principal (acm.amazonaws.com).
--
--
-- These permissions allow ACM to issue and renew ACM certificates that reside in the same AWS account as the CA. If you revoke these permissions, ACM will no longer renew the affected certificates automatically.
--
-- Permissions can be granted with the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreatePermission.html CreatePermission> action and listed with the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListPermissions.html ListPermissions> action.
--
-- __About Permissions__
--
--     * If the private CA and the certificates it issues reside in the same account, you can use @CreatePermission@ to grant permissions for ACM to carry out automatic certificate renewals.
--
--     * For automatic certificate renewal to succeed, the ACM service principal needs permissions to create, retrieve, and list certificates.
--
--     * If the private CA and the ACM certificates reside in different accounts, then permissions cannot be used to enable automatic renewals. Instead, the ACM certificate owner must set up a resource-based policy to enable cross-account issuance and renewals. For more information, see <acm-pca/latest/userguide/pca-rbp.html Using a Resource Based Policy with ACM Private CA> .
module Network.AWS.CertificateManagerPCA.DeletePermission
  ( -- * Creating a Request
    deletePermission,
    DeletePermission,

    -- * Request Lenses
    dpSourceAccount,
    dpCertificateAuthorityARN,
    dpPrincipal,

    -- * Destructuring the Response
    deletePermissionResponse,
    DeletePermissionResponse,
  )
where

import Network.AWS.CertificateManagerPCA.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deletePermission' smart constructor.
data DeletePermission = DeletePermission'
  { _dpSourceAccount ::
      !(Maybe Text),
    _dpCertificateAuthorityARN :: !Text,
    _dpPrincipal :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeletePermission' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpSourceAccount' - The AWS account that calls this action.
--
-- * 'dpCertificateAuthorityARN' - The Amazon Resource Number (ARN) of the private CA that issued the permissions. You can find the CA's ARN by calling the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities> action. This must have the following form:  @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
--
-- * 'dpPrincipal' - The AWS service or identity that will have its CA permissions revoked. At this time, the only valid service principal is @acm.amazonaws.com@
deletePermission ::
  -- | 'dpCertificateAuthorityARN'
  Text ->
  -- | 'dpPrincipal'
  Text ->
  DeletePermission
deletePermission pCertificateAuthorityARN_ pPrincipal_ =
  DeletePermission'
    { _dpSourceAccount = Nothing,
      _dpCertificateAuthorityARN = pCertificateAuthorityARN_,
      _dpPrincipal = pPrincipal_
    }

-- | The AWS account that calls this action.
dpSourceAccount :: Lens' DeletePermission (Maybe Text)
dpSourceAccount = lens _dpSourceAccount (\s a -> s {_dpSourceAccount = a})

-- | The Amazon Resource Number (ARN) of the private CA that issued the permissions. You can find the CA's ARN by calling the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities> action. This must have the following form:  @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
dpCertificateAuthorityARN :: Lens' DeletePermission Text
dpCertificateAuthorityARN = lens _dpCertificateAuthorityARN (\s a -> s {_dpCertificateAuthorityARN = a})

-- | The AWS service or identity that will have its CA permissions revoked. At this time, the only valid service principal is @acm.amazonaws.com@
dpPrincipal :: Lens' DeletePermission Text
dpPrincipal = lens _dpPrincipal (\s a -> s {_dpPrincipal = a})

instance AWSRequest DeletePermission where
  type Rs DeletePermission = DeletePermissionResponse
  request = postJSON certificateManagerPCA
  response = receiveNull DeletePermissionResponse'

instance Hashable DeletePermission

instance NFData DeletePermission

instance ToHeaders DeletePermission where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("ACMPrivateCA.DeletePermission" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeletePermission where
  toJSON DeletePermission' {..} =
    object
      ( catMaybes
          [ ("SourceAccount" .=) <$> _dpSourceAccount,
            Just ("CertificateAuthorityArn" .= _dpCertificateAuthorityARN),
            Just ("Principal" .= _dpPrincipal)
          ]
      )

instance ToPath DeletePermission where
  toPath = const "/"

instance ToQuery DeletePermission where
  toQuery = const mempty

-- | /See:/ 'deletePermissionResponse' smart constructor.
data DeletePermissionResponse = DeletePermissionResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeletePermissionResponse' with the minimum fields required to make a request.
deletePermissionResponse ::
  DeletePermissionResponse
deletePermissionResponse = DeletePermissionResponse'

instance NFData DeletePermissionResponse
