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
-- Module      : Network.AWS.CertificateManagerPCA.CreatePermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Grants one or more permissions on a private CA to the AWS Certificate Manager (ACM) service principal (@acm.amazonaws.com@ ). These permissions allow ACM to issue and renew ACM certificates that reside in the same AWS account as the CA.
--
--
-- You can list current permissions with the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListPermissions.html ListPermissions> action and revoke them with the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_DeletePermission.html DeletePermission> action.
--
-- __About Permissions__
--
--     * If the private CA and the certificates it issues reside in the same account, you can use @CreatePermission@ to grant permissions for ACM to carry out automatic certificate renewals.
--
--     * For automatic certificate renewal to succeed, the ACM service principal needs permissions to create, retrieve, and list certificates.
--
--     * If the private CA and the ACM certificates reside in different accounts, then permissions cannot be used to enable automatic renewals. Instead, the ACM certificate owner must set up a resource-based policy to enable cross-account issuance and renewals. For more information, see <acm-pca/latest/userguide/pca-rbp.html Using a Resource Based Policy with ACM Private CA> .
module Network.AWS.CertificateManagerPCA.CreatePermission
  ( -- * Creating a Request
    createPermission,
    CreatePermission,

    -- * Request Lenses
    cpSourceAccount,
    cpCertificateAuthorityARN,
    cpPrincipal,
    cpActions,

    -- * Destructuring the Response
    createPermissionResponse,
    CreatePermissionResponse,
  )
where

import Network.AWS.CertificateManagerPCA.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createPermission' smart constructor.
data CreatePermission = CreatePermission'
  { _cpSourceAccount ::
      !(Maybe Text),
    _cpCertificateAuthorityARN :: !Text,
    _cpPrincipal :: !Text,
    _cpActions :: !(List1 ActionType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreatePermission' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpSourceAccount' - The ID of the calling account.
--
-- * 'cpCertificateAuthorityARN' - The Amazon Resource Name (ARN) of the CA that grants the permissions. You can find the ARN by calling the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities> action. This must have the following form:  @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
--
-- * 'cpPrincipal' - The AWS service or identity that receives the permission. At this time, the only valid principal is @acm.amazonaws.com@ .
--
-- * 'cpActions' - The actions that the specified AWS service principal can use. These include @IssueCertificate@ , @GetCertificate@ , and @ListPermissions@ .
createPermission ::
  -- | 'cpCertificateAuthorityARN'
  Text ->
  -- | 'cpPrincipal'
  Text ->
  -- | 'cpActions'
  NonEmpty ActionType ->
  CreatePermission
createPermission pCertificateAuthorityARN_ pPrincipal_ pActions_ =
  CreatePermission'
    { _cpSourceAccount = Nothing,
      _cpCertificateAuthorityARN = pCertificateAuthorityARN_,
      _cpPrincipal = pPrincipal_,
      _cpActions = _List1 # pActions_
    }

-- | The ID of the calling account.
cpSourceAccount :: Lens' CreatePermission (Maybe Text)
cpSourceAccount = lens _cpSourceAccount (\s a -> s {_cpSourceAccount = a})

-- | The Amazon Resource Name (ARN) of the CA that grants the permissions. You can find the ARN by calling the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities> action. This must have the following form:  @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
cpCertificateAuthorityARN :: Lens' CreatePermission Text
cpCertificateAuthorityARN = lens _cpCertificateAuthorityARN (\s a -> s {_cpCertificateAuthorityARN = a})

-- | The AWS service or identity that receives the permission. At this time, the only valid principal is @acm.amazonaws.com@ .
cpPrincipal :: Lens' CreatePermission Text
cpPrincipal = lens _cpPrincipal (\s a -> s {_cpPrincipal = a})

-- | The actions that the specified AWS service principal can use. These include @IssueCertificate@ , @GetCertificate@ , and @ListPermissions@ .
cpActions :: Lens' CreatePermission (NonEmpty ActionType)
cpActions = lens _cpActions (\s a -> s {_cpActions = a}) . _List1

instance AWSRequest CreatePermission where
  type Rs CreatePermission = CreatePermissionResponse
  request = postJSON certificateManagerPCA
  response = receiveNull CreatePermissionResponse'

instance Hashable CreatePermission

instance NFData CreatePermission

instance ToHeaders CreatePermission where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("ACMPrivateCA.CreatePermission" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreatePermission where
  toJSON CreatePermission' {..} =
    object
      ( catMaybes
          [ ("SourceAccount" .=) <$> _cpSourceAccount,
            Just ("CertificateAuthorityArn" .= _cpCertificateAuthorityARN),
            Just ("Principal" .= _cpPrincipal),
            Just ("Actions" .= _cpActions)
          ]
      )

instance ToPath CreatePermission where
  toPath = const "/"

instance ToQuery CreatePermission where
  toQuery = const mempty

-- | /See:/ 'createPermissionResponse' smart constructor.
data CreatePermissionResponse = CreatePermissionResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreatePermissionResponse' with the minimum fields required to make a request.
createPermissionResponse ::
  CreatePermissionResponse
createPermissionResponse = CreatePermissionResponse'

instance NFData CreatePermissionResponse
