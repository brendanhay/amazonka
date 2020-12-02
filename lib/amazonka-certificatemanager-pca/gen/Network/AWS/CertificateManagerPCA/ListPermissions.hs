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
-- Module      : Network.AWS.CertificateManagerPCA.ListPermissions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all permissions on a private CA, if any, granted to the AWS Certificate Manager (ACM) service principal (acm.amazonaws.com).
--
--
-- These permissions allow ACM to issue and renew ACM certificates that reside in the same AWS account as the CA.
--
-- Permissions can be granted with the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreatePermission.html CreatePermission> action and revoked with the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_DeletePermission.html DeletePermission> action.
--
-- __About Permissions__
--
--     * If the private CA and the certificates it issues reside in the same account, you can use @CreatePermission@ to grant permissions for ACM to carry out automatic certificate renewals.
--
--     * For automatic certificate renewal to succeed, the ACM service principal needs permissions to create, retrieve, and list certificates.
--
--     * If the private CA and the ACM certificates reside in different accounts, then permissions cannot be used to enable automatic renewals. Instead, the ACM certificate owner must set up a resource-based policy to enable cross-account issuance and renewals. For more information, see <acm-pca/latest/userguide/pca-rbp.html Using a Resource Based Policy with ACM Private CA> .
--
--
--
--
-- This operation returns paginated results.
module Network.AWS.CertificateManagerPCA.ListPermissions
  ( -- * Creating a Request
    listPermissions,
    ListPermissions,

    -- * Request Lenses
    lpNextToken,
    lpMaxResults,
    lpCertificateAuthorityARN,

    -- * Destructuring the Response
    listPermissionsResponse,
    ListPermissionsResponse,

    -- * Response Lenses
    lprsNextToken,
    lprsPermissions,
    lprsResponseStatus,
  )
where

import Network.AWS.CertificateManagerPCA.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listPermissions' smart constructor.
data ListPermissions = ListPermissions'
  { _lpNextToken ::
      !(Maybe Text),
    _lpMaxResults :: !(Maybe Nat),
    _lpCertificateAuthorityARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListPermissions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpNextToken' - When paginating results, use this parameter in a subsequent request after you receive a response with truncated results. Set it to the value of __NextToken__ from the response you just received.
--
-- * 'lpMaxResults' - When paginating results, use this parameter to specify the maximum number of items to return in the response. If additional items exist beyond the number you specify, the __NextToken__ element is sent in the response. Use this __NextToken__ value in a subsequent request to retrieve additional items.
--
-- * 'lpCertificateAuthorityARN' - The Amazon Resource Number (ARN) of the private CA to inspect. You can find the ARN by calling the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities> action. This must be of the form: @arn:aws:acm-pca:region:account:certificate-authority/12345678-1234-1234-1234-123456789012@ You can get a private CA's ARN by running the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities> action.
listPermissions ::
  -- | 'lpCertificateAuthorityARN'
  Text ->
  ListPermissions
listPermissions pCertificateAuthorityARN_ =
  ListPermissions'
    { _lpNextToken = Nothing,
      _lpMaxResults = Nothing,
      _lpCertificateAuthorityARN = pCertificateAuthorityARN_
    }

-- | When paginating results, use this parameter in a subsequent request after you receive a response with truncated results. Set it to the value of __NextToken__ from the response you just received.
lpNextToken :: Lens' ListPermissions (Maybe Text)
lpNextToken = lens _lpNextToken (\s a -> s {_lpNextToken = a})

-- | When paginating results, use this parameter to specify the maximum number of items to return in the response. If additional items exist beyond the number you specify, the __NextToken__ element is sent in the response. Use this __NextToken__ value in a subsequent request to retrieve additional items.
lpMaxResults :: Lens' ListPermissions (Maybe Natural)
lpMaxResults = lens _lpMaxResults (\s a -> s {_lpMaxResults = a}) . mapping _Nat

-- | The Amazon Resource Number (ARN) of the private CA to inspect. You can find the ARN by calling the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities> action. This must be of the form: @arn:aws:acm-pca:region:account:certificate-authority/12345678-1234-1234-1234-123456789012@ You can get a private CA's ARN by running the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities> action.
lpCertificateAuthorityARN :: Lens' ListPermissions Text
lpCertificateAuthorityARN = lens _lpCertificateAuthorityARN (\s a -> s {_lpCertificateAuthorityARN = a})

instance AWSPager ListPermissions where
  page rq rs
    | stop (rs ^. lprsNextToken) = Nothing
    | stop (rs ^. lprsPermissions) = Nothing
    | otherwise = Just $ rq & lpNextToken .~ rs ^. lprsNextToken

instance AWSRequest ListPermissions where
  type Rs ListPermissions = ListPermissionsResponse
  request = postJSON certificateManagerPCA
  response =
    receiveJSON
      ( \s h x ->
          ListPermissionsResponse'
            <$> (x .?> "NextToken")
            <*> (x .?> "Permissions" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListPermissions

instance NFData ListPermissions

instance ToHeaders ListPermissions where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("ACMPrivateCA.ListPermissions" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListPermissions where
  toJSON ListPermissions' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _lpNextToken,
            ("MaxResults" .=) <$> _lpMaxResults,
            Just ("CertificateAuthorityArn" .= _lpCertificateAuthorityARN)
          ]
      )

instance ToPath ListPermissions where
  toPath = const "/"

instance ToQuery ListPermissions where
  toQuery = const mempty

-- | /See:/ 'listPermissionsResponse' smart constructor.
data ListPermissionsResponse = ListPermissionsResponse'
  { _lprsNextToken ::
      !(Maybe Text),
    _lprsPermissions :: !(Maybe [Permission]),
    _lprsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListPermissionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lprsNextToken' - When the list is truncated, this value is present and should be used for the __NextToken__ parameter in a subsequent pagination request.
--
-- * 'lprsPermissions' - Summary information about each permission assigned by the specified private CA, including the action enabled, the policy provided, and the time of creation.
--
-- * 'lprsResponseStatus' - -- | The response status code.
listPermissionsResponse ::
  -- | 'lprsResponseStatus'
  Int ->
  ListPermissionsResponse
listPermissionsResponse pResponseStatus_ =
  ListPermissionsResponse'
    { _lprsNextToken = Nothing,
      _lprsPermissions = Nothing,
      _lprsResponseStatus = pResponseStatus_
    }

-- | When the list is truncated, this value is present and should be used for the __NextToken__ parameter in a subsequent pagination request.
lprsNextToken :: Lens' ListPermissionsResponse (Maybe Text)
lprsNextToken = lens _lprsNextToken (\s a -> s {_lprsNextToken = a})

-- | Summary information about each permission assigned by the specified private CA, including the action enabled, the policy provided, and the time of creation.
lprsPermissions :: Lens' ListPermissionsResponse [Permission]
lprsPermissions = lens _lprsPermissions (\s a -> s {_lprsPermissions = a}) . _Default . _Coerce

-- | -- | The response status code.
lprsResponseStatus :: Lens' ListPermissionsResponse Int
lprsResponseStatus = lens _lprsResponseStatus (\s a -> s {_lprsResponseStatus = a})

instance NFData ListPermissionsResponse
