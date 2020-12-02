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
-- Module      : Network.AWS.CertificateManagerPCA.DeletePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the resource-based policy attached to a private CA. Deletion will remove any access that the policy has granted. If there is no policy attached to the private CA, this action will return successful.
--
--
-- If you delete a policy that was applied through AWS Resource Access Manager (RAM), the CA will be removed from all shares in which it was included.
--
-- The AWS Certificate Manager Service Linked Role that the policy supports is not affected when you delete the policy.
--
-- The current policy can be shown with <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_GetPolicy.html GetPolicy> and updated with <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_PutPolicy.html PutPolicy> .
--
-- __About Policies__
--
--     * A policy grants access on a private CA to an AWS customer account, to AWS Organizations, or to an AWS Organizations unit. Policies are under the control of a CA administrator. For more information, see <acm-pca/latest/userguide/pca-rbp.html Using a Resource Based Policy with ACM Private CA> .
--
--     * A policy permits a user of AWS Certificate Manager (ACM) to issue ACM certificates signed by a CA in another account.
--
--     * For ACM to manage automatic renewal of these certificates, the ACM user must configure a Service Linked Role (SLR). The SLR allows the ACM service to assume the identity of the user, subject to confirmation against the ACM Private CA policy. For more information, see <https://docs.aws.amazon.com/acm/latest/userguide/acm-slr.html Using a Service Linked Role with ACM> .
--
--     * Updates made in AWS Resource Manager (RAM) are reflected in policies. For more information, see <acm-pca/latest/userguide/pca-ram.html Using AWS Resource Access Manager (RAM) with ACM Private CA> .
module Network.AWS.CertificateManagerPCA.DeletePolicy
  ( -- * Creating a Request
    deletePolicy,
    DeletePolicy,

    -- * Request Lenses
    dpResourceARN,

    -- * Destructuring the Response
    deletePolicyResponse,
    DeletePolicyResponse,
  )
where

import Network.AWS.CertificateManagerPCA.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deletePolicy' smart constructor.
newtype DeletePolicy = DeletePolicy' {_dpResourceARN :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeletePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpResourceARN' - The Amazon Resource Number (ARN) of the private CA that will have its policy deleted. You can find the CA's ARN by calling the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities> action. The ARN value must have the form @arn:aws:acm-pca:region:account:certificate-authority/01234567-89ab-cdef-0123-0123456789ab@ .
deletePolicy ::
  -- | 'dpResourceARN'
  Text ->
  DeletePolicy
deletePolicy pResourceARN_ =
  DeletePolicy' {_dpResourceARN = pResourceARN_}

-- | The Amazon Resource Number (ARN) of the private CA that will have its policy deleted. You can find the CA's ARN by calling the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities> action. The ARN value must have the form @arn:aws:acm-pca:region:account:certificate-authority/01234567-89ab-cdef-0123-0123456789ab@ .
dpResourceARN :: Lens' DeletePolicy Text
dpResourceARN = lens _dpResourceARN (\s a -> s {_dpResourceARN = a})

instance AWSRequest DeletePolicy where
  type Rs DeletePolicy = DeletePolicyResponse
  request = postJSON certificateManagerPCA
  response = receiveNull DeletePolicyResponse'

instance Hashable DeletePolicy

instance NFData DeletePolicy

instance ToHeaders DeletePolicy where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("ACMPrivateCA.DeletePolicy" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeletePolicy where
  toJSON DeletePolicy' {..} =
    object (catMaybes [Just ("ResourceArn" .= _dpResourceARN)])

instance ToPath DeletePolicy where
  toPath = const "/"

instance ToQuery DeletePolicy where
  toQuery = const mempty

-- | /See:/ 'deletePolicyResponse' smart constructor.
data DeletePolicyResponse = DeletePolicyResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeletePolicyResponse' with the minimum fields required to make a request.
deletePolicyResponse ::
  DeletePolicyResponse
deletePolicyResponse = DeletePolicyResponse'

instance NFData DeletePolicyResponse
