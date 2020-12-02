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
-- Module      : Network.AWS.CertificateManagerPCA.GetPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the resource-based policy attached to a private CA. If either the private CA resource or the policy cannot be found, this action returns a @ResourceNotFoundException@ .
--
--
-- The policy can be attached or updated with <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_PutPolicy.html PutPolicy> and removed with <acm-pca/latest/APIReference/API_DeletePolicy.html DeletePolicy> .
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
module Network.AWS.CertificateManagerPCA.GetPolicy
  ( -- * Creating a Request
    getPolicy,
    GetPolicy,

    -- * Request Lenses
    gpResourceARN,

    -- * Destructuring the Response
    getPolicyResponse,
    GetPolicyResponse,

    -- * Response Lenses
    gprsPolicy,
    gprsResponseStatus,
  )
where

import Network.AWS.CertificateManagerPCA.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getPolicy' smart constructor.
newtype GetPolicy = GetPolicy' {_gpResourceARN :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpResourceARN' - The Amazon Resource Number (ARN) of the private CA that will have its policy retrieved. You can find the CA's ARN by calling the ListCertificateAuthorities action.
getPolicy ::
  -- | 'gpResourceARN'
  Text ->
  GetPolicy
getPolicy pResourceARN_ =
  GetPolicy' {_gpResourceARN = pResourceARN_}

-- | The Amazon Resource Number (ARN) of the private CA that will have its policy retrieved. You can find the CA's ARN by calling the ListCertificateAuthorities action.
gpResourceARN :: Lens' GetPolicy Text
gpResourceARN = lens _gpResourceARN (\s a -> s {_gpResourceARN = a})

instance AWSRequest GetPolicy where
  type Rs GetPolicy = GetPolicyResponse
  request = postJSON certificateManagerPCA
  response =
    receiveJSON
      ( \s h x ->
          GetPolicyResponse' <$> (x .?> "Policy") <*> (pure (fromEnum s))
      )

instance Hashable GetPolicy

instance NFData GetPolicy

instance ToHeaders GetPolicy where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("ACMPrivateCA.GetPolicy" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetPolicy where
  toJSON GetPolicy' {..} =
    object (catMaybes [Just ("ResourceArn" .= _gpResourceARN)])

instance ToPath GetPolicy where
  toPath = const "/"

instance ToQuery GetPolicy where
  toQuery = const mempty

-- | /See:/ 'getPolicyResponse' smart constructor.
data GetPolicyResponse = GetPolicyResponse'
  { _gprsPolicy ::
      !(Maybe Text),
    _gprsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gprsPolicy' - The policy attached to the private CA as a JSON document.
--
-- * 'gprsResponseStatus' - -- | The response status code.
getPolicyResponse ::
  -- | 'gprsResponseStatus'
  Int ->
  GetPolicyResponse
getPolicyResponse pResponseStatus_ =
  GetPolicyResponse'
    { _gprsPolicy = Nothing,
      _gprsResponseStatus = pResponseStatus_
    }

-- | The policy attached to the private CA as a JSON document.
gprsPolicy :: Lens' GetPolicyResponse (Maybe Text)
gprsPolicy = lens _gprsPolicy (\s a -> s {_gprsPolicy = a})

-- | -- | The response status code.
gprsResponseStatus :: Lens' GetPolicyResponse Int
gprsResponseStatus = lens _gprsResponseStatus (\s a -> s {_gprsResponseStatus = a})

instance NFData GetPolicyResponse
