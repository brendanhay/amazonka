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
-- Module      : Network.AWS.CertificateManagerPCA.PutPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches a resource-based policy to a private CA.
--
--
-- A policy can also be applied by <https://docs.aws.amazon.com/acm-pca/latest/userguide/pca-ram.html sharing> a private CA through AWS Resource Access Manager (RAM).
--
-- The policy can be displayed with <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_GetPolicy.html GetPolicy> and removed with <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_DeletePolicy.html DeletePolicy> .
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
module Network.AWS.CertificateManagerPCA.PutPolicy
  ( -- * Creating a Request
    putPolicy,
    PutPolicy,

    -- * Request Lenses
    ppResourceARN,
    ppPolicy,

    -- * Destructuring the Response
    putPolicyResponse,
    PutPolicyResponse,
  )
where

import Network.AWS.CertificateManagerPCA.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putPolicy' smart constructor.
data PutPolicy = PutPolicy'
  { _ppResourceARN :: !Text,
    _ppPolicy :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ppResourceARN' - The Amazon Resource Number (ARN) of the private CA to associate with the policy. The ARN of the CA can be found by calling the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities> action.
--
-- * 'ppPolicy' - The path and filename of a JSON-formatted IAM policy to attach to the specified private CA resource. If this policy does not contain all required statements or if it includes any statement that is not allowed, the @PutPolicy@ action returns an @InvalidPolicyException@ . For information about IAM policy and statement structure, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#access_policies-json Overview of JSON Policies> .
putPolicy ::
  -- | 'ppResourceARN'
  Text ->
  -- | 'ppPolicy'
  Text ->
  PutPolicy
putPolicy pResourceARN_ pPolicy_ =
  PutPolicy' {_ppResourceARN = pResourceARN_, _ppPolicy = pPolicy_}

-- | The Amazon Resource Number (ARN) of the private CA to associate with the policy. The ARN of the CA can be found by calling the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities> action.
ppResourceARN :: Lens' PutPolicy Text
ppResourceARN = lens _ppResourceARN (\s a -> s {_ppResourceARN = a})

-- | The path and filename of a JSON-formatted IAM policy to attach to the specified private CA resource. If this policy does not contain all required statements or if it includes any statement that is not allowed, the @PutPolicy@ action returns an @InvalidPolicyException@ . For information about IAM policy and statement structure, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#access_policies-json Overview of JSON Policies> .
ppPolicy :: Lens' PutPolicy Text
ppPolicy = lens _ppPolicy (\s a -> s {_ppPolicy = a})

instance AWSRequest PutPolicy where
  type Rs PutPolicy = PutPolicyResponse
  request = postJSON certificateManagerPCA
  response = receiveNull PutPolicyResponse'

instance Hashable PutPolicy

instance NFData PutPolicy

instance ToHeaders PutPolicy where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("ACMPrivateCA.PutPolicy" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON PutPolicy where
  toJSON PutPolicy' {..} =
    object
      ( catMaybes
          [ Just ("ResourceArn" .= _ppResourceARN),
            Just ("Policy" .= _ppPolicy)
          ]
      )

instance ToPath PutPolicy where
  toPath = const "/"

instance ToQuery PutPolicy where
  toQuery = const mempty

-- | /See:/ 'putPolicyResponse' smart constructor.
data PutPolicyResponse = PutPolicyResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutPolicyResponse' with the minimum fields required to make a request.
putPolicyResponse ::
  PutPolicyResponse
putPolicyResponse = PutPolicyResponse'

instance NFData PutPolicyResponse
