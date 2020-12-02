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
-- Module      : Network.AWS.KMS.CreateGrant
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a grant to a customer master key (CMK). The grant allows the grantee principal to use the CMK when the conditions specified in the grant are met. When setting permissions, grants are an alternative to key policies.
--
--
-- To create a grant that allows a <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operation> only when the request includes a particular <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context encryption context> , use the @Constraints@ parameter. For details, see 'GrantConstraints' .
--
-- You can create grants on symmetric and asymmetric CMKs. However, if the grant allows an operation that the CMK does not support, @CreateGrant@ fails with a @ValidationException@ .
--
--     * Grants for symmetric CMKs cannot allow operations that are not supported for symmetric CMKs, including 'Sign' , 'Verify' , and 'GetPublicKey' . (There are limited exceptions to this rule for legacy operations, but you should not create a grant for an operation that AWS KMS does not support.)
--
--     * Grants for asymmetric CMKs cannot allow operations that are not supported for asymmetric CMKs, including operations that <https://docs.aws.amazon.com/kms/latest/APIReference/API_GenerateDataKey generate data keys> or <https://docs.aws.amazon.com/kms/latest/APIReference/API_GenerateDataKeyPair data key pairs> , or operations related to <https://docs.aws.amazon.com/kms/latest/developerguide/rotate-keys.html automatic key rotation> , <https://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html imported key material> , or CMKs in <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key stores> .
--
--     * Grants for asymmetric CMKs with a @KeyUsage@ of @ENCRYPT_DECRYPT@ cannot allow the 'Sign' or 'Verify' operations. Grants for asymmetric CMKs with a @KeyUsage@ of @SIGN_VERIFY@ cannot allow the 'Encrypt' or 'Decrypt' operations.
--
--     * Grants for asymmetric CMKs cannot include an encryption context grant constraint. An encryption context is not supported on asymmetric CMKs.
--
--
--
-- For information about symmetric and asymmetric CMKs, see <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric CMKs> in the /AWS Key Management Service Developer Guide/ .
--
-- To perform this operation on a CMK in a different AWS account, specify the key ARN in the value of the @KeyId@ parameter. For more information about grants, see <https://docs.aws.amazon.com/kms/latest/developerguide/grants.html Grants> in the /\/AWS Key Management Service Developer Guide\/ / .
--
-- The CMK that you use for this operation must be in a compatible key state. For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
module Network.AWS.KMS.CreateGrant
  ( -- * Creating a Request
    createGrant,
    CreateGrant,

    -- * Request Lenses
    cgRetiringPrincipal,
    cgGrantTokens,
    cgConstraints,
    cgName,
    cgKeyId,
    cgGranteePrincipal,
    cgOperations,

    -- * Destructuring the Response
    createGrantResponse,
    CreateGrantResponse,

    -- * Response Lenses
    cgrsGrantId,
    cgrsGrantToken,
    cgrsResponseStatus,
  )
where

import Network.AWS.KMS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createGrant' smart constructor.
data CreateGrant = CreateGrant'
  { _cgRetiringPrincipal ::
      !(Maybe Text),
    _cgGrantTokens :: !(Maybe [Text]),
    _cgConstraints :: !(Maybe GrantConstraints),
    _cgName :: !(Maybe Text),
    _cgKeyId :: !Text,
    _cgGranteePrincipal :: !Text,
    _cgOperations :: ![GrantOperation]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateGrant' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgRetiringPrincipal' - The principal that is given permission to retire the grant by using 'RetireGrant' operation. To specify the principal, use the <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)> of an AWS principal. Valid AWS principals include AWS accounts (root), IAM users, federated users, and assumed role users. For examples of the ARN syntax to use for specifying a principal, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam AWS Identity and Access Management (IAM)> in the Example ARNs section of the /AWS General Reference/ .
--
-- * 'cgGrantTokens' - A list of grant tokens. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
--
-- * 'cgConstraints' - Allows a <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operation> only when the encryption context matches or includes the encryption context specified in this structure. For more information about encryption context, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /\/AWS Key Management Service Developer Guide\/ / .
--
-- * 'cgName' - A friendly name for identifying the grant. Use this value to prevent the unintended creation of duplicate grants when retrying this request. When this value is absent, all @CreateGrant@ requests result in a new grant with a unique @GrantId@ even if all the supplied parameters are identical. This can result in unintended duplicates when you retry the @CreateGrant@ request. When this value is present, you can retry a @CreateGrant@ request with identical parameters; if the grant already exists, the original @GrantId@ is returned without creating a new grant. Note that the returned grant token is unique with every @CreateGrant@ request, even when a duplicate @GrantId@ is returned. All grant tokens obtained in this way can be used interchangeably.
--
-- * 'cgKeyId' - The unique identifier for the customer master key (CMK) that the grant applies to. Specify the key ID or the Amazon Resource Name (ARN) of the CMK. To specify a CMK in a different AWS account, you must use the key ARN. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
--
-- * 'cgGranteePrincipal' - The principal that is given permission to perform the operations that the grant permits. To specify the principal, use the <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)> of an AWS principal. Valid AWS principals include AWS accounts (root), IAM users, IAM roles, federated users, and assumed role users. For examples of the ARN syntax to use for specifying a principal, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam AWS Identity and Access Management (IAM)> in the Example ARNs section of the /AWS General Reference/ .
--
-- * 'cgOperations' - A list of operations that the grant permits.
createGrant ::
  -- | 'cgKeyId'
  Text ->
  -- | 'cgGranteePrincipal'
  Text ->
  CreateGrant
createGrant pKeyId_ pGranteePrincipal_ =
  CreateGrant'
    { _cgRetiringPrincipal = Nothing,
      _cgGrantTokens = Nothing,
      _cgConstraints = Nothing,
      _cgName = Nothing,
      _cgKeyId = pKeyId_,
      _cgGranteePrincipal = pGranteePrincipal_,
      _cgOperations = mempty
    }

-- | The principal that is given permission to retire the grant by using 'RetireGrant' operation. To specify the principal, use the <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)> of an AWS principal. Valid AWS principals include AWS accounts (root), IAM users, federated users, and assumed role users. For examples of the ARN syntax to use for specifying a principal, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam AWS Identity and Access Management (IAM)> in the Example ARNs section of the /AWS General Reference/ .
cgRetiringPrincipal :: Lens' CreateGrant (Maybe Text)
cgRetiringPrincipal = lens _cgRetiringPrincipal (\s a -> s {_cgRetiringPrincipal = a})

-- | A list of grant tokens. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
cgGrantTokens :: Lens' CreateGrant [Text]
cgGrantTokens = lens _cgGrantTokens (\s a -> s {_cgGrantTokens = a}) . _Default . _Coerce

-- | Allows a <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operation> only when the encryption context matches or includes the encryption context specified in this structure. For more information about encryption context, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /\/AWS Key Management Service Developer Guide\/ / .
cgConstraints :: Lens' CreateGrant (Maybe GrantConstraints)
cgConstraints = lens _cgConstraints (\s a -> s {_cgConstraints = a})

-- | A friendly name for identifying the grant. Use this value to prevent the unintended creation of duplicate grants when retrying this request. When this value is absent, all @CreateGrant@ requests result in a new grant with a unique @GrantId@ even if all the supplied parameters are identical. This can result in unintended duplicates when you retry the @CreateGrant@ request. When this value is present, you can retry a @CreateGrant@ request with identical parameters; if the grant already exists, the original @GrantId@ is returned without creating a new grant. Note that the returned grant token is unique with every @CreateGrant@ request, even when a duplicate @GrantId@ is returned. All grant tokens obtained in this way can be used interchangeably.
cgName :: Lens' CreateGrant (Maybe Text)
cgName = lens _cgName (\s a -> s {_cgName = a})

-- | The unique identifier for the customer master key (CMK) that the grant applies to. Specify the key ID or the Amazon Resource Name (ARN) of the CMK. To specify a CMK in a different AWS account, you must use the key ARN. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
cgKeyId :: Lens' CreateGrant Text
cgKeyId = lens _cgKeyId (\s a -> s {_cgKeyId = a})

-- | The principal that is given permission to perform the operations that the grant permits. To specify the principal, use the <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)> of an AWS principal. Valid AWS principals include AWS accounts (root), IAM users, IAM roles, federated users, and assumed role users. For examples of the ARN syntax to use for specifying a principal, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam AWS Identity and Access Management (IAM)> in the Example ARNs section of the /AWS General Reference/ .
cgGranteePrincipal :: Lens' CreateGrant Text
cgGranteePrincipal = lens _cgGranteePrincipal (\s a -> s {_cgGranteePrincipal = a})

-- | A list of operations that the grant permits.
cgOperations :: Lens' CreateGrant [GrantOperation]
cgOperations = lens _cgOperations (\s a -> s {_cgOperations = a}) . _Coerce

instance AWSRequest CreateGrant where
  type Rs CreateGrant = CreateGrantResponse
  request = postJSON kms
  response =
    receiveJSON
      ( \s h x ->
          CreateGrantResponse'
            <$> (x .?> "GrantId") <*> (x .?> "GrantToken") <*> (pure (fromEnum s))
      )

instance Hashable CreateGrant

instance NFData CreateGrant

instance ToHeaders CreateGrant where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("TrentService.CreateGrant" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateGrant where
  toJSON CreateGrant' {..} =
    object
      ( catMaybes
          [ ("RetiringPrincipal" .=) <$> _cgRetiringPrincipal,
            ("GrantTokens" .=) <$> _cgGrantTokens,
            ("Constraints" .=) <$> _cgConstraints,
            ("Name" .=) <$> _cgName,
            Just ("KeyId" .= _cgKeyId),
            Just ("GranteePrincipal" .= _cgGranteePrincipal),
            Just ("Operations" .= _cgOperations)
          ]
      )

instance ToPath CreateGrant where
  toPath = const "/"

instance ToQuery CreateGrant where
  toQuery = const mempty

-- | /See:/ 'createGrantResponse' smart constructor.
data CreateGrantResponse = CreateGrantResponse'
  { _cgrsGrantId ::
      !(Maybe Text),
    _cgrsGrantToken :: !(Maybe Text),
    _cgrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateGrantResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgrsGrantId' - The unique identifier for the grant. You can use the @GrantId@ in a subsequent 'RetireGrant' or 'RevokeGrant' operation.
--
-- * 'cgrsGrantToken' - The grant token. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
--
-- * 'cgrsResponseStatus' - -- | The response status code.
createGrantResponse ::
  -- | 'cgrsResponseStatus'
  Int ->
  CreateGrantResponse
createGrantResponse pResponseStatus_ =
  CreateGrantResponse'
    { _cgrsGrantId = Nothing,
      _cgrsGrantToken = Nothing,
      _cgrsResponseStatus = pResponseStatus_
    }

-- | The unique identifier for the grant. You can use the @GrantId@ in a subsequent 'RetireGrant' or 'RevokeGrant' operation.
cgrsGrantId :: Lens' CreateGrantResponse (Maybe Text)
cgrsGrantId = lens _cgrsGrantId (\s a -> s {_cgrsGrantId = a})

-- | The grant token. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
cgrsGrantToken :: Lens' CreateGrantResponse (Maybe Text)
cgrsGrantToken = lens _cgrsGrantToken (\s a -> s {_cgrsGrantToken = a})

-- | -- | The response status code.
cgrsResponseStatus :: Lens' CreateGrantResponse Int
cgrsResponseStatus = lens _cgrsResponseStatus (\s a -> s {_cgrsResponseStatus = a})

instance NFData CreateGrantResponse
