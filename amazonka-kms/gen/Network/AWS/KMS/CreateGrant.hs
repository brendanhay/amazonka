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
-- Module      : Network.AWS.KMS.CreateGrant
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a grant to a customer master key (CMK). The grant specifies who can use the CMK and under what conditions. When setting permissions, grants are an alternative to key policies.
--
--
-- To perform this operation on a CMK in a different AWS account, specify the key ARN in the value of the KeyId parameter. For more information about grants, see <http://docs.aws.amazon.com/kms/latest/developerguide/grants.html Grants> in the /AWS Key Management Service Developer Guide/ .
--
module Network.AWS.KMS.CreateGrant
    (
    -- * Creating a Request
      createGrant
    , CreateGrant
    -- * Request Lenses
    , cgRetiringPrincipal
    , cgGrantTokens
    , cgConstraints
    , cgName
    , cgKeyId
    , cgGranteePrincipal
    , cgOperations

    -- * Destructuring the Response
    , createGrantResponse
    , CreateGrantResponse
    -- * Response Lenses
    , cgrsGrantId
    , cgrsGrantToken
    , cgrsResponseStatus
    ) where

import Network.AWS.KMS.Types
import Network.AWS.KMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createGrant' smart constructor.
data CreateGrant = CreateGrant'
  { _cgRetiringPrincipal :: !(Maybe Text)
  , _cgGrantTokens       :: !(Maybe [Text])
  , _cgConstraints       :: !(Maybe GrantConstraints)
  , _cgName              :: !(Maybe Text)
  , _cgKeyId             :: !Text
  , _cgGranteePrincipal  :: !Text
  , _cgOperations        :: ![GrantOperation]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateGrant' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgRetiringPrincipal' - The principal that is given permission to retire the grant by using 'RetireGrant' operation. To specify the principal, use the <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)> of an AWS principal. Valid AWS principals include AWS accounts (root), IAM users, federated users, and assumed role users. For examples of the ARN syntax to use for specifying a principal, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam AWS Identity and Access Management (IAM)> in the Example ARNs section of the /AWS General Reference/ .
--
-- * 'cgGrantTokens' - A list of grant tokens. For more information, see <http://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
--
-- * 'cgConstraints' - A structure that you can use to allow certain operations in the grant only when the desired encryption context is present. For more information about encryption context, see <http://docs.aws.amazon.com/kms/latest/developerguide/encryption-context.html Encryption Context> in the /AWS Key Management Service Developer Guide/ .
--
-- * 'cgName' - A friendly name for identifying the grant. Use this value to prevent unintended creation of duplicate grants when retrying this request. When this value is absent, all @CreateGrant@ requests result in a new grant with a unique @GrantId@ even if all the supplied parameters are identical. This can result in unintended duplicates when you retry the @CreateGrant@ request. When this value is present, you can retry a @CreateGrant@ request with identical parameters; if the grant already exists, the original @GrantId@ is returned without creating a new grant. Note that the returned grant token is unique with every @CreateGrant@ request, even when a duplicate @GrantId@ is returned. All grant tokens obtained in this way can be used interchangeably.
--
-- * 'cgKeyId' - The unique identifier for the customer master key (CMK) that the grant applies to. Specify the key ID or the Amazon Resource Name (ARN) of the CMK. To specify a CMK in a different AWS account, you must use the key ARN. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
--
-- * 'cgGranteePrincipal' - The principal that is given permission to perform the operations that the grant permits. To specify the principal, use the <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)> of an AWS principal. Valid AWS principals include AWS accounts (root), IAM users, IAM roles, federated users, and assumed role users. For examples of the ARN syntax to use for specifying a principal, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam AWS Identity and Access Management (IAM)> in the Example ARNs section of the /AWS General Reference/ .
--
-- * 'cgOperations' - A list of operations that the grant permits.
createGrant
    :: Text -- ^ 'cgKeyId'
    -> Text -- ^ 'cgGranteePrincipal'
    -> CreateGrant
createGrant pKeyId_ pGranteePrincipal_ =
  CreateGrant'
    { _cgRetiringPrincipal = Nothing
    , _cgGrantTokens = Nothing
    , _cgConstraints = Nothing
    , _cgName = Nothing
    , _cgKeyId = pKeyId_
    , _cgGranteePrincipal = pGranteePrincipal_
    , _cgOperations = mempty
    }


-- | The principal that is given permission to retire the grant by using 'RetireGrant' operation. To specify the principal, use the <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)> of an AWS principal. Valid AWS principals include AWS accounts (root), IAM users, federated users, and assumed role users. For examples of the ARN syntax to use for specifying a principal, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam AWS Identity and Access Management (IAM)> in the Example ARNs section of the /AWS General Reference/ .
cgRetiringPrincipal :: Lens' CreateGrant (Maybe Text)
cgRetiringPrincipal = lens _cgRetiringPrincipal (\ s a -> s{_cgRetiringPrincipal = a})

-- | A list of grant tokens. For more information, see <http://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
cgGrantTokens :: Lens' CreateGrant [Text]
cgGrantTokens = lens _cgGrantTokens (\ s a -> s{_cgGrantTokens = a}) . _Default . _Coerce

-- | A structure that you can use to allow certain operations in the grant only when the desired encryption context is present. For more information about encryption context, see <http://docs.aws.amazon.com/kms/latest/developerguide/encryption-context.html Encryption Context> in the /AWS Key Management Service Developer Guide/ .
cgConstraints :: Lens' CreateGrant (Maybe GrantConstraints)
cgConstraints = lens _cgConstraints (\ s a -> s{_cgConstraints = a})

-- | A friendly name for identifying the grant. Use this value to prevent unintended creation of duplicate grants when retrying this request. When this value is absent, all @CreateGrant@ requests result in a new grant with a unique @GrantId@ even if all the supplied parameters are identical. This can result in unintended duplicates when you retry the @CreateGrant@ request. When this value is present, you can retry a @CreateGrant@ request with identical parameters; if the grant already exists, the original @GrantId@ is returned without creating a new grant. Note that the returned grant token is unique with every @CreateGrant@ request, even when a duplicate @GrantId@ is returned. All grant tokens obtained in this way can be used interchangeably.
cgName :: Lens' CreateGrant (Maybe Text)
cgName = lens _cgName (\ s a -> s{_cgName = a})

-- | The unique identifier for the customer master key (CMK) that the grant applies to. Specify the key ID or the Amazon Resource Name (ARN) of the CMK. To specify a CMK in a different AWS account, you must use the key ARN. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
cgKeyId :: Lens' CreateGrant Text
cgKeyId = lens _cgKeyId (\ s a -> s{_cgKeyId = a})

-- | The principal that is given permission to perform the operations that the grant permits. To specify the principal, use the <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)> of an AWS principal. Valid AWS principals include AWS accounts (root), IAM users, IAM roles, federated users, and assumed role users. For examples of the ARN syntax to use for specifying a principal, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam AWS Identity and Access Management (IAM)> in the Example ARNs section of the /AWS General Reference/ .
cgGranteePrincipal :: Lens' CreateGrant Text
cgGranteePrincipal = lens _cgGranteePrincipal (\ s a -> s{_cgGranteePrincipal = a})

-- | A list of operations that the grant permits.
cgOperations :: Lens' CreateGrant [GrantOperation]
cgOperations = lens _cgOperations (\ s a -> s{_cgOperations = a}) . _Coerce

instance AWSRequest CreateGrant where
        type Rs CreateGrant = CreateGrantResponse
        request = postJSON kms
        response
          = receiveJSON
              (\ s h x ->
                 CreateGrantResponse' <$>
                   (x .?> "GrantId") <*> (x .?> "GrantToken") <*>
                     (pure (fromEnum s)))

instance Hashable CreateGrant where

instance NFData CreateGrant where

instance ToHeaders CreateGrant where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("TrentService.CreateGrant" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateGrant where
        toJSON CreateGrant'{..}
          = object
              (catMaybes
                 [("RetiringPrincipal" .=) <$> _cgRetiringPrincipal,
                  ("GrantTokens" .=) <$> _cgGrantTokens,
                  ("Constraints" .=) <$> _cgConstraints,
                  ("Name" .=) <$> _cgName, Just ("KeyId" .= _cgKeyId),
                  Just ("GranteePrincipal" .= _cgGranteePrincipal),
                  Just ("Operations" .= _cgOperations)])

instance ToPath CreateGrant where
        toPath = const "/"

instance ToQuery CreateGrant where
        toQuery = const mempty

-- | /See:/ 'createGrantResponse' smart constructor.
data CreateGrantResponse = CreateGrantResponse'
  { _cgrsGrantId        :: !(Maybe Text)
  , _cgrsGrantToken     :: !(Maybe Text)
  , _cgrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateGrantResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgrsGrantId' - The unique identifier for the grant. You can use the @GrantId@ in a subsequent 'RetireGrant' or 'RevokeGrant' operation.
--
-- * 'cgrsGrantToken' - The grant token. For more information, see <http://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
--
-- * 'cgrsResponseStatus' - -- | The response status code.
createGrantResponse
    :: Int -- ^ 'cgrsResponseStatus'
    -> CreateGrantResponse
createGrantResponse pResponseStatus_ =
  CreateGrantResponse'
    { _cgrsGrantId = Nothing
    , _cgrsGrantToken = Nothing
    , _cgrsResponseStatus = pResponseStatus_
    }


-- | The unique identifier for the grant. You can use the @GrantId@ in a subsequent 'RetireGrant' or 'RevokeGrant' operation.
cgrsGrantId :: Lens' CreateGrantResponse (Maybe Text)
cgrsGrantId = lens _cgrsGrantId (\ s a -> s{_cgrsGrantId = a})

-- | The grant token. For more information, see <http://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
cgrsGrantToken :: Lens' CreateGrantResponse (Maybe Text)
cgrsGrantToken = lens _cgrsGrantToken (\ s a -> s{_cgrsGrantToken = a})

-- | -- | The response status code.
cgrsResponseStatus :: Lens' CreateGrantResponse Int
cgrsResponseStatus = lens _cgrsResponseStatus (\ s a -> s{_cgrsResponseStatus = a})

instance NFData CreateGrantResponse where
