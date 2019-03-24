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
-- Module      : Network.AWS.KMS.CreateKey
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a customer master key (CMK) in the caller's AWS account.
--
--
-- You can use a CMK to encrypt small amounts of data (4 KiB or less) directly, but CMKs are more commonly used to encrypt data keys, which are used to encrypt raw data. For more information about data keys and the difference between CMKs and data keys, see the following:
--
--     * The 'GenerateDataKey' operation
--
--     * <http://docs.aws.amazon.com/kms/latest/developerguide/concepts.html AWS Key Management Service Concepts> in the /AWS Key Management Service Developer Guide/
--
--
--
-- If you plan to <http://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html import key material> , use the @Origin@ parameter with a value of @EXTERNAL@ to create a CMK with no key material.
--
-- To create a CMK in a <http://docs.aws.amazon.com/kms/latest/developerguide/key-store-overview.html custom key store> , use @CustomKeyStoreId@ parameter to specify the custom key store. You must also use the @Origin@ parameter with a value of @AWS_CLOUDHSM@ . The AWS CloudHSM cluster that is associated with the custom key store must have at least two active HSMs, each in a different Availability Zone in the Region.
--
-- You cannot use this operation to create a CMK in a different AWS account.
--
module Network.AWS.KMS.CreateKey
    (
    -- * Creating a Request
      createKey
    , CreateKey
    -- * Request Lenses
    , ckOrigin
    , ckKeyUsage
    , ckBypassPolicyLockoutSafetyCheck
    , ckPolicy
    , ckDescription
    , ckCustomKeyStoreId
    , ckTags

    -- * Destructuring the Response
    , createKeyResponse
    , CreateKeyResponse
    -- * Response Lenses
    , ckrsKeyMetadata
    , ckrsResponseStatus
    ) where

import Network.AWS.KMS.Types
import Network.AWS.KMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createKey' smart constructor.
data CreateKey = CreateKey'
  { _ckOrigin                         :: !(Maybe OriginType)
  , _ckKeyUsage                       :: !(Maybe KeyUsageType)
  , _ckBypassPolicyLockoutSafetyCheck :: !(Maybe Bool)
  , _ckPolicy                         :: !(Maybe Text)
  , _ckDescription                    :: !(Maybe Text)
  , _ckCustomKeyStoreId               :: !(Maybe Text)
  , _ckTags                           :: !(Maybe [Tag])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ckOrigin' - The source of the CMK's key material. You cannot change the origin after you create the CMK. The default is @AWS_KMS@ , which means AWS KMS creates the key material in its own key store. When the parameter value is @EXTERNAL@ , AWS KMS creates a CMK without key material so that you can import key material from your existing key management infrastructure. For more information about importing key material into AWS KMS, see <http://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html Importing Key Material> in the /AWS Key Management Service Developer Guide/ . When the parameter value is @AWS_CLOUDHSM@ , AWS KMS creates the CMK in a AWS KMS <http://docs.aws.amazon.com/kms/latest/developerguide/key-store-overview.html custom key store> and creates its key material in the associated AWS CloudHSM cluster. You must also use the @CustomKeyStoreId@ parameter to identify the custom key store.
--
-- * 'ckKeyUsage' - The intended use of the CMK. You can use CMKs only for symmetric encryption and decryption.
--
-- * 'ckBypassPolicyLockoutSafetyCheck' - A flag to indicate whether to bypass the key policy lockout safety check. /Important:/ Setting this value to true increases the risk that the CMK becomes unmanageable. Do not set this value to true indiscriminately. For more information, refer to the scenario in the <http://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html#key-policy-default-allow-root-enable-iam Default Key Policy> section in the /AWS Key Management Service Developer Guide/ . Use this parameter only when you include a policy in the request and you intend to prevent the principal that is making the request from making a subsequent 'PutKeyPolicy' request on the CMK. The default value is false.
--
-- * 'ckPolicy' - The key policy to attach to the CMK. If you provide a key policy, it must meet the following criteria:     * If you don't set @BypassPolicyLockoutSafetyCheck@ to true, the key policy must allow the principal that is making the @CreateKey@ request to make a subsequent 'PutKeyPolicy' request on the CMK. This reduces the risk that the CMK becomes unmanageable. For more information, refer to the scenario in the <http://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html#key-policy-default-allow-root-enable-iam Default Key Policy> section of the /AWS Key Management Service Developer Guide/ .     * Each statement in the key policy must contain one or more principals. The principals in the key policy must exist and be visible to AWS KMS. When you create a new AWS principal (for example, an IAM user or role), you might need to enforce a delay before including the new principal in a key policy because the new principal might not be immediately visible to AWS KMS. For more information, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/troubleshoot_general.html#troubleshoot_general_eventual-consistency Changes that I make are not always immediately visible> in the /AWS Identity and Access Management User Guide/ . If you do not provide a key policy, AWS KMS attaches a default key policy to the CMK. For more information, see <http://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html#key-policy-default Default Key Policy> in the /AWS Key Management Service Developer Guide/ . The key policy size limit is 32 kilobytes (32768 bytes).
--
-- * 'ckDescription' - A description of the CMK. Use a description that helps you decide whether the CMK is appropriate for a task.
--
-- * 'ckCustomKeyStoreId' - Creates the CMK in the specified <http://docs.aws.amazon.com/kms/latest/developerguide/key-store-overview.html custom key store> and the key material in its associated AWS CloudHSM cluster. To create a CMK in a custom key store, you must also specify the @Origin@ parameter with a value of @AWS_CLOUDHSM@ . The AWS CloudHSM cluster that is associated with the custom key store must have at least two active HSMs, each in a different Availability Zone in the Region. To find the ID of a custom key store, use the 'DescribeCustomKeyStores' operation. The response includes the custom key store ID and the ID of the AWS CloudHSM cluster. This operation is part of the <http://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html Custom Key Store feature> feature in AWS KMS, which combines the convenience and extensive integration of AWS KMS with the isolation and control of a single-tenant key store.
--
-- * 'ckTags' - One or more tags. Each tag consists of a tag key and a tag value. Tag keys and tag values are both required, but tag values can be empty (null) strings. Use this parameter to tag the CMK when it is created. Alternately, you can omit this parameter and instead tag the CMK after it is created using 'TagResource' .
createKey
    :: CreateKey
createKey =
  CreateKey'
    { _ckOrigin = Nothing
    , _ckKeyUsage = Nothing
    , _ckBypassPolicyLockoutSafetyCheck = Nothing
    , _ckPolicy = Nothing
    , _ckDescription = Nothing
    , _ckCustomKeyStoreId = Nothing
    , _ckTags = Nothing
    }


-- | The source of the CMK's key material. You cannot change the origin after you create the CMK. The default is @AWS_KMS@ , which means AWS KMS creates the key material in its own key store. When the parameter value is @EXTERNAL@ , AWS KMS creates a CMK without key material so that you can import key material from your existing key management infrastructure. For more information about importing key material into AWS KMS, see <http://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html Importing Key Material> in the /AWS Key Management Service Developer Guide/ . When the parameter value is @AWS_CLOUDHSM@ , AWS KMS creates the CMK in a AWS KMS <http://docs.aws.amazon.com/kms/latest/developerguide/key-store-overview.html custom key store> and creates its key material in the associated AWS CloudHSM cluster. You must also use the @CustomKeyStoreId@ parameter to identify the custom key store.
ckOrigin :: Lens' CreateKey (Maybe OriginType)
ckOrigin = lens _ckOrigin (\ s a -> s{_ckOrigin = a})

-- | The intended use of the CMK. You can use CMKs only for symmetric encryption and decryption.
ckKeyUsage :: Lens' CreateKey (Maybe KeyUsageType)
ckKeyUsage = lens _ckKeyUsage (\ s a -> s{_ckKeyUsage = a})

-- | A flag to indicate whether to bypass the key policy lockout safety check. /Important:/ Setting this value to true increases the risk that the CMK becomes unmanageable. Do not set this value to true indiscriminately. For more information, refer to the scenario in the <http://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html#key-policy-default-allow-root-enable-iam Default Key Policy> section in the /AWS Key Management Service Developer Guide/ . Use this parameter only when you include a policy in the request and you intend to prevent the principal that is making the request from making a subsequent 'PutKeyPolicy' request on the CMK. The default value is false.
ckBypassPolicyLockoutSafetyCheck :: Lens' CreateKey (Maybe Bool)
ckBypassPolicyLockoutSafetyCheck = lens _ckBypassPolicyLockoutSafetyCheck (\ s a -> s{_ckBypassPolicyLockoutSafetyCheck = a})

-- | The key policy to attach to the CMK. If you provide a key policy, it must meet the following criteria:     * If you don't set @BypassPolicyLockoutSafetyCheck@ to true, the key policy must allow the principal that is making the @CreateKey@ request to make a subsequent 'PutKeyPolicy' request on the CMK. This reduces the risk that the CMK becomes unmanageable. For more information, refer to the scenario in the <http://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html#key-policy-default-allow-root-enable-iam Default Key Policy> section of the /AWS Key Management Service Developer Guide/ .     * Each statement in the key policy must contain one or more principals. The principals in the key policy must exist and be visible to AWS KMS. When you create a new AWS principal (for example, an IAM user or role), you might need to enforce a delay before including the new principal in a key policy because the new principal might not be immediately visible to AWS KMS. For more information, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/troubleshoot_general.html#troubleshoot_general_eventual-consistency Changes that I make are not always immediately visible> in the /AWS Identity and Access Management User Guide/ . If you do not provide a key policy, AWS KMS attaches a default key policy to the CMK. For more information, see <http://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html#key-policy-default Default Key Policy> in the /AWS Key Management Service Developer Guide/ . The key policy size limit is 32 kilobytes (32768 bytes).
ckPolicy :: Lens' CreateKey (Maybe Text)
ckPolicy = lens _ckPolicy (\ s a -> s{_ckPolicy = a})

-- | A description of the CMK. Use a description that helps you decide whether the CMK is appropriate for a task.
ckDescription :: Lens' CreateKey (Maybe Text)
ckDescription = lens _ckDescription (\ s a -> s{_ckDescription = a})

-- | Creates the CMK in the specified <http://docs.aws.amazon.com/kms/latest/developerguide/key-store-overview.html custom key store> and the key material in its associated AWS CloudHSM cluster. To create a CMK in a custom key store, you must also specify the @Origin@ parameter with a value of @AWS_CLOUDHSM@ . The AWS CloudHSM cluster that is associated with the custom key store must have at least two active HSMs, each in a different Availability Zone in the Region. To find the ID of a custom key store, use the 'DescribeCustomKeyStores' operation. The response includes the custom key store ID and the ID of the AWS CloudHSM cluster. This operation is part of the <http://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html Custom Key Store feature> feature in AWS KMS, which combines the convenience and extensive integration of AWS KMS with the isolation and control of a single-tenant key store.
ckCustomKeyStoreId :: Lens' CreateKey (Maybe Text)
ckCustomKeyStoreId = lens _ckCustomKeyStoreId (\ s a -> s{_ckCustomKeyStoreId = a})

-- | One or more tags. Each tag consists of a tag key and a tag value. Tag keys and tag values are both required, but tag values can be empty (null) strings. Use this parameter to tag the CMK when it is created. Alternately, you can omit this parameter and instead tag the CMK after it is created using 'TagResource' .
ckTags :: Lens' CreateKey [Tag]
ckTags = lens _ckTags (\ s a -> s{_ckTags = a}) . _Default . _Coerce

instance AWSRequest CreateKey where
        type Rs CreateKey = CreateKeyResponse
        request = postJSON kms
        response
          = receiveJSON
              (\ s h x ->
                 CreateKeyResponse' <$>
                   (x .?> "KeyMetadata") <*> (pure (fromEnum s)))

instance Hashable CreateKey where

instance NFData CreateKey where

instance ToHeaders CreateKey where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("TrentService.CreateKey" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateKey where
        toJSON CreateKey'{..}
          = object
              (catMaybes
                 [("Origin" .=) <$> _ckOrigin,
                  ("KeyUsage" .=) <$> _ckKeyUsage,
                  ("BypassPolicyLockoutSafetyCheck" .=) <$>
                    _ckBypassPolicyLockoutSafetyCheck,
                  ("Policy" .=) <$> _ckPolicy,
                  ("Description" .=) <$> _ckDescription,
                  ("CustomKeyStoreId" .=) <$> _ckCustomKeyStoreId,
                  ("Tags" .=) <$> _ckTags])

instance ToPath CreateKey where
        toPath = const "/"

instance ToQuery CreateKey where
        toQuery = const mempty

-- | /See:/ 'createKeyResponse' smart constructor.
data CreateKeyResponse = CreateKeyResponse'
  { _ckrsKeyMetadata    :: !(Maybe KeyMetadata)
  , _ckrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateKeyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ckrsKeyMetadata' - Metadata associated with the CMK.
--
-- * 'ckrsResponseStatus' - -- | The response status code.
createKeyResponse
    :: Int -- ^ 'ckrsResponseStatus'
    -> CreateKeyResponse
createKeyResponse pResponseStatus_ =
  CreateKeyResponse'
    {_ckrsKeyMetadata = Nothing, _ckrsResponseStatus = pResponseStatus_}


-- | Metadata associated with the CMK.
ckrsKeyMetadata :: Lens' CreateKeyResponse (Maybe KeyMetadata)
ckrsKeyMetadata = lens _ckrsKeyMetadata (\ s a -> s{_ckrsKeyMetadata = a})

-- | -- | The response status code.
ckrsResponseStatus :: Lens' CreateKeyResponse Int
ckrsResponseStatus = lens _ckrsResponseStatus (\ s a -> s{_ckrsResponseStatus = a})

instance NFData CreateKeyResponse where
