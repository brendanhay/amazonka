{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.GrantConstraints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.GrantConstraints where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Use this structure to allow <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations> in the grant only when the operation request includes the specified <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context encryption context> .
--
--
-- AWS KMS applies the grant constraints only to cryptographic operations that support an encryption context, that is, all cryptographic operations with a <https://docs.aws.amazon.com/kms/latest/developerguide/symm-asymm-concepts.html#symmetric-cmks symmetric CMK> . Grant constraints are not applied to operations that do not support an encryption context, such as cryptographic operations with asymmetric CMKs and management operations, such as 'DescribeKey' or 'ScheduleKeyDeletion' .
--
-- /Important:/ In a cryptographic operation, the encryption context in the decryption operation must be an exact, case-sensitive match for the keys and values in the encryption context of the encryption operation. Only the order of the pairs can vary.
--
-- However, in a grant constraint, the key in each key-value pair is not case sensitive, but the value is case sensitive.
--
-- To avoid confusion, do not use multiple encryption context pairs that differ only by case. To require a fully case-sensitive encryption context, use the @kms:EncryptionContext:@ and @kms:EncryptionContextKeys@ conditions in an IAM or key policy. For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/policy-conditions.html#conditions-kms-encryption-context kms:EncryptionContext:> in the /\/AWS Key Management Service Developer Guide\/ / .
--
--
-- /See:/ 'grantConstraints' smart constructor.
data GrantConstraints = GrantConstraints'
  { _gcEncryptionContextEquals ::
      !(Maybe (Map Text (Text))),
    _gcEncryptionContextSubset :: !(Maybe (Map Text (Text)))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GrantConstraints' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcEncryptionContextEquals' - A list of key-value pairs that must match the encryption context in the <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operation> request. The grant allows the operation only when the encryption context in the request is the same as the encryption context specified in this constraint.
--
-- * 'gcEncryptionContextSubset' - A list of key-value pairs that must be included in the encryption context of the <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operation> request. The grant allows the cryptographic operation only when the encryption context in the request includes the key-value pairs specified in this constraint, although it can include additional key-value pairs.
grantConstraints ::
  GrantConstraints
grantConstraints =
  GrantConstraints'
    { _gcEncryptionContextEquals = Nothing,
      _gcEncryptionContextSubset = Nothing
    }

-- | A list of key-value pairs that must match the encryption context in the <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operation> request. The grant allows the operation only when the encryption context in the request is the same as the encryption context specified in this constraint.
gcEncryptionContextEquals :: Lens' GrantConstraints (HashMap Text (Text))
gcEncryptionContextEquals = lens _gcEncryptionContextEquals (\s a -> s {_gcEncryptionContextEquals = a}) . _Default . _Map

-- | A list of key-value pairs that must be included in the encryption context of the <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operation> request. The grant allows the cryptographic operation only when the encryption context in the request includes the key-value pairs specified in this constraint, although it can include additional key-value pairs.
gcEncryptionContextSubset :: Lens' GrantConstraints (HashMap Text (Text))
gcEncryptionContextSubset = lens _gcEncryptionContextSubset (\s a -> s {_gcEncryptionContextSubset = a}) . _Default . _Map

instance FromJSON GrantConstraints where
  parseJSON =
    withObject
      "GrantConstraints"
      ( \x ->
          GrantConstraints'
            <$> (x .:? "EncryptionContextEquals" .!= mempty)
            <*> (x .:? "EncryptionContextSubset" .!= mempty)
      )

instance Hashable GrantConstraints

instance NFData GrantConstraints

instance ToJSON GrantConstraints where
  toJSON GrantConstraints' {..} =
    object
      ( catMaybes
          [ ("EncryptionContextEquals" .=) <$> _gcEncryptionContextEquals,
            ("EncryptionContextSubset" .=) <$> _gcEncryptionContextSubset
          ]
      )
