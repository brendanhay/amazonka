-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.GrantOperation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.GrantOperation
  ( GrantOperation
      ( GrantOperation',
        CreateGrant,
        Decrypt,
        DescribeKey,
        Encrypt,
        GenerateDataKey,
        GenerateDataKeyPair,
        GenerateDataKeyPairWithoutPlaintext,
        GenerateDataKeyWithoutPlaintext,
        GetPublicKey,
        ReEncryptFrom,
        ReEncryptTo,
        RetireGrant,
        Sign,
        Verify
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype GrantOperation = GrantOperation' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern CreateGrant :: GrantOperation
pattern CreateGrant = GrantOperation' "CreateGrant"

pattern Decrypt :: GrantOperation
pattern Decrypt = GrantOperation' "Decrypt"

pattern DescribeKey :: GrantOperation
pattern DescribeKey = GrantOperation' "DescribeKey"

pattern Encrypt :: GrantOperation
pattern Encrypt = GrantOperation' "Encrypt"

pattern GenerateDataKey :: GrantOperation
pattern GenerateDataKey = GrantOperation' "GenerateDataKey"

pattern GenerateDataKeyPair :: GrantOperation
pattern GenerateDataKeyPair = GrantOperation' "GenerateDataKeyPair"

pattern GenerateDataKeyPairWithoutPlaintext :: GrantOperation
pattern GenerateDataKeyPairWithoutPlaintext = GrantOperation' "GenerateDataKeyPairWithoutPlaintext"

pattern GenerateDataKeyWithoutPlaintext :: GrantOperation
pattern GenerateDataKeyWithoutPlaintext = GrantOperation' "GenerateDataKeyWithoutPlaintext"

pattern GetPublicKey :: GrantOperation
pattern GetPublicKey = GrantOperation' "GetPublicKey"

pattern ReEncryptFrom :: GrantOperation
pattern ReEncryptFrom = GrantOperation' "ReEncryptFrom"

pattern ReEncryptTo :: GrantOperation
pattern ReEncryptTo = GrantOperation' "ReEncryptTo"

pattern RetireGrant :: GrantOperation
pattern RetireGrant = GrantOperation' "RetireGrant"

pattern Sign :: GrantOperation
pattern Sign = GrantOperation' "Sign"

pattern Verify :: GrantOperation
pattern Verify = GrantOperation' "Verify"

{-# COMPLETE
  CreateGrant,
  Decrypt,
  DescribeKey,
  Encrypt,
  GenerateDataKey,
  GenerateDataKeyPair,
  GenerateDataKeyPairWithoutPlaintext,
  GenerateDataKeyWithoutPlaintext,
  GetPublicKey,
  ReEncryptFrom,
  ReEncryptTo,
  RetireGrant,
  Sign,
  Verify,
  GrantOperation'
  #-}
