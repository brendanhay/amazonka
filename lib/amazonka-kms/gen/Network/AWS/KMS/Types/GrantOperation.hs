{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.GrantOperation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KMS.Types.GrantOperation
  ( GrantOperation
    ( GrantOperation'
    , GrantOperationDecrypt
    , GrantOperationEncrypt
    , GrantOperationGenerateDataKey
    , GrantOperationGenerateDataKeyWithoutPlaintext
    , GrantOperationReEncryptFrom
    , GrantOperationReEncryptTo
    , GrantOperationSign
    , GrantOperationVerify
    , GrantOperationGetPublicKey
    , GrantOperationCreateGrant
    , GrantOperationRetireGrant
    , GrantOperationDescribeKey
    , GrantOperationGenerateDataKeyPair
    , GrantOperationGenerateDataKeyPairWithoutPlaintext
    , fromGrantOperation
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype GrantOperation = GrantOperation'{fromGrantOperation ::
                                         Core.Text}
                           deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                           Core.Generic)
                           deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                             Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                             Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                             Core.FromText, Core.ToByteString, Core.ToQuery,
                                             Core.ToHeader)

pattern GrantOperationDecrypt :: GrantOperation
pattern GrantOperationDecrypt = GrantOperation' "Decrypt"

pattern GrantOperationEncrypt :: GrantOperation
pattern GrantOperationEncrypt = GrantOperation' "Encrypt"

pattern GrantOperationGenerateDataKey :: GrantOperation
pattern GrantOperationGenerateDataKey = GrantOperation' "GenerateDataKey"

pattern GrantOperationGenerateDataKeyWithoutPlaintext :: GrantOperation
pattern GrantOperationGenerateDataKeyWithoutPlaintext = GrantOperation' "GenerateDataKeyWithoutPlaintext"

pattern GrantOperationReEncryptFrom :: GrantOperation
pattern GrantOperationReEncryptFrom = GrantOperation' "ReEncryptFrom"

pattern GrantOperationReEncryptTo :: GrantOperation
pattern GrantOperationReEncryptTo = GrantOperation' "ReEncryptTo"

pattern GrantOperationSign :: GrantOperation
pattern GrantOperationSign = GrantOperation' "Sign"

pattern GrantOperationVerify :: GrantOperation
pattern GrantOperationVerify = GrantOperation' "Verify"

pattern GrantOperationGetPublicKey :: GrantOperation
pattern GrantOperationGetPublicKey = GrantOperation' "GetPublicKey"

pattern GrantOperationCreateGrant :: GrantOperation
pattern GrantOperationCreateGrant = GrantOperation' "CreateGrant"

pattern GrantOperationRetireGrant :: GrantOperation
pattern GrantOperationRetireGrant = GrantOperation' "RetireGrant"

pattern GrantOperationDescribeKey :: GrantOperation
pattern GrantOperationDescribeKey = GrantOperation' "DescribeKey"

pattern GrantOperationGenerateDataKeyPair :: GrantOperation
pattern GrantOperationGenerateDataKeyPair = GrantOperation' "GenerateDataKeyPair"

pattern GrantOperationGenerateDataKeyPairWithoutPlaintext :: GrantOperation
pattern GrantOperationGenerateDataKeyPairWithoutPlaintext = GrantOperation' "GenerateDataKeyPairWithoutPlaintext"

{-# COMPLETE 
  GrantOperationDecrypt,

  GrantOperationEncrypt,

  GrantOperationGenerateDataKey,

  GrantOperationGenerateDataKeyWithoutPlaintext,

  GrantOperationReEncryptFrom,

  GrantOperationReEncryptTo,

  GrantOperationSign,

  GrantOperationVerify,

  GrantOperationGetPublicKey,

  GrantOperationCreateGrant,

  GrantOperationRetireGrant,

  GrantOperationDescribeKey,

  GrantOperationGenerateDataKeyPair,

  GrantOperationGenerateDataKeyPairWithoutPlaintext,
  GrantOperation'
  #-}
