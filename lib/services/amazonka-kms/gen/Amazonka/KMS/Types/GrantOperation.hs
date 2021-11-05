{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.KMS.Types.GrantOperation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KMS.Types.GrantOperation
  ( GrantOperation
      ( ..,
        GrantOperation_CreateGrant,
        GrantOperation_Decrypt,
        GrantOperation_DescribeKey,
        GrantOperation_Encrypt,
        GrantOperation_GenerateDataKey,
        GrantOperation_GenerateDataKeyPair,
        GrantOperation_GenerateDataKeyPairWithoutPlaintext,
        GrantOperation_GenerateDataKeyWithoutPlaintext,
        GrantOperation_GetPublicKey,
        GrantOperation_ReEncryptFrom,
        GrantOperation_ReEncryptTo,
        GrantOperation_RetireGrant,
        GrantOperation_Sign,
        GrantOperation_Verify
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype GrantOperation = GrantOperation'
  { fromGrantOperation ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern GrantOperation_CreateGrant :: GrantOperation
pattern GrantOperation_CreateGrant = GrantOperation' "CreateGrant"

pattern GrantOperation_Decrypt :: GrantOperation
pattern GrantOperation_Decrypt = GrantOperation' "Decrypt"

pattern GrantOperation_DescribeKey :: GrantOperation
pattern GrantOperation_DescribeKey = GrantOperation' "DescribeKey"

pattern GrantOperation_Encrypt :: GrantOperation
pattern GrantOperation_Encrypt = GrantOperation' "Encrypt"

pattern GrantOperation_GenerateDataKey :: GrantOperation
pattern GrantOperation_GenerateDataKey = GrantOperation' "GenerateDataKey"

pattern GrantOperation_GenerateDataKeyPair :: GrantOperation
pattern GrantOperation_GenerateDataKeyPair = GrantOperation' "GenerateDataKeyPair"

pattern GrantOperation_GenerateDataKeyPairWithoutPlaintext :: GrantOperation
pattern GrantOperation_GenerateDataKeyPairWithoutPlaintext = GrantOperation' "GenerateDataKeyPairWithoutPlaintext"

pattern GrantOperation_GenerateDataKeyWithoutPlaintext :: GrantOperation
pattern GrantOperation_GenerateDataKeyWithoutPlaintext = GrantOperation' "GenerateDataKeyWithoutPlaintext"

pattern GrantOperation_GetPublicKey :: GrantOperation
pattern GrantOperation_GetPublicKey = GrantOperation' "GetPublicKey"

pattern GrantOperation_ReEncryptFrom :: GrantOperation
pattern GrantOperation_ReEncryptFrom = GrantOperation' "ReEncryptFrom"

pattern GrantOperation_ReEncryptTo :: GrantOperation
pattern GrantOperation_ReEncryptTo = GrantOperation' "ReEncryptTo"

pattern GrantOperation_RetireGrant :: GrantOperation
pattern GrantOperation_RetireGrant = GrantOperation' "RetireGrant"

pattern GrantOperation_Sign :: GrantOperation
pattern GrantOperation_Sign = GrantOperation' "Sign"

pattern GrantOperation_Verify :: GrantOperation
pattern GrantOperation_Verify = GrantOperation' "Verify"

{-# COMPLETE
  GrantOperation_CreateGrant,
  GrantOperation_Decrypt,
  GrantOperation_DescribeKey,
  GrantOperation_Encrypt,
  GrantOperation_GenerateDataKey,
  GrantOperation_GenerateDataKeyPair,
  GrantOperation_GenerateDataKeyPairWithoutPlaintext,
  GrantOperation_GenerateDataKeyWithoutPlaintext,
  GrantOperation_GetPublicKey,
  GrantOperation_ReEncryptFrom,
  GrantOperation_ReEncryptTo,
  GrantOperation_RetireGrant,
  GrantOperation_Sign,
  GrantOperation_Verify,
  GrantOperation'
  #-}
