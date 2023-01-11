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
-- Module      : Amazonka.AccessAnalyzer.Types.KmsGrantOperation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.KmsGrantOperation
  ( KmsGrantOperation
      ( ..,
        KmsGrantOperation_CreateGrant,
        KmsGrantOperation_Decrypt,
        KmsGrantOperation_DescribeKey,
        KmsGrantOperation_Encrypt,
        KmsGrantOperation_GenerateDataKey,
        KmsGrantOperation_GenerateDataKeyPair,
        KmsGrantOperation_GenerateDataKeyPairWithoutPlaintext,
        KmsGrantOperation_GenerateDataKeyWithoutPlaintext,
        KmsGrantOperation_GetPublicKey,
        KmsGrantOperation_ReEncryptFrom,
        KmsGrantOperation_ReEncryptTo,
        KmsGrantOperation_RetireGrant,
        KmsGrantOperation_Sign,
        KmsGrantOperation_Verify
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype KmsGrantOperation = KmsGrantOperation'
  { fromKmsGrantOperation ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern KmsGrantOperation_CreateGrant :: KmsGrantOperation
pattern KmsGrantOperation_CreateGrant = KmsGrantOperation' "CreateGrant"

pattern KmsGrantOperation_Decrypt :: KmsGrantOperation
pattern KmsGrantOperation_Decrypt = KmsGrantOperation' "Decrypt"

pattern KmsGrantOperation_DescribeKey :: KmsGrantOperation
pattern KmsGrantOperation_DescribeKey = KmsGrantOperation' "DescribeKey"

pattern KmsGrantOperation_Encrypt :: KmsGrantOperation
pattern KmsGrantOperation_Encrypt = KmsGrantOperation' "Encrypt"

pattern KmsGrantOperation_GenerateDataKey :: KmsGrantOperation
pattern KmsGrantOperation_GenerateDataKey = KmsGrantOperation' "GenerateDataKey"

pattern KmsGrantOperation_GenerateDataKeyPair :: KmsGrantOperation
pattern KmsGrantOperation_GenerateDataKeyPair = KmsGrantOperation' "GenerateDataKeyPair"

pattern KmsGrantOperation_GenerateDataKeyPairWithoutPlaintext :: KmsGrantOperation
pattern KmsGrantOperation_GenerateDataKeyPairWithoutPlaintext = KmsGrantOperation' "GenerateDataKeyPairWithoutPlaintext"

pattern KmsGrantOperation_GenerateDataKeyWithoutPlaintext :: KmsGrantOperation
pattern KmsGrantOperation_GenerateDataKeyWithoutPlaintext = KmsGrantOperation' "GenerateDataKeyWithoutPlaintext"

pattern KmsGrantOperation_GetPublicKey :: KmsGrantOperation
pattern KmsGrantOperation_GetPublicKey = KmsGrantOperation' "GetPublicKey"

pattern KmsGrantOperation_ReEncryptFrom :: KmsGrantOperation
pattern KmsGrantOperation_ReEncryptFrom = KmsGrantOperation' "ReEncryptFrom"

pattern KmsGrantOperation_ReEncryptTo :: KmsGrantOperation
pattern KmsGrantOperation_ReEncryptTo = KmsGrantOperation' "ReEncryptTo"

pattern KmsGrantOperation_RetireGrant :: KmsGrantOperation
pattern KmsGrantOperation_RetireGrant = KmsGrantOperation' "RetireGrant"

pattern KmsGrantOperation_Sign :: KmsGrantOperation
pattern KmsGrantOperation_Sign = KmsGrantOperation' "Sign"

pattern KmsGrantOperation_Verify :: KmsGrantOperation
pattern KmsGrantOperation_Verify = KmsGrantOperation' "Verify"

{-# COMPLETE
  KmsGrantOperation_CreateGrant,
  KmsGrantOperation_Decrypt,
  KmsGrantOperation_DescribeKey,
  KmsGrantOperation_Encrypt,
  KmsGrantOperation_GenerateDataKey,
  KmsGrantOperation_GenerateDataKeyPair,
  KmsGrantOperation_GenerateDataKeyPairWithoutPlaintext,
  KmsGrantOperation_GenerateDataKeyWithoutPlaintext,
  KmsGrantOperation_GetPublicKey,
  KmsGrantOperation_ReEncryptFrom,
  KmsGrantOperation_ReEncryptTo,
  KmsGrantOperation_RetireGrant,
  KmsGrantOperation_Sign,
  KmsGrantOperation_Verify,
  KmsGrantOperation'
  #-}
