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
-- Module      : Amazonka.KMS.Types.EncryptionAlgorithmSpec
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KMS.Types.EncryptionAlgorithmSpec
  ( EncryptionAlgorithmSpec
      ( ..,
        EncryptionAlgorithmSpec_RSAES_OAEP_SHA_1,
        EncryptionAlgorithmSpec_RSAES_OAEP_SHA_256,
        EncryptionAlgorithmSpec_SM2PKE,
        EncryptionAlgorithmSpec_SYMMETRIC_DEFAULT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EncryptionAlgorithmSpec = EncryptionAlgorithmSpec'
  { fromEncryptionAlgorithmSpec ::
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

pattern EncryptionAlgorithmSpec_RSAES_OAEP_SHA_1 :: EncryptionAlgorithmSpec
pattern EncryptionAlgorithmSpec_RSAES_OAEP_SHA_1 = EncryptionAlgorithmSpec' "RSAES_OAEP_SHA_1"

pattern EncryptionAlgorithmSpec_RSAES_OAEP_SHA_256 :: EncryptionAlgorithmSpec
pattern EncryptionAlgorithmSpec_RSAES_OAEP_SHA_256 = EncryptionAlgorithmSpec' "RSAES_OAEP_SHA_256"

pattern EncryptionAlgorithmSpec_SM2PKE :: EncryptionAlgorithmSpec
pattern EncryptionAlgorithmSpec_SM2PKE = EncryptionAlgorithmSpec' "SM2PKE"

pattern EncryptionAlgorithmSpec_SYMMETRIC_DEFAULT :: EncryptionAlgorithmSpec
pattern EncryptionAlgorithmSpec_SYMMETRIC_DEFAULT = EncryptionAlgorithmSpec' "SYMMETRIC_DEFAULT"

{-# COMPLETE
  EncryptionAlgorithmSpec_RSAES_OAEP_SHA_1,
  EncryptionAlgorithmSpec_RSAES_OAEP_SHA_256,
  EncryptionAlgorithmSpec_SM2PKE,
  EncryptionAlgorithmSpec_SYMMETRIC_DEFAULT,
  EncryptionAlgorithmSpec'
  #-}
