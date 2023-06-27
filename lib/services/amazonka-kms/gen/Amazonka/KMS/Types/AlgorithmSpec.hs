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
-- Module      : Amazonka.KMS.Types.AlgorithmSpec
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KMS.Types.AlgorithmSpec
  ( AlgorithmSpec
      ( ..,
        AlgorithmSpec_RSAES_OAEP_SHA_1,
        AlgorithmSpec_RSAES_OAEP_SHA_256,
        AlgorithmSpec_RSAES_PKCS1_V1_5,
        AlgorithmSpec_RSA_AES_KEY_WRAP_SHA_1,
        AlgorithmSpec_RSA_AES_KEY_WRAP_SHA_256
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AlgorithmSpec = AlgorithmSpec'
  { fromAlgorithmSpec ::
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

pattern AlgorithmSpec_RSAES_OAEP_SHA_1 :: AlgorithmSpec
pattern AlgorithmSpec_RSAES_OAEP_SHA_1 = AlgorithmSpec' "RSAES_OAEP_SHA_1"

pattern AlgorithmSpec_RSAES_OAEP_SHA_256 :: AlgorithmSpec
pattern AlgorithmSpec_RSAES_OAEP_SHA_256 = AlgorithmSpec' "RSAES_OAEP_SHA_256"

pattern AlgorithmSpec_RSAES_PKCS1_V1_5 :: AlgorithmSpec
pattern AlgorithmSpec_RSAES_PKCS1_V1_5 = AlgorithmSpec' "RSAES_PKCS1_V1_5"

pattern AlgorithmSpec_RSA_AES_KEY_WRAP_SHA_1 :: AlgorithmSpec
pattern AlgorithmSpec_RSA_AES_KEY_WRAP_SHA_1 = AlgorithmSpec' "RSA_AES_KEY_WRAP_SHA_1"

pattern AlgorithmSpec_RSA_AES_KEY_WRAP_SHA_256 :: AlgorithmSpec
pattern AlgorithmSpec_RSA_AES_KEY_WRAP_SHA_256 = AlgorithmSpec' "RSA_AES_KEY_WRAP_SHA_256"

{-# COMPLETE
  AlgorithmSpec_RSAES_OAEP_SHA_1,
  AlgorithmSpec_RSAES_OAEP_SHA_256,
  AlgorithmSpec_RSAES_PKCS1_V1_5,
  AlgorithmSpec_RSA_AES_KEY_WRAP_SHA_1,
  AlgorithmSpec_RSA_AES_KEY_WRAP_SHA_256,
  AlgorithmSpec'
  #-}
