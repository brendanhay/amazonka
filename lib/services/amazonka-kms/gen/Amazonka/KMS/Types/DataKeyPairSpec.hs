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
-- Module      : Amazonka.KMS.Types.DataKeyPairSpec
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KMS.Types.DataKeyPairSpec
  ( DataKeyPairSpec
      ( ..,
        DataKeyPairSpec_ECC_NIST_P256,
        DataKeyPairSpec_ECC_NIST_P384,
        DataKeyPairSpec_ECC_NIST_P521,
        DataKeyPairSpec_ECC_SECG_P256K1,
        DataKeyPairSpec_RSA_2048,
        DataKeyPairSpec_RSA_3072,
        DataKeyPairSpec_RSA_4096,
        DataKeyPairSpec_SM2
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DataKeyPairSpec = DataKeyPairSpec'
  { fromDataKeyPairSpec ::
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

pattern DataKeyPairSpec_ECC_NIST_P256 :: DataKeyPairSpec
pattern DataKeyPairSpec_ECC_NIST_P256 = DataKeyPairSpec' "ECC_NIST_P256"

pattern DataKeyPairSpec_ECC_NIST_P384 :: DataKeyPairSpec
pattern DataKeyPairSpec_ECC_NIST_P384 = DataKeyPairSpec' "ECC_NIST_P384"

pattern DataKeyPairSpec_ECC_NIST_P521 :: DataKeyPairSpec
pattern DataKeyPairSpec_ECC_NIST_P521 = DataKeyPairSpec' "ECC_NIST_P521"

pattern DataKeyPairSpec_ECC_SECG_P256K1 :: DataKeyPairSpec
pattern DataKeyPairSpec_ECC_SECG_P256K1 = DataKeyPairSpec' "ECC_SECG_P256K1"

pattern DataKeyPairSpec_RSA_2048 :: DataKeyPairSpec
pattern DataKeyPairSpec_RSA_2048 = DataKeyPairSpec' "RSA_2048"

pattern DataKeyPairSpec_RSA_3072 :: DataKeyPairSpec
pattern DataKeyPairSpec_RSA_3072 = DataKeyPairSpec' "RSA_3072"

pattern DataKeyPairSpec_RSA_4096 :: DataKeyPairSpec
pattern DataKeyPairSpec_RSA_4096 = DataKeyPairSpec' "RSA_4096"

pattern DataKeyPairSpec_SM2 :: DataKeyPairSpec
pattern DataKeyPairSpec_SM2 = DataKeyPairSpec' "SM2"

{-# COMPLETE
  DataKeyPairSpec_ECC_NIST_P256,
  DataKeyPairSpec_ECC_NIST_P384,
  DataKeyPairSpec_ECC_NIST_P521,
  DataKeyPairSpec_ECC_SECG_P256K1,
  DataKeyPairSpec_RSA_2048,
  DataKeyPairSpec_RSA_3072,
  DataKeyPairSpec_RSA_4096,
  DataKeyPairSpec_SM2,
  DataKeyPairSpec'
  #-}
