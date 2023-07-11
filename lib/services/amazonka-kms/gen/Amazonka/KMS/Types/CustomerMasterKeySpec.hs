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
-- Module      : Amazonka.KMS.Types.CustomerMasterKeySpec
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KMS.Types.CustomerMasterKeySpec
  ( CustomerMasterKeySpec
      ( ..,
        CustomerMasterKeySpec_ECC_NIST_P256,
        CustomerMasterKeySpec_ECC_NIST_P384,
        CustomerMasterKeySpec_ECC_NIST_P521,
        CustomerMasterKeySpec_ECC_SECG_P256K1,
        CustomerMasterKeySpec_HMAC_224,
        CustomerMasterKeySpec_HMAC_256,
        CustomerMasterKeySpec_HMAC_384,
        CustomerMasterKeySpec_HMAC_512,
        CustomerMasterKeySpec_RSA_2048,
        CustomerMasterKeySpec_RSA_3072,
        CustomerMasterKeySpec_RSA_4096,
        CustomerMasterKeySpec_SM2,
        CustomerMasterKeySpec_SYMMETRIC_DEFAULT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CustomerMasterKeySpec = CustomerMasterKeySpec'
  { fromCustomerMasterKeySpec ::
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

pattern CustomerMasterKeySpec_ECC_NIST_P256 :: CustomerMasterKeySpec
pattern CustomerMasterKeySpec_ECC_NIST_P256 = CustomerMasterKeySpec' "ECC_NIST_P256"

pattern CustomerMasterKeySpec_ECC_NIST_P384 :: CustomerMasterKeySpec
pattern CustomerMasterKeySpec_ECC_NIST_P384 = CustomerMasterKeySpec' "ECC_NIST_P384"

pattern CustomerMasterKeySpec_ECC_NIST_P521 :: CustomerMasterKeySpec
pattern CustomerMasterKeySpec_ECC_NIST_P521 = CustomerMasterKeySpec' "ECC_NIST_P521"

pattern CustomerMasterKeySpec_ECC_SECG_P256K1 :: CustomerMasterKeySpec
pattern CustomerMasterKeySpec_ECC_SECG_P256K1 = CustomerMasterKeySpec' "ECC_SECG_P256K1"

pattern CustomerMasterKeySpec_HMAC_224 :: CustomerMasterKeySpec
pattern CustomerMasterKeySpec_HMAC_224 = CustomerMasterKeySpec' "HMAC_224"

pattern CustomerMasterKeySpec_HMAC_256 :: CustomerMasterKeySpec
pattern CustomerMasterKeySpec_HMAC_256 = CustomerMasterKeySpec' "HMAC_256"

pattern CustomerMasterKeySpec_HMAC_384 :: CustomerMasterKeySpec
pattern CustomerMasterKeySpec_HMAC_384 = CustomerMasterKeySpec' "HMAC_384"

pattern CustomerMasterKeySpec_HMAC_512 :: CustomerMasterKeySpec
pattern CustomerMasterKeySpec_HMAC_512 = CustomerMasterKeySpec' "HMAC_512"

pattern CustomerMasterKeySpec_RSA_2048 :: CustomerMasterKeySpec
pattern CustomerMasterKeySpec_RSA_2048 = CustomerMasterKeySpec' "RSA_2048"

pattern CustomerMasterKeySpec_RSA_3072 :: CustomerMasterKeySpec
pattern CustomerMasterKeySpec_RSA_3072 = CustomerMasterKeySpec' "RSA_3072"

pattern CustomerMasterKeySpec_RSA_4096 :: CustomerMasterKeySpec
pattern CustomerMasterKeySpec_RSA_4096 = CustomerMasterKeySpec' "RSA_4096"

pattern CustomerMasterKeySpec_SM2 :: CustomerMasterKeySpec
pattern CustomerMasterKeySpec_SM2 = CustomerMasterKeySpec' "SM2"

pattern CustomerMasterKeySpec_SYMMETRIC_DEFAULT :: CustomerMasterKeySpec
pattern CustomerMasterKeySpec_SYMMETRIC_DEFAULT = CustomerMasterKeySpec' "SYMMETRIC_DEFAULT"

{-# COMPLETE
  CustomerMasterKeySpec_ECC_NIST_P256,
  CustomerMasterKeySpec_ECC_NIST_P384,
  CustomerMasterKeySpec_ECC_NIST_P521,
  CustomerMasterKeySpec_ECC_SECG_P256K1,
  CustomerMasterKeySpec_HMAC_224,
  CustomerMasterKeySpec_HMAC_256,
  CustomerMasterKeySpec_HMAC_384,
  CustomerMasterKeySpec_HMAC_512,
  CustomerMasterKeySpec_RSA_2048,
  CustomerMasterKeySpec_RSA_3072,
  CustomerMasterKeySpec_RSA_4096,
  CustomerMasterKeySpec_SM2,
  CustomerMasterKeySpec_SYMMETRIC_DEFAULT,
  CustomerMasterKeySpec'
  #-}
