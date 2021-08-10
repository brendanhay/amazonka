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
-- Module      : Network.AWS.KMS.Types.DataKeyPairSpec
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.DataKeyPairSpec
  ( DataKeyPairSpec
      ( ..,
        DataKeyPairSpec_ECC_NIST_P256,
        DataKeyPairSpec_ECC_NIST_P384,
        DataKeyPairSpec_ECC_NIST_P521,
        DataKeyPairSpec_ECC_SECG_P256K1,
        DataKeyPairSpec_RSA_2048,
        DataKeyPairSpec_RSA_3072,
        DataKeyPairSpec_RSA_4096
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype DataKeyPairSpec = DataKeyPairSpec'
  { fromDataKeyPairSpec ::
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

{-# COMPLETE
  DataKeyPairSpec_ECC_NIST_P256,
  DataKeyPairSpec_ECC_NIST_P384,
  DataKeyPairSpec_ECC_NIST_P521,
  DataKeyPairSpec_ECC_SECG_P256K1,
  DataKeyPairSpec_RSA_2048,
  DataKeyPairSpec_RSA_3072,
  DataKeyPairSpec_RSA_4096,
  DataKeyPairSpec'
  #-}
