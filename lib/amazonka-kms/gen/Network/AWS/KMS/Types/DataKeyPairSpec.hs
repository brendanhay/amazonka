{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.DataKeyPairSpec
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.DataKeyPairSpec
  ( DataKeyPairSpec
      ( DataKeyPairSpec',
        DataKeyPairSpecRsa2048,
        DataKeyPairSpecRsa3072,
        DataKeyPairSpecRsa4096,
        DataKeyPairSpecEccNistP256,
        DataKeyPairSpecEccNistP384,
        DataKeyPairSpecEccNistP521,
        DataKeyPairSpecEccSecgP256K1,
        fromDataKeyPairSpec
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype DataKeyPairSpec = DataKeyPairSpec'
  { fromDataKeyPairSpec ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern DataKeyPairSpecRsa2048 :: DataKeyPairSpec
pattern DataKeyPairSpecRsa2048 = DataKeyPairSpec' "RSA_2048"

pattern DataKeyPairSpecRsa3072 :: DataKeyPairSpec
pattern DataKeyPairSpecRsa3072 = DataKeyPairSpec' "RSA_3072"

pattern DataKeyPairSpecRsa4096 :: DataKeyPairSpec
pattern DataKeyPairSpecRsa4096 = DataKeyPairSpec' "RSA_4096"

pattern DataKeyPairSpecEccNistP256 :: DataKeyPairSpec
pattern DataKeyPairSpecEccNistP256 = DataKeyPairSpec' "ECC_NIST_P256"

pattern DataKeyPairSpecEccNistP384 :: DataKeyPairSpec
pattern DataKeyPairSpecEccNistP384 = DataKeyPairSpec' "ECC_NIST_P384"

pattern DataKeyPairSpecEccNistP521 :: DataKeyPairSpec
pattern DataKeyPairSpecEccNistP521 = DataKeyPairSpec' "ECC_NIST_P521"

pattern DataKeyPairSpecEccSecgP256K1 :: DataKeyPairSpec
pattern DataKeyPairSpecEccSecgP256K1 = DataKeyPairSpec' "ECC_SECG_P256K1"

{-# COMPLETE
  DataKeyPairSpecRsa2048,
  DataKeyPairSpecRsa3072,
  DataKeyPairSpecRsa4096,
  DataKeyPairSpecEccNistP256,
  DataKeyPairSpecEccNistP384,
  DataKeyPairSpecEccNistP521,
  DataKeyPairSpecEccSecgP256K1,
  DataKeyPairSpec'
  #-}
