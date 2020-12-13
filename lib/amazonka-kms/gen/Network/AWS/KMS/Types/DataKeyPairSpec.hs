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
        Rsa2048,
        Rsa3072,
        Rsa4096,
        EccNistP256,
        EccNistP384,
        EccNistP521,
        EccSecgP256K1
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype DataKeyPairSpec = DataKeyPairSpec' Lude.Text
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

pattern Rsa2048 :: DataKeyPairSpec
pattern Rsa2048 = DataKeyPairSpec' "RSA_2048"

pattern Rsa3072 :: DataKeyPairSpec
pattern Rsa3072 = DataKeyPairSpec' "RSA_3072"

pattern Rsa4096 :: DataKeyPairSpec
pattern Rsa4096 = DataKeyPairSpec' "RSA_4096"

pattern EccNistP256 :: DataKeyPairSpec
pattern EccNistP256 = DataKeyPairSpec' "ECC_NIST_P256"

pattern EccNistP384 :: DataKeyPairSpec
pattern EccNistP384 = DataKeyPairSpec' "ECC_NIST_P384"

pattern EccNistP521 :: DataKeyPairSpec
pattern EccNistP521 = DataKeyPairSpec' "ECC_NIST_P521"

pattern EccSecgP256K1 :: DataKeyPairSpec
pattern EccSecgP256K1 = DataKeyPairSpec' "ECC_SECG_P256K1"

{-# COMPLETE
  Rsa2048,
  Rsa3072,
  Rsa4096,
  EccNistP256,
  EccNistP384,
  EccNistP521,
  EccSecgP256K1,
  DataKeyPairSpec'
  #-}
