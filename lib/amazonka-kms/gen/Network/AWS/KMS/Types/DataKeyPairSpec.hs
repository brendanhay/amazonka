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
        DKPSEccNistP256,
        DKPSEccNistP384,
        DKPSEccNistP521,
        DKPSEccSecgP256K1,
        DKPSRsa2048,
        DKPSRsa3072,
        DKPSRsa4096
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

pattern DKPSEccNistP256 :: DataKeyPairSpec
pattern DKPSEccNistP256 = DataKeyPairSpec' "ECC_NIST_P256"

pattern DKPSEccNistP384 :: DataKeyPairSpec
pattern DKPSEccNistP384 = DataKeyPairSpec' "ECC_NIST_P384"

pattern DKPSEccNistP521 :: DataKeyPairSpec
pattern DKPSEccNistP521 = DataKeyPairSpec' "ECC_NIST_P521"

pattern DKPSEccSecgP256K1 :: DataKeyPairSpec
pattern DKPSEccSecgP256K1 = DataKeyPairSpec' "ECC_SECG_P256K1"

pattern DKPSRsa2048 :: DataKeyPairSpec
pattern DKPSRsa2048 = DataKeyPairSpec' "RSA_2048"

pattern DKPSRsa3072 :: DataKeyPairSpec
pattern DKPSRsa3072 = DataKeyPairSpec' "RSA_3072"

pattern DKPSRsa4096 :: DataKeyPairSpec
pattern DKPSRsa4096 = DataKeyPairSpec' "RSA_4096"

{-# COMPLETE
  DKPSEccNistP256,
  DKPSEccNistP384,
  DKPSEccNistP521,
  DKPSEccSecgP256K1,
  DKPSRsa2048,
  DKPSRsa3072,
  DKPSRsa4096,
  DataKeyPairSpec'
  #-}
