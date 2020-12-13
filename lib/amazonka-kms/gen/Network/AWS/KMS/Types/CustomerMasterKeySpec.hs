{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.CustomerMasterKeySpec
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.CustomerMasterKeySpec
  ( CustomerMasterKeySpec
      ( CustomerMasterKeySpec',
        CMKSRsa2048,
        CMKSRsa3072,
        CMKSRsa4096,
        CMKSEccNistP256,
        CMKSEccNistP384,
        CMKSEccNistP521,
        CMKSEccSecgP256K1,
        CMKSSymmetricDefault
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype CustomerMasterKeySpec = CustomerMasterKeySpec' Lude.Text
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

pattern CMKSRsa2048 :: CustomerMasterKeySpec
pattern CMKSRsa2048 = CustomerMasterKeySpec' "RSA_2048"

pattern CMKSRsa3072 :: CustomerMasterKeySpec
pattern CMKSRsa3072 = CustomerMasterKeySpec' "RSA_3072"

pattern CMKSRsa4096 :: CustomerMasterKeySpec
pattern CMKSRsa4096 = CustomerMasterKeySpec' "RSA_4096"

pattern CMKSEccNistP256 :: CustomerMasterKeySpec
pattern CMKSEccNistP256 = CustomerMasterKeySpec' "ECC_NIST_P256"

pattern CMKSEccNistP384 :: CustomerMasterKeySpec
pattern CMKSEccNistP384 = CustomerMasterKeySpec' "ECC_NIST_P384"

pattern CMKSEccNistP521 :: CustomerMasterKeySpec
pattern CMKSEccNistP521 = CustomerMasterKeySpec' "ECC_NIST_P521"

pattern CMKSEccSecgP256K1 :: CustomerMasterKeySpec
pattern CMKSEccSecgP256K1 = CustomerMasterKeySpec' "ECC_SECG_P256K1"

pattern CMKSSymmetricDefault :: CustomerMasterKeySpec
pattern CMKSSymmetricDefault = CustomerMasterKeySpec' "SYMMETRIC_DEFAULT"

{-# COMPLETE
  CMKSRsa2048,
  CMKSRsa3072,
  CMKSRsa4096,
  CMKSEccNistP256,
  CMKSEccNistP384,
  CMKSEccNistP521,
  CMKSEccSecgP256K1,
  CMKSSymmetricDefault,
  CustomerMasterKeySpec'
  #-}
