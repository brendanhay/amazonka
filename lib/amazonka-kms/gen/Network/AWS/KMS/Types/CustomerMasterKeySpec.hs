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
        CustomerMasterKeySpecRsa2048,
        CustomerMasterKeySpecRsa3072,
        CustomerMasterKeySpecRsa4096,
        CustomerMasterKeySpecEccNistP256,
        CustomerMasterKeySpecEccNistP384,
        CustomerMasterKeySpecEccNistP521,
        CustomerMasterKeySpecEccSecgP256K1,
        CustomerMasterKeySpecSymmetricDefault,
        fromCustomerMasterKeySpec
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype CustomerMasterKeySpec = CustomerMasterKeySpec'
  { fromCustomerMasterKeySpec ::
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

pattern CustomerMasterKeySpecRsa2048 :: CustomerMasterKeySpec
pattern CustomerMasterKeySpecRsa2048 = CustomerMasterKeySpec' "RSA_2048"

pattern CustomerMasterKeySpecRsa3072 :: CustomerMasterKeySpec
pattern CustomerMasterKeySpecRsa3072 = CustomerMasterKeySpec' "RSA_3072"

pattern CustomerMasterKeySpecRsa4096 :: CustomerMasterKeySpec
pattern CustomerMasterKeySpecRsa4096 = CustomerMasterKeySpec' "RSA_4096"

pattern CustomerMasterKeySpecEccNistP256 :: CustomerMasterKeySpec
pattern CustomerMasterKeySpecEccNistP256 = CustomerMasterKeySpec' "ECC_NIST_P256"

pattern CustomerMasterKeySpecEccNistP384 :: CustomerMasterKeySpec
pattern CustomerMasterKeySpecEccNistP384 = CustomerMasterKeySpec' "ECC_NIST_P384"

pattern CustomerMasterKeySpecEccNistP521 :: CustomerMasterKeySpec
pattern CustomerMasterKeySpecEccNistP521 = CustomerMasterKeySpec' "ECC_NIST_P521"

pattern CustomerMasterKeySpecEccSecgP256K1 :: CustomerMasterKeySpec
pattern CustomerMasterKeySpecEccSecgP256K1 = CustomerMasterKeySpec' "ECC_SECG_P256K1"

pattern CustomerMasterKeySpecSymmetricDefault :: CustomerMasterKeySpec
pattern CustomerMasterKeySpecSymmetricDefault = CustomerMasterKeySpec' "SYMMETRIC_DEFAULT"

{-# COMPLETE
  CustomerMasterKeySpecRsa2048,
  CustomerMasterKeySpecRsa3072,
  CustomerMasterKeySpecRsa4096,
  CustomerMasterKeySpecEccNistP256,
  CustomerMasterKeySpecEccNistP384,
  CustomerMasterKeySpecEccNistP521,
  CustomerMasterKeySpecEccSecgP256K1,
  CustomerMasterKeySpecSymmetricDefault,
  CustomerMasterKeySpec'
  #-}
