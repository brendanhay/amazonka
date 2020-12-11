-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.KeyUsageType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.KeyUsageType
  ( KeyUsageType
      ( KeyUsageType',
        EncryptDecrypt,
        SignVerify
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype KeyUsageType = KeyUsageType' Lude.Text
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

pattern EncryptDecrypt :: KeyUsageType
pattern EncryptDecrypt = KeyUsageType' "ENCRYPT_DECRYPT"

pattern SignVerify :: KeyUsageType
pattern SignVerify = KeyUsageType' "SIGN_VERIFY"

{-# COMPLETE
  EncryptDecrypt,
  SignVerify,
  KeyUsageType'
  #-}
