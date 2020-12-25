{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        KeyUsageTypeSignVerify,
        KeyUsageTypeEncryptDecrypt,
        fromKeyUsageType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype KeyUsageType = KeyUsageType' {fromKeyUsageType :: Core.Text}
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

pattern KeyUsageTypeSignVerify :: KeyUsageType
pattern KeyUsageTypeSignVerify = KeyUsageType' "SIGN_VERIFY"

pattern KeyUsageTypeEncryptDecrypt :: KeyUsageType
pattern KeyUsageTypeEncryptDecrypt = KeyUsageType' "ENCRYPT_DECRYPT"

{-# COMPLETE
  KeyUsageTypeSignVerify,
  KeyUsageTypeEncryptDecrypt,
  KeyUsageType'
  #-}
