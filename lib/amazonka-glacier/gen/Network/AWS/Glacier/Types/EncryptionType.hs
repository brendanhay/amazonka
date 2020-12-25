{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.EncryptionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.EncryptionType
  ( EncryptionType
      ( EncryptionType',
        EncryptionTypeAwsKms,
        EncryptionTypeAES256,
        fromEncryptionType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype EncryptionType = EncryptionType'
  { fromEncryptionType ::
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

pattern EncryptionTypeAwsKms :: EncryptionType
pattern EncryptionTypeAwsKms = EncryptionType' "aws:kms"

pattern EncryptionTypeAES256 :: EncryptionType
pattern EncryptionTypeAES256 = EncryptionType' "AES256"

{-# COMPLETE
  EncryptionTypeAwsKms,
  EncryptionTypeAES256,
  EncryptionType'
  #-}
