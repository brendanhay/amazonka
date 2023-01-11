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
-- Module      : Amazonka.Transfer.Types.EncryptionAlg
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.EncryptionAlg
  ( EncryptionAlg
      ( ..,
        EncryptionAlg_AES128_CBC,
        EncryptionAlg_AES192_CBC,
        EncryptionAlg_AES256_CBC,
        EncryptionAlg_NONE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EncryptionAlg = EncryptionAlg'
  { fromEncryptionAlg ::
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

pattern EncryptionAlg_AES128_CBC :: EncryptionAlg
pattern EncryptionAlg_AES128_CBC = EncryptionAlg' "AES128_CBC"

pattern EncryptionAlg_AES192_CBC :: EncryptionAlg
pattern EncryptionAlg_AES192_CBC = EncryptionAlg' "AES192_CBC"

pattern EncryptionAlg_AES256_CBC :: EncryptionAlg
pattern EncryptionAlg_AES256_CBC = EncryptionAlg' "AES256_CBC"

pattern EncryptionAlg_NONE :: EncryptionAlg
pattern EncryptionAlg_NONE = EncryptionAlg' "NONE"

{-# COMPLETE
  EncryptionAlg_AES128_CBC,
  EncryptionAlg_AES192_CBC,
  EncryptionAlg_AES256_CBC,
  EncryptionAlg_NONE,
  EncryptionAlg'
  #-}
