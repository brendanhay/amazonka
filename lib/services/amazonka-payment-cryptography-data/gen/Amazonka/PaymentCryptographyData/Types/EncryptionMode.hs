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
-- Module      : Amazonka.PaymentCryptographyData.Types.EncryptionMode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptographyData.Types.EncryptionMode
  ( EncryptionMode
      ( ..,
        EncryptionMode_CBC,
        EncryptionMode_CFB,
        EncryptionMode_CFB1,
        EncryptionMode_CFB128,
        EncryptionMode_CFB64,
        EncryptionMode_CFB8,
        EncryptionMode_ECB,
        EncryptionMode_OFB
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EncryptionMode = EncryptionMode'
  { fromEncryptionMode ::
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

pattern EncryptionMode_CBC :: EncryptionMode
pattern EncryptionMode_CBC = EncryptionMode' "CBC"

pattern EncryptionMode_CFB :: EncryptionMode
pattern EncryptionMode_CFB = EncryptionMode' "CFB"

pattern EncryptionMode_CFB1 :: EncryptionMode
pattern EncryptionMode_CFB1 = EncryptionMode' "CFB1"

pattern EncryptionMode_CFB128 :: EncryptionMode
pattern EncryptionMode_CFB128 = EncryptionMode' "CFB128"

pattern EncryptionMode_CFB64 :: EncryptionMode
pattern EncryptionMode_CFB64 = EncryptionMode' "CFB64"

pattern EncryptionMode_CFB8 :: EncryptionMode
pattern EncryptionMode_CFB8 = EncryptionMode' "CFB8"

pattern EncryptionMode_ECB :: EncryptionMode
pattern EncryptionMode_ECB = EncryptionMode' "ECB"

pattern EncryptionMode_OFB :: EncryptionMode
pattern EncryptionMode_OFB = EncryptionMode' "OFB"

{-# COMPLETE
  EncryptionMode_CBC,
  EncryptionMode_CFB,
  EncryptionMode_CFB1,
  EncryptionMode_CFB128,
  EncryptionMode_CFB64,
  EncryptionMode_CFB8,
  EncryptionMode_ECB,
  EncryptionMode_OFB,
  EncryptionMode'
  #-}
