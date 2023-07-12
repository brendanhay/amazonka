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
-- Module      : Amazonka.Athena.Types.EncryptionOption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.EncryptionOption
  ( EncryptionOption
      ( ..,
        EncryptionOption_CSE_KMS,
        EncryptionOption_SSE_KMS,
        EncryptionOption_SSE_S3
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EncryptionOption = EncryptionOption'
  { fromEncryptionOption ::
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

pattern EncryptionOption_CSE_KMS :: EncryptionOption
pattern EncryptionOption_CSE_KMS = EncryptionOption' "CSE_KMS"

pattern EncryptionOption_SSE_KMS :: EncryptionOption
pattern EncryptionOption_SSE_KMS = EncryptionOption' "SSE_KMS"

pattern EncryptionOption_SSE_S3 :: EncryptionOption
pattern EncryptionOption_SSE_S3 = EncryptionOption' "SSE_S3"

{-# COMPLETE
  EncryptionOption_CSE_KMS,
  EncryptionOption_SSE_KMS,
  EncryptionOption_SSE_S3,
  EncryptionOption'
  #-}
