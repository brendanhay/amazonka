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
-- Module      : Amazonka.Glue.Types.MLUserDataEncryptionModeString
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.MLUserDataEncryptionModeString
  ( MLUserDataEncryptionModeString
      ( ..,
        MLUserDataEncryptionModeString_DISABLED,
        MLUserDataEncryptionModeString_SSE_KMS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype MLUserDataEncryptionModeString = MLUserDataEncryptionModeString'
  { fromMLUserDataEncryptionModeString ::
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

pattern MLUserDataEncryptionModeString_DISABLED :: MLUserDataEncryptionModeString
pattern MLUserDataEncryptionModeString_DISABLED = MLUserDataEncryptionModeString' "DISABLED"

pattern MLUserDataEncryptionModeString_SSE_KMS :: MLUserDataEncryptionModeString
pattern MLUserDataEncryptionModeString_SSE_KMS = MLUserDataEncryptionModeString' "SSE-KMS"

{-# COMPLETE
  MLUserDataEncryptionModeString_DISABLED,
  MLUserDataEncryptionModeString_SSE_KMS,
  MLUserDataEncryptionModeString'
  #-}
