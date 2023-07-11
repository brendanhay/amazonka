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
-- Module      : Amazonka.Omics.Types.FormatToHeaderKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.FormatToHeaderKey
  ( FormatToHeaderKey
      ( ..,
        FormatToHeaderKey_ALT,
        FormatToHeaderKey_CHR,
        FormatToHeaderKey_END,
        FormatToHeaderKey_POS,
        FormatToHeaderKey_REF,
        FormatToHeaderKey_START
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FormatToHeaderKey = FormatToHeaderKey'
  { fromFormatToHeaderKey ::
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

pattern FormatToHeaderKey_ALT :: FormatToHeaderKey
pattern FormatToHeaderKey_ALT = FormatToHeaderKey' "ALT"

pattern FormatToHeaderKey_CHR :: FormatToHeaderKey
pattern FormatToHeaderKey_CHR = FormatToHeaderKey' "CHR"

pattern FormatToHeaderKey_END :: FormatToHeaderKey
pattern FormatToHeaderKey_END = FormatToHeaderKey' "END"

pattern FormatToHeaderKey_POS :: FormatToHeaderKey
pattern FormatToHeaderKey_POS = FormatToHeaderKey' "POS"

pattern FormatToHeaderKey_REF :: FormatToHeaderKey
pattern FormatToHeaderKey_REF = FormatToHeaderKey' "REF"

pattern FormatToHeaderKey_START :: FormatToHeaderKey
pattern FormatToHeaderKey_START = FormatToHeaderKey' "START"

{-# COMPLETE
  FormatToHeaderKey_ALT,
  FormatToHeaderKey_CHR,
  FormatToHeaderKey_END,
  FormatToHeaderKey_POS,
  FormatToHeaderKey_REF,
  FormatToHeaderKey_START,
  FormatToHeaderKey'
  #-}
