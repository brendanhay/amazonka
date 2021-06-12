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
-- Module      : Network.AWS.Glue.Types.CsvHeaderOption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CsvHeaderOption
  ( CsvHeaderOption
      ( ..,
        CsvHeaderOption_ABSENT,
        CsvHeaderOption_PRESENT,
        CsvHeaderOption_UNKNOWN
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype CsvHeaderOption = CsvHeaderOption'
  { fromCsvHeaderOption ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern CsvHeaderOption_ABSENT :: CsvHeaderOption
pattern CsvHeaderOption_ABSENT = CsvHeaderOption' "ABSENT"

pattern CsvHeaderOption_PRESENT :: CsvHeaderOption
pattern CsvHeaderOption_PRESENT = CsvHeaderOption' "PRESENT"

pattern CsvHeaderOption_UNKNOWN :: CsvHeaderOption
pattern CsvHeaderOption_UNKNOWN = CsvHeaderOption' "UNKNOWN"

{-# COMPLETE
  CsvHeaderOption_ABSENT,
  CsvHeaderOption_PRESENT,
  CsvHeaderOption_UNKNOWN,
  CsvHeaderOption'
  #-}
