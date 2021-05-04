{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype CsvHeaderOption = CsvHeaderOption'
  { fromCsvHeaderOption ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
