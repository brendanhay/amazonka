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
-- Module      : Amazonka.QuickSight.Types.PaperSize
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.PaperSize
  ( PaperSize
      ( ..,
        PaperSize_A0,
        PaperSize_A1,
        PaperSize_A2,
        PaperSize_A3,
        PaperSize_A4,
        PaperSize_A5,
        PaperSize_JIS_B4,
        PaperSize_JIS_B5,
        PaperSize_US_LEGAL,
        PaperSize_US_LETTER,
        PaperSize_US_TABLOID_LEDGER
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PaperSize = PaperSize'
  { fromPaperSize ::
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

pattern PaperSize_A0 :: PaperSize
pattern PaperSize_A0 = PaperSize' "A0"

pattern PaperSize_A1 :: PaperSize
pattern PaperSize_A1 = PaperSize' "A1"

pattern PaperSize_A2 :: PaperSize
pattern PaperSize_A2 = PaperSize' "A2"

pattern PaperSize_A3 :: PaperSize
pattern PaperSize_A3 = PaperSize' "A3"

pattern PaperSize_A4 :: PaperSize
pattern PaperSize_A4 = PaperSize' "A4"

pattern PaperSize_A5 :: PaperSize
pattern PaperSize_A5 = PaperSize' "A5"

pattern PaperSize_JIS_B4 :: PaperSize
pattern PaperSize_JIS_B4 = PaperSize' "JIS_B4"

pattern PaperSize_JIS_B5 :: PaperSize
pattern PaperSize_JIS_B5 = PaperSize' "JIS_B5"

pattern PaperSize_US_LEGAL :: PaperSize
pattern PaperSize_US_LEGAL = PaperSize' "US_LEGAL"

pattern PaperSize_US_LETTER :: PaperSize
pattern PaperSize_US_LETTER = PaperSize' "US_LETTER"

pattern PaperSize_US_TABLOID_LEDGER :: PaperSize
pattern PaperSize_US_TABLOID_LEDGER = PaperSize' "US_TABLOID_LEDGER"

{-# COMPLETE
  PaperSize_A0,
  PaperSize_A1,
  PaperSize_A2,
  PaperSize_A3,
  PaperSize_A4,
  PaperSize_A5,
  PaperSize_JIS_B4,
  PaperSize_JIS_B5,
  PaperSize_US_LEGAL,
  PaperSize_US_LETTER,
  PaperSize_US_TABLOID_LEDGER,
  PaperSize'
  #-}
