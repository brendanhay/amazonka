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
-- Module      : Amazonka.QuickSight.Types.DisplayFormat
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DisplayFormat
  ( DisplayFormat
      ( ..,
        DisplayFormat_AUTO,
        DisplayFormat_CURRENCY,
        DisplayFormat_DATE,
        DisplayFormat_NUMBER,
        DisplayFormat_PERCENT,
        DisplayFormat_STRING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DisplayFormat = DisplayFormat'
  { fromDisplayFormat ::
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

pattern DisplayFormat_AUTO :: DisplayFormat
pattern DisplayFormat_AUTO = DisplayFormat' "AUTO"

pattern DisplayFormat_CURRENCY :: DisplayFormat
pattern DisplayFormat_CURRENCY = DisplayFormat' "CURRENCY"

pattern DisplayFormat_DATE :: DisplayFormat
pattern DisplayFormat_DATE = DisplayFormat' "DATE"

pattern DisplayFormat_NUMBER :: DisplayFormat
pattern DisplayFormat_NUMBER = DisplayFormat' "NUMBER"

pattern DisplayFormat_PERCENT :: DisplayFormat
pattern DisplayFormat_PERCENT = DisplayFormat' "PERCENT"

pattern DisplayFormat_STRING :: DisplayFormat
pattern DisplayFormat_STRING = DisplayFormat' "STRING"

{-# COMPLETE
  DisplayFormat_AUTO,
  DisplayFormat_CURRENCY,
  DisplayFormat_DATE,
  DisplayFormat_NUMBER,
  DisplayFormat_PERCENT,
  DisplayFormat_STRING,
  DisplayFormat'
  #-}
