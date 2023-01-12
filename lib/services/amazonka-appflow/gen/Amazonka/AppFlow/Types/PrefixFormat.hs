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
-- Module      : Amazonka.AppFlow.Types.PrefixFormat
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.PrefixFormat
  ( PrefixFormat
      ( ..,
        PrefixFormat_DAY,
        PrefixFormat_HOUR,
        PrefixFormat_MINUTE,
        PrefixFormat_MONTH,
        PrefixFormat_YEAR
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PrefixFormat = PrefixFormat'
  { fromPrefixFormat ::
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

pattern PrefixFormat_DAY :: PrefixFormat
pattern PrefixFormat_DAY = PrefixFormat' "DAY"

pattern PrefixFormat_HOUR :: PrefixFormat
pattern PrefixFormat_HOUR = PrefixFormat' "HOUR"

pattern PrefixFormat_MINUTE :: PrefixFormat
pattern PrefixFormat_MINUTE = PrefixFormat' "MINUTE"

pattern PrefixFormat_MONTH :: PrefixFormat
pattern PrefixFormat_MONTH = PrefixFormat' "MONTH"

pattern PrefixFormat_YEAR :: PrefixFormat
pattern PrefixFormat_YEAR = PrefixFormat' "YEAR"

{-# COMPLETE
  PrefixFormat_DAY,
  PrefixFormat_HOUR,
  PrefixFormat_MINUTE,
  PrefixFormat_MONTH,
  PrefixFormat_YEAR,
  PrefixFormat'
  #-}
