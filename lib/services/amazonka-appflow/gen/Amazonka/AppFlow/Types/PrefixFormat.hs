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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
import qualified Amazonka.Prelude as Prelude

newtype PrefixFormat = PrefixFormat'
  { fromPrefixFormat ::
      Core.Text
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
