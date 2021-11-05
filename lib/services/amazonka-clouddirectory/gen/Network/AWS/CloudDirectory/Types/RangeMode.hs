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
-- Module      : Amazonka.CloudDirectory.Types.RangeMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.RangeMode
  ( RangeMode
      ( ..,
        RangeMode_EXCLUSIVE,
        RangeMode_FIRST,
        RangeMode_INCLUSIVE,
        RangeMode_LAST,
        RangeMode_LAST_BEFORE_MISSING_VALUES
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype RangeMode = RangeMode'
  { fromRangeMode ::
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

pattern RangeMode_EXCLUSIVE :: RangeMode
pattern RangeMode_EXCLUSIVE = RangeMode' "EXCLUSIVE"

pattern RangeMode_FIRST :: RangeMode
pattern RangeMode_FIRST = RangeMode' "FIRST"

pattern RangeMode_INCLUSIVE :: RangeMode
pattern RangeMode_INCLUSIVE = RangeMode' "INCLUSIVE"

pattern RangeMode_LAST :: RangeMode
pattern RangeMode_LAST = RangeMode' "LAST"

pattern RangeMode_LAST_BEFORE_MISSING_VALUES :: RangeMode
pattern RangeMode_LAST_BEFORE_MISSING_VALUES = RangeMode' "LAST_BEFORE_MISSING_VALUES"

{-# COMPLETE
  RangeMode_EXCLUSIVE,
  RangeMode_FIRST,
  RangeMode_INCLUSIVE,
  RangeMode_LAST,
  RangeMode_LAST_BEFORE_MISSING_VALUES,
  RangeMode'
  #-}
