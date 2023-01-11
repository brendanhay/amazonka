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
-- Module      : Amazonka.TimeStreamWrite.Types.TimeUnit
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamWrite.Types.TimeUnit
  ( TimeUnit
      ( ..,
        TimeUnit_MICROSECONDS,
        TimeUnit_MILLISECONDS,
        TimeUnit_NANOSECONDS,
        TimeUnit_SECONDS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TimeUnit = TimeUnit'
  { fromTimeUnit ::
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

pattern TimeUnit_MICROSECONDS :: TimeUnit
pattern TimeUnit_MICROSECONDS = TimeUnit' "MICROSECONDS"

pattern TimeUnit_MILLISECONDS :: TimeUnit
pattern TimeUnit_MILLISECONDS = TimeUnit' "MILLISECONDS"

pattern TimeUnit_NANOSECONDS :: TimeUnit
pattern TimeUnit_NANOSECONDS = TimeUnit' "NANOSECONDS"

pattern TimeUnit_SECONDS :: TimeUnit
pattern TimeUnit_SECONDS = TimeUnit' "SECONDS"

{-# COMPLETE
  TimeUnit_MICROSECONDS,
  TimeUnit_MILLISECONDS,
  TimeUnit_NANOSECONDS,
  TimeUnit_SECONDS,
  TimeUnit'
  #-}
