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
-- Module      : Amazonka.Snowball.Types.SnowballCapacity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Snowball.Types.SnowballCapacity
  ( SnowballCapacity
      ( ..,
        SnowballCapacity_NoPreference,
        SnowballCapacity_T100,
        SnowballCapacity_T14,
        SnowballCapacity_T240,
        SnowballCapacity_T32,
        SnowballCapacity_T42,
        SnowballCapacity_T50,
        SnowballCapacity_T8,
        SnowballCapacity_T80,
        SnowballCapacity_T98
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SnowballCapacity = SnowballCapacity'
  { fromSnowballCapacity ::
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

pattern SnowballCapacity_NoPreference :: SnowballCapacity
pattern SnowballCapacity_NoPreference = SnowballCapacity' "NoPreference"

pattern SnowballCapacity_T100 :: SnowballCapacity
pattern SnowballCapacity_T100 = SnowballCapacity' "T100"

pattern SnowballCapacity_T14 :: SnowballCapacity
pattern SnowballCapacity_T14 = SnowballCapacity' "T14"

pattern SnowballCapacity_T240 :: SnowballCapacity
pattern SnowballCapacity_T240 = SnowballCapacity' "T240"

pattern SnowballCapacity_T32 :: SnowballCapacity
pattern SnowballCapacity_T32 = SnowballCapacity' "T32"

pattern SnowballCapacity_T42 :: SnowballCapacity
pattern SnowballCapacity_T42 = SnowballCapacity' "T42"

pattern SnowballCapacity_T50 :: SnowballCapacity
pattern SnowballCapacity_T50 = SnowballCapacity' "T50"

pattern SnowballCapacity_T8 :: SnowballCapacity
pattern SnowballCapacity_T8 = SnowballCapacity' "T8"

pattern SnowballCapacity_T80 :: SnowballCapacity
pattern SnowballCapacity_T80 = SnowballCapacity' "T80"

pattern SnowballCapacity_T98 :: SnowballCapacity
pattern SnowballCapacity_T98 = SnowballCapacity' "T98"

{-# COMPLETE
  SnowballCapacity_NoPreference,
  SnowballCapacity_T100,
  SnowballCapacity_T14,
  SnowballCapacity_T240,
  SnowballCapacity_T32,
  SnowballCapacity_T42,
  SnowballCapacity_T50,
  SnowballCapacity_T8,
  SnowballCapacity_T80,
  SnowballCapacity_T98,
  SnowballCapacity'
  #-}
