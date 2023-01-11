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
-- Module      : Amazonka.CognitoIdentityProvider.Types.TimeUnitsType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.TimeUnitsType
  ( TimeUnitsType
      ( ..,
        TimeUnitsType_Days,
        TimeUnitsType_Hours,
        TimeUnitsType_Minutes,
        TimeUnitsType_Seconds
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TimeUnitsType = TimeUnitsType'
  { fromTimeUnitsType ::
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

pattern TimeUnitsType_Days :: TimeUnitsType
pattern TimeUnitsType_Days = TimeUnitsType' "days"

pattern TimeUnitsType_Hours :: TimeUnitsType
pattern TimeUnitsType_Hours = TimeUnitsType' "hours"

pattern TimeUnitsType_Minutes :: TimeUnitsType
pattern TimeUnitsType_Minutes = TimeUnitsType' "minutes"

pattern TimeUnitsType_Seconds :: TimeUnitsType
pattern TimeUnitsType_Seconds = TimeUnitsType' "seconds"

{-# COMPLETE
  TimeUnitsType_Days,
  TimeUnitsType_Hours,
  TimeUnitsType_Minutes,
  TimeUnitsType_Seconds,
  TimeUnitsType'
  #-}
