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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.TimeUnitsType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.TimeUnitsType
  ( TimeUnitsType
      ( ..,
        TimeUnitsType_Days,
        TimeUnitsType_Hours,
        TimeUnitsType_Minutes,
        TimeUnitsType_Seconds
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype TimeUnitsType = TimeUnitsType'
  { fromTimeUnitsType ::
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
