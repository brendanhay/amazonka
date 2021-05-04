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

import qualified Network.AWS.Prelude as Prelude

newtype TimeUnitsType = TimeUnitsType'
  { fromTimeUnitsType ::
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
