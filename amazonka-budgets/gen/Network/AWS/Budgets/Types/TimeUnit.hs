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
-- Module      : Network.AWS.Budgets.Types.TimeUnit
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.TimeUnit
  ( TimeUnit
      ( ..,
        TimeUnit_ANNUALLY,
        TimeUnit_DAILY,
        TimeUnit_MONTHLY,
        TimeUnit_QUARTERLY
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | The time unit of the budget, such as MONTHLY or QUARTERLY.
newtype TimeUnit = TimeUnit'
  { fromTimeUnit ::
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

pattern TimeUnit_ANNUALLY :: TimeUnit
pattern TimeUnit_ANNUALLY = TimeUnit' "ANNUALLY"

pattern TimeUnit_DAILY :: TimeUnit
pattern TimeUnit_DAILY = TimeUnit' "DAILY"

pattern TimeUnit_MONTHLY :: TimeUnit
pattern TimeUnit_MONTHLY = TimeUnit' "MONTHLY"

pattern TimeUnit_QUARTERLY :: TimeUnit
pattern TimeUnit_QUARTERLY = TimeUnit' "QUARTERLY"

{-# COMPLETE
  TimeUnit_ANNUALLY,
  TimeUnit_DAILY,
  TimeUnit_MONTHLY,
  TimeUnit_QUARTERLY,
  TimeUnit'
  #-}
