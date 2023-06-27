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
-- Module      : Amazonka.QuickSight.Types.RefreshInterval
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.RefreshInterval
  ( RefreshInterval
      ( ..,
        RefreshInterval_DAILY,
        RefreshInterval_HOURLY,
        RefreshInterval_MINUTE15,
        RefreshInterval_MINUTE30,
        RefreshInterval_MONTHLY,
        RefreshInterval_WEEKLY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RefreshInterval = RefreshInterval'
  { fromRefreshInterval ::
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

pattern RefreshInterval_DAILY :: RefreshInterval
pattern RefreshInterval_DAILY = RefreshInterval' "DAILY"

pattern RefreshInterval_HOURLY :: RefreshInterval
pattern RefreshInterval_HOURLY = RefreshInterval' "HOURLY"

pattern RefreshInterval_MINUTE15 :: RefreshInterval
pattern RefreshInterval_MINUTE15 = RefreshInterval' "MINUTE15"

pattern RefreshInterval_MINUTE30 :: RefreshInterval
pattern RefreshInterval_MINUTE30 = RefreshInterval' "MINUTE30"

pattern RefreshInterval_MONTHLY :: RefreshInterval
pattern RefreshInterval_MONTHLY = RefreshInterval' "MONTHLY"

pattern RefreshInterval_WEEKLY :: RefreshInterval
pattern RefreshInterval_WEEKLY = RefreshInterval' "WEEKLY"

{-# COMPLETE
  RefreshInterval_DAILY,
  RefreshInterval_HOURLY,
  RefreshInterval_MINUTE15,
  RefreshInterval_MINUTE30,
  RefreshInterval_MONTHLY,
  RefreshInterval_WEEKLY,
  RefreshInterval'
  #-}
