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
-- Module      : Network.AWS.CostExplorer.Types.Granularity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.Granularity
  ( Granularity
      ( ..,
        Granularity_DAILY,
        Granularity_HOURLY,
        Granularity_MONTHLY
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype Granularity = Granularity'
  { fromGranularity ::
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

pattern Granularity_DAILY :: Granularity
pattern Granularity_DAILY = Granularity' "DAILY"

pattern Granularity_HOURLY :: Granularity
pattern Granularity_HOURLY = Granularity' "HOURLY"

pattern Granularity_MONTHLY :: Granularity
pattern Granularity_MONTHLY = Granularity' "MONTHLY"

{-# COMPLETE
  Granularity_DAILY,
  Granularity_HOURLY,
  Granularity_MONTHLY,
  Granularity'
  #-}
