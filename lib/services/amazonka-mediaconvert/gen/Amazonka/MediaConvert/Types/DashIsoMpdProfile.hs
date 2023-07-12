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
-- Module      : Amazonka.MediaConvert.Types.DashIsoMpdProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.DashIsoMpdProfile
  ( DashIsoMpdProfile
      ( ..,
        DashIsoMpdProfile_MAIN_PROFILE,
        DashIsoMpdProfile_ON_DEMAND_PROFILE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specify whether your DASH profile is on-demand or main. When you choose
-- Main profile (MAIN_PROFILE), the service signals
-- urn:mpeg:dash:profile:isoff-main:2011 in your .mpd DASH manifest. When
-- you choose On-demand (ON_DEMAND_PROFILE), the service signals
-- urn:mpeg:dash:profile:isoff-on-demand:2011 in your .mpd. When you choose
-- On-demand, you must also set the output group setting Segment control
-- (SegmentControl) to Single file (SINGLE_FILE).
newtype DashIsoMpdProfile = DashIsoMpdProfile'
  { fromDashIsoMpdProfile ::
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

pattern DashIsoMpdProfile_MAIN_PROFILE :: DashIsoMpdProfile
pattern DashIsoMpdProfile_MAIN_PROFILE = DashIsoMpdProfile' "MAIN_PROFILE"

pattern DashIsoMpdProfile_ON_DEMAND_PROFILE :: DashIsoMpdProfile
pattern DashIsoMpdProfile_ON_DEMAND_PROFILE = DashIsoMpdProfile' "ON_DEMAND_PROFILE"

{-# COMPLETE
  DashIsoMpdProfile_MAIN_PROFILE,
  DashIsoMpdProfile_ON_DEMAND_PROFILE,
  DashIsoMpdProfile'
  #-}
