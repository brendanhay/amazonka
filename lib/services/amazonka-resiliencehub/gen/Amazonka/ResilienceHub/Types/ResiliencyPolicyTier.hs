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
-- Module      : Amazonka.ResilienceHub.Types.ResiliencyPolicyTier
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResilienceHub.Types.ResiliencyPolicyTier
  ( ResiliencyPolicyTier
      ( ..,
        ResiliencyPolicyTier_CoreServices,
        ResiliencyPolicyTier_Critical,
        ResiliencyPolicyTier_Important,
        ResiliencyPolicyTier_MissionCritical,
        ResiliencyPolicyTier_NonCritical
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ResiliencyPolicyTier = ResiliencyPolicyTier'
  { fromResiliencyPolicyTier ::
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

pattern ResiliencyPolicyTier_CoreServices :: ResiliencyPolicyTier
pattern ResiliencyPolicyTier_CoreServices = ResiliencyPolicyTier' "CoreServices"

pattern ResiliencyPolicyTier_Critical :: ResiliencyPolicyTier
pattern ResiliencyPolicyTier_Critical = ResiliencyPolicyTier' "Critical"

pattern ResiliencyPolicyTier_Important :: ResiliencyPolicyTier
pattern ResiliencyPolicyTier_Important = ResiliencyPolicyTier' "Important"

pattern ResiliencyPolicyTier_MissionCritical :: ResiliencyPolicyTier
pattern ResiliencyPolicyTier_MissionCritical = ResiliencyPolicyTier' "MissionCritical"

pattern ResiliencyPolicyTier_NonCritical :: ResiliencyPolicyTier
pattern ResiliencyPolicyTier_NonCritical = ResiliencyPolicyTier' "NonCritical"

{-# COMPLETE
  ResiliencyPolicyTier_CoreServices,
  ResiliencyPolicyTier_Critical,
  ResiliencyPolicyTier_Important,
  ResiliencyPolicyTier_MissionCritical,
  ResiliencyPolicyTier_NonCritical,
  ResiliencyPolicyTier'
  #-}
