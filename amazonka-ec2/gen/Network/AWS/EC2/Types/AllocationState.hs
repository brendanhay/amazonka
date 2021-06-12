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
-- Module      : Network.AWS.EC2.Types.AllocationState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AllocationState
  ( AllocationState
      ( ..,
        AllocationState_Available,
        AllocationState_Pending,
        AllocationState_Permanent_failure,
        AllocationState_Released,
        AllocationState_Released_permanent_failure,
        AllocationState_Under_assessment
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal

newtype AllocationState = AllocationState'
  { fromAllocationState ::
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

pattern AllocationState_Available :: AllocationState
pattern AllocationState_Available = AllocationState' "available"

pattern AllocationState_Pending :: AllocationState
pattern AllocationState_Pending = AllocationState' "pending"

pattern AllocationState_Permanent_failure :: AllocationState
pattern AllocationState_Permanent_failure = AllocationState' "permanent-failure"

pattern AllocationState_Released :: AllocationState
pattern AllocationState_Released = AllocationState' "released"

pattern AllocationState_Released_permanent_failure :: AllocationState
pattern AllocationState_Released_permanent_failure = AllocationState' "released-permanent-failure"

pattern AllocationState_Under_assessment :: AllocationState
pattern AllocationState_Under_assessment = AllocationState' "under-assessment"

{-# COMPLETE
  AllocationState_Available,
  AllocationState_Pending,
  AllocationState_Permanent_failure,
  AllocationState_Released,
  AllocationState_Released_permanent_failure,
  AllocationState_Under_assessment,
  AllocationState'
  #-}
