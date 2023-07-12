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
-- Module      : Amazonka.EC2.Types.AllocationState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.AllocationState
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype AllocationState = AllocationState'
  { fromAllocationState ::
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
