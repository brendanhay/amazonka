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
-- Module      : Amazonka.Snowball.Types.JobState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Snowball.Types.JobState
  ( JobState
      ( ..,
        JobState_Cancelled,
        JobState_Complete,
        JobState_InProgress,
        JobState_InTransitToAWS,
        JobState_InTransitToCustomer,
        JobState_Listing,
        JobState_New,
        JobState_Pending,
        JobState_PreparingAppliance,
        JobState_PreparingShipment,
        JobState_WithAWS,
        JobState_WithAWSSortingFacility,
        JobState_WithCustomer
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype JobState = JobState'
  { fromJobState ::
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

pattern JobState_Cancelled :: JobState
pattern JobState_Cancelled = JobState' "Cancelled"

pattern JobState_Complete :: JobState
pattern JobState_Complete = JobState' "Complete"

pattern JobState_InProgress :: JobState
pattern JobState_InProgress = JobState' "InProgress"

pattern JobState_InTransitToAWS :: JobState
pattern JobState_InTransitToAWS = JobState' "InTransitToAWS"

pattern JobState_InTransitToCustomer :: JobState
pattern JobState_InTransitToCustomer = JobState' "InTransitToCustomer"

pattern JobState_Listing :: JobState
pattern JobState_Listing = JobState' "Listing"

pattern JobState_New :: JobState
pattern JobState_New = JobState' "New"

pattern JobState_Pending :: JobState
pattern JobState_Pending = JobState' "Pending"

pattern JobState_PreparingAppliance :: JobState
pattern JobState_PreparingAppliance = JobState' "PreparingAppliance"

pattern JobState_PreparingShipment :: JobState
pattern JobState_PreparingShipment = JobState' "PreparingShipment"

pattern JobState_WithAWS :: JobState
pattern JobState_WithAWS = JobState' "WithAWS"

pattern JobState_WithAWSSortingFacility :: JobState
pattern JobState_WithAWSSortingFacility = JobState' "WithAWSSortingFacility"

pattern JobState_WithCustomer :: JobState
pattern JobState_WithCustomer = JobState' "WithCustomer"

{-# COMPLETE
  JobState_Cancelled,
  JobState_Complete,
  JobState_InProgress,
  JobState_InTransitToAWS,
  JobState_InTransitToCustomer,
  JobState_Listing,
  JobState_New,
  JobState_Pending,
  JobState_PreparingAppliance,
  JobState_PreparingShipment,
  JobState_WithAWS,
  JobState_WithAWSSortingFacility,
  JobState_WithCustomer,
  JobState'
  #-}
