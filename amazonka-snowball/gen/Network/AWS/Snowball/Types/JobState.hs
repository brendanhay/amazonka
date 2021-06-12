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
-- Module      : Network.AWS.Snowball.Types.JobState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.JobState
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

import qualified Network.AWS.Core as Core

newtype JobState = JobState'
  { fromJobState ::
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
