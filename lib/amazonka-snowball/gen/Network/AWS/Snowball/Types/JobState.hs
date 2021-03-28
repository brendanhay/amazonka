{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.JobState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Snowball.Types.JobState
  ( JobState
    ( JobState'
    , JobStateNew
    , JobStatePreparingAppliance
    , JobStatePreparingShipment
    , JobStateInTransitToCustomer
    , JobStateWithCustomer
    , JobStateInTransitToAWS
    , JobStateWithAWSSortingFacility
    , JobStateWithAWS
    , JobStateInProgress
    , JobStateComplete
    , JobStateCancelled
    , JobStateListing
    , JobStatePending
    , fromJobState
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype JobState = JobState'{fromJobState :: Core.Text}
                     deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                     Core.Generic)
                     deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                       Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON, Core.FromJSON,
                                       Core.ToXML, Core.FromXML, Core.ToText, Core.FromText,
                                       Core.ToByteString, Core.ToQuery, Core.ToHeader)

pattern JobStateNew :: JobState
pattern JobStateNew = JobState' "New"

pattern JobStatePreparingAppliance :: JobState
pattern JobStatePreparingAppliance = JobState' "PreparingAppliance"

pattern JobStatePreparingShipment :: JobState
pattern JobStatePreparingShipment = JobState' "PreparingShipment"

pattern JobStateInTransitToCustomer :: JobState
pattern JobStateInTransitToCustomer = JobState' "InTransitToCustomer"

pattern JobStateWithCustomer :: JobState
pattern JobStateWithCustomer = JobState' "WithCustomer"

pattern JobStateInTransitToAWS :: JobState
pattern JobStateInTransitToAWS = JobState' "InTransitToAWS"

pattern JobStateWithAWSSortingFacility :: JobState
pattern JobStateWithAWSSortingFacility = JobState' "WithAWSSortingFacility"

pattern JobStateWithAWS :: JobState
pattern JobStateWithAWS = JobState' "WithAWS"

pattern JobStateInProgress :: JobState
pattern JobStateInProgress = JobState' "InProgress"

pattern JobStateComplete :: JobState
pattern JobStateComplete = JobState' "Complete"

pattern JobStateCancelled :: JobState
pattern JobStateCancelled = JobState' "Cancelled"

pattern JobStateListing :: JobState
pattern JobStateListing = JobState' "Listing"

pattern JobStatePending :: JobState
pattern JobStatePending = JobState' "Pending"

{-# COMPLETE 
  JobStateNew,

  JobStatePreparingAppliance,

  JobStatePreparingShipment,

  JobStateInTransitToCustomer,

  JobStateWithCustomer,

  JobStateInTransitToAWS,

  JobStateWithAWSSortingFacility,

  JobStateWithAWS,

  JobStateInProgress,

  JobStateComplete,

  JobStateCancelled,

  JobStateListing,

  JobStatePending,
  JobState'
  #-}
