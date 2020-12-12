{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.JobState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.JobState
  ( JobState
      ( JobState',
        JSCancelled,
        JSComplete,
        JSInProgress,
        JSInTransitToAWS,
        JSInTransitToCustomer,
        JSListing,
        JSNew,
        JSPending,
        JSPreparingAppliance,
        JSPreparingShipment,
        JSWithAWS,
        JSWithAWSSortingFacility,
        JSWithCustomer
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype JobState = JobState' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern JSCancelled :: JobState
pattern JSCancelled = JobState' "Cancelled"

pattern JSComplete :: JobState
pattern JSComplete = JobState' "Complete"

pattern JSInProgress :: JobState
pattern JSInProgress = JobState' "InProgress"

pattern JSInTransitToAWS :: JobState
pattern JSInTransitToAWS = JobState' "InTransitToAWS"

pattern JSInTransitToCustomer :: JobState
pattern JSInTransitToCustomer = JobState' "InTransitToCustomer"

pattern JSListing :: JobState
pattern JSListing = JobState' "Listing"

pattern JSNew :: JobState
pattern JSNew = JobState' "New"

pattern JSPending :: JobState
pattern JSPending = JobState' "Pending"

pattern JSPreparingAppliance :: JobState
pattern JSPreparingAppliance = JobState' "PreparingAppliance"

pattern JSPreparingShipment :: JobState
pattern JSPreparingShipment = JobState' "PreparingShipment"

pattern JSWithAWS :: JobState
pattern JSWithAWS = JobState' "WithAWS"

pattern JSWithAWSSortingFacility :: JobState
pattern JSWithAWSSortingFacility = JobState' "WithAWSSortingFacility"

pattern JSWithCustomer :: JobState
pattern JSWithCustomer = JobState' "WithCustomer"

{-# COMPLETE
  JSCancelled,
  JSComplete,
  JSInProgress,
  JSInTransitToAWS,
  JSInTransitToCustomer,
  JSListing,
  JSNew,
  JSPending,
  JSPreparingAppliance,
  JSPreparingShipment,
  JSWithAWS,
  JSWithAWSSortingFacility,
  JSWithCustomer,
  JobState'
  #-}
