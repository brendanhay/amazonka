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
        New,
        PreparingAppliance,
        PreparingShipment,
        InTransitToCustomer,
        WithCustomer,
        InTransitToAWS,
        WithAWSSortingFacility,
        WithAWS,
        InProgress,
        Complete,
        Cancelled,
        Listing,
        Pending
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

pattern New :: JobState
pattern New = JobState' "New"

pattern PreparingAppliance :: JobState
pattern PreparingAppliance = JobState' "PreparingAppliance"

pattern PreparingShipment :: JobState
pattern PreparingShipment = JobState' "PreparingShipment"

pattern InTransitToCustomer :: JobState
pattern InTransitToCustomer = JobState' "InTransitToCustomer"

pattern WithCustomer :: JobState
pattern WithCustomer = JobState' "WithCustomer"

pattern InTransitToAWS :: JobState
pattern InTransitToAWS = JobState' "InTransitToAWS"

pattern WithAWSSortingFacility :: JobState
pattern WithAWSSortingFacility = JobState' "WithAWSSortingFacility"

pattern WithAWS :: JobState
pattern WithAWS = JobState' "WithAWS"

pattern InProgress :: JobState
pattern InProgress = JobState' "InProgress"

pattern Complete :: JobState
pattern Complete = JobState' "Complete"

pattern Cancelled :: JobState
pattern Cancelled = JobState' "Cancelled"

pattern Listing :: JobState
pattern Listing = JobState' "Listing"

pattern Pending :: JobState
pattern Pending = JobState' "Pending"

{-# COMPLETE
  New,
  PreparingAppliance,
  PreparingShipment,
  InTransitToCustomer,
  WithCustomer,
  InTransitToAWS,
  WithAWSSortingFacility,
  WithAWS,
  InProgress,
  Complete,
  Cancelled,
  Listing,
  Pending,
  JobState'
  #-}
