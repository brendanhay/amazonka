{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AvailabilityZoneOptInStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AvailabilityZoneOptInStatus
  ( AvailabilityZoneOptInStatus
      ( AvailabilityZoneOptInStatus',
        AZOISOptInNotRequired,
        AZOISOptedIn,
        AZOISNotOptedIn
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AvailabilityZoneOptInStatus = AvailabilityZoneOptInStatus' Lude.Text
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

pattern AZOISOptInNotRequired :: AvailabilityZoneOptInStatus
pattern AZOISOptInNotRequired = AvailabilityZoneOptInStatus' "opt-in-not-required"

pattern AZOISOptedIn :: AvailabilityZoneOptInStatus
pattern AZOISOptedIn = AvailabilityZoneOptInStatus' "opted-in"

pattern AZOISNotOptedIn :: AvailabilityZoneOptInStatus
pattern AZOISNotOptedIn = AvailabilityZoneOptInStatus' "not-opted-in"

{-# COMPLETE
  AZOISOptInNotRequired,
  AZOISOptedIn,
  AZOISNotOptedIn,
  AvailabilityZoneOptInStatus'
  #-}
