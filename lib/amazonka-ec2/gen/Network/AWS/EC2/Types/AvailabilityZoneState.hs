{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AvailabilityZoneState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AvailabilityZoneState
  ( AvailabilityZoneState
      ( AvailabilityZoneState',
        AZSAvailable,
        AZSImpaired,
        AZSInformation,
        AZSUnavailable
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AvailabilityZoneState = AvailabilityZoneState' Lude.Text
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

pattern AZSAvailable :: AvailabilityZoneState
pattern AZSAvailable = AvailabilityZoneState' "available"

pattern AZSImpaired :: AvailabilityZoneState
pattern AZSImpaired = AvailabilityZoneState' "impaired"

pattern AZSInformation :: AvailabilityZoneState
pattern AZSInformation = AvailabilityZoneState' "information"

pattern AZSUnavailable :: AvailabilityZoneState
pattern AZSUnavailable = AvailabilityZoneState' "unavailable"

{-# COMPLETE
  AZSAvailable,
  AZSImpaired,
  AZSInformation,
  AZSUnavailable,
  AvailabilityZoneState'
  #-}
