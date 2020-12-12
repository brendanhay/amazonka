{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LocationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LocationType
  ( LocationType
      ( LocationType',
        AvailabilityZone,
        AvailabilityZoneId,
        Region
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype LocationType = LocationType' Lude.Text
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

pattern AvailabilityZone :: LocationType
pattern AvailabilityZone = LocationType' "availability-zone"

pattern AvailabilityZoneId :: LocationType
pattern AvailabilityZoneId = LocationType' "availability-zone-id"

pattern Region :: LocationType
pattern Region = LocationType' "region"

{-# COMPLETE
  AvailabilityZone,
  AvailabilityZoneId,
  Region,
  LocationType'
  #-}
