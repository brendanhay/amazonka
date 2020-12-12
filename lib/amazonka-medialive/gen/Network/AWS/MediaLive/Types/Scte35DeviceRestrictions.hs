{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35DeviceRestrictions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte35DeviceRestrictions
  ( Scte35DeviceRestrictions
      ( Scte35DeviceRestrictions',
        SDRNone,
        SDRRestrictGROUP0,
        SDRRestrictGROUP1,
        SDRRestrictGROUP2
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Corresponds to the device_restrictions parameter in a segmentation_descriptor. If you include one of the "restriction" flags then you must include all four of them.
newtype Scte35DeviceRestrictions = Scte35DeviceRestrictions' Lude.Text
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

pattern SDRNone :: Scte35DeviceRestrictions
pattern SDRNone = Scte35DeviceRestrictions' "NONE"

pattern SDRRestrictGROUP0 :: Scte35DeviceRestrictions
pattern SDRRestrictGROUP0 = Scte35DeviceRestrictions' "RESTRICT_GROUP0"

pattern SDRRestrictGROUP1 :: Scte35DeviceRestrictions
pattern SDRRestrictGROUP1 = Scte35DeviceRestrictions' "RESTRICT_GROUP1"

pattern SDRRestrictGROUP2 :: Scte35DeviceRestrictions
pattern SDRRestrictGROUP2 = Scte35DeviceRestrictions' "RESTRICT_GROUP2"

{-# COMPLETE
  SDRNone,
  SDRRestrictGROUP0,
  SDRRestrictGROUP1,
  SDRRestrictGROUP2,
  Scte35DeviceRestrictions'
  #-}
