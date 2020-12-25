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
        Scte35DeviceRestrictionsNone,
        Scte35DeviceRestrictionsRestrictGROUP0,
        Scte35DeviceRestrictionsRestrictGROUP1,
        Scte35DeviceRestrictionsRestrictGROUP2,
        fromScte35DeviceRestrictions
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Corresponds to the device_restrictions parameter in a segmentation_descriptor. If you include one of the "restriction" flags then you must include all four of them.
newtype Scte35DeviceRestrictions = Scte35DeviceRestrictions'
  { fromScte35DeviceRestrictions ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern Scte35DeviceRestrictionsNone :: Scte35DeviceRestrictions
pattern Scte35DeviceRestrictionsNone = Scte35DeviceRestrictions' "NONE"

pattern Scte35DeviceRestrictionsRestrictGROUP0 :: Scte35DeviceRestrictions
pattern Scte35DeviceRestrictionsRestrictGROUP0 = Scte35DeviceRestrictions' "RESTRICT_GROUP0"

pattern Scte35DeviceRestrictionsRestrictGROUP1 :: Scte35DeviceRestrictions
pattern Scte35DeviceRestrictionsRestrictGROUP1 = Scte35DeviceRestrictions' "RESTRICT_GROUP1"

pattern Scte35DeviceRestrictionsRestrictGROUP2 :: Scte35DeviceRestrictions
pattern Scte35DeviceRestrictionsRestrictGROUP2 = Scte35DeviceRestrictions' "RESTRICT_GROUP2"

{-# COMPLETE
  Scte35DeviceRestrictionsNone,
  Scte35DeviceRestrictionsRestrictGROUP0,
  Scte35DeviceRestrictionsRestrictGROUP1,
  Scte35DeviceRestrictionsRestrictGROUP2,
  Scte35DeviceRestrictions'
  #-}
