{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.AdsOnDeliveryRestrictions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.AdsOnDeliveryRestrictions
  ( AdsOnDeliveryRestrictions
      ( AdsOnDeliveryRestrictions',
        AdsOnDeliveryRestrictionsNone,
        AdsOnDeliveryRestrictionsRestricted,
        AdsOnDeliveryRestrictionsUnrestricted,
        AdsOnDeliveryRestrictionsBoth,
        fromAdsOnDeliveryRestrictions
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | This setting allows the delivery restriction flags on SCTE-35 segmentation descriptors to
--
-- determine whether a message signals an ad.  Choosing "NONE" means no SCTE-35 messages become
-- ads.  Choosing "RESTRICTED" means SCTE-35 messages of the types specified in AdTriggers that
-- contain delivery restrictions will be treated as ads.  Choosing "UNRESTRICTED" means SCTE-35
-- messages of the types specified in AdTriggers that do not contain delivery restrictions will
-- be treated as ads.  Choosing "BOTH" means all SCTE-35 messages of the types specified in
-- AdTriggers will be treated as ads.  Note that Splice Insert messages do not have these flags
-- and are always treated as ads if specified in AdTriggers.
newtype AdsOnDeliveryRestrictions = AdsOnDeliveryRestrictions'
  { fromAdsOnDeliveryRestrictions ::
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

pattern AdsOnDeliveryRestrictionsNone :: AdsOnDeliveryRestrictions
pattern AdsOnDeliveryRestrictionsNone = AdsOnDeliveryRestrictions' "NONE"

pattern AdsOnDeliveryRestrictionsRestricted :: AdsOnDeliveryRestrictions
pattern AdsOnDeliveryRestrictionsRestricted = AdsOnDeliveryRestrictions' "RESTRICTED"

pattern AdsOnDeliveryRestrictionsUnrestricted :: AdsOnDeliveryRestrictions
pattern AdsOnDeliveryRestrictionsUnrestricted = AdsOnDeliveryRestrictions' "UNRESTRICTED"

pattern AdsOnDeliveryRestrictionsBoth :: AdsOnDeliveryRestrictions
pattern AdsOnDeliveryRestrictionsBoth = AdsOnDeliveryRestrictions' "BOTH"

{-# COMPLETE
  AdsOnDeliveryRestrictionsNone,
  AdsOnDeliveryRestrictionsRestricted,
  AdsOnDeliveryRestrictionsUnrestricted,
  AdsOnDeliveryRestrictionsBoth,
  AdsOnDeliveryRestrictions'
  #-}
