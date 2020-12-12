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
        Both,
        None,
        Restricted,
        Unrestricted
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | This setting allows the delivery restriction flags on SCTE-35 segmentation descriptors to
--
-- determine whether a message signals an ad.  Choosing "NONE" means no SCTE-35 messages become
-- ads.  Choosing "RESTRICTED" means SCTE-35 messages of the types specified in AdTriggers that
-- contain delivery restrictions will be treated as ads.  Choosing "UNRESTRICTED" means SCTE-35
-- messages of the types specified in AdTriggers that do not contain delivery restrictions will
-- be treated as ads.  Choosing "BOTH" means all SCTE-35 messages of the types specified in
-- AdTriggers will be treated as ads.  Note that Splice Insert messages do not have these flags
-- and are always treated as ads if specified in AdTriggers.
newtype AdsOnDeliveryRestrictions = AdsOnDeliveryRestrictions' Lude.Text
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

pattern Both :: AdsOnDeliveryRestrictions
pattern Both = AdsOnDeliveryRestrictions' "BOTH"

pattern None :: AdsOnDeliveryRestrictions
pattern None = AdsOnDeliveryRestrictions' "NONE"

pattern Restricted :: AdsOnDeliveryRestrictions
pattern Restricted = AdsOnDeliveryRestrictions' "RESTRICTED"

pattern Unrestricted :: AdsOnDeliveryRestrictions
pattern Unrestricted = AdsOnDeliveryRestrictions' "UNRESTRICTED"

{-# COMPLETE
  Both,
  None,
  Restricted,
  Unrestricted,
  AdsOnDeliveryRestrictions'
  #-}
