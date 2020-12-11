-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.DvbSdtOutputSdt
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.DvbSdtOutputSdt
  ( DvbSdtOutputSdt
      ( DvbSdtOutputSdt',
        SdtFollow,
        SdtFollowIfPresent,
        SdtManual,
        SdtNone
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Dvb Sdt Output Sdt
newtype DvbSdtOutputSdt = DvbSdtOutputSdt' Lude.Text
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

pattern SdtFollow :: DvbSdtOutputSdt
pattern SdtFollow = DvbSdtOutputSdt' "SDT_FOLLOW"

pattern SdtFollowIfPresent :: DvbSdtOutputSdt
pattern SdtFollowIfPresent = DvbSdtOutputSdt' "SDT_FOLLOW_IF_PRESENT"

pattern SdtManual :: DvbSdtOutputSdt
pattern SdtManual = DvbSdtOutputSdt' "SDT_MANUAL"

pattern SdtNone :: DvbSdtOutputSdt
pattern SdtNone = DvbSdtOutputSdt' "SDT_NONE"

{-# COMPLETE
  SdtFollow,
  SdtFollowIfPresent,
  SdtManual,
  SdtNone,
  DvbSdtOutputSdt'
  #-}
