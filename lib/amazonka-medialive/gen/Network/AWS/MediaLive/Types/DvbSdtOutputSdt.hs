{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        DvbSdtOutputSdtSdtFollow,
        DvbSdtOutputSdtSdtFollowIfPresent,
        DvbSdtOutputSdtSdtManual,
        DvbSdtOutputSdtSdtNone,
        fromDvbSdtOutputSdt
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Dvb Sdt Output Sdt
newtype DvbSdtOutputSdt = DvbSdtOutputSdt'
  { fromDvbSdtOutputSdt ::
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

pattern DvbSdtOutputSdtSdtFollow :: DvbSdtOutputSdt
pattern DvbSdtOutputSdtSdtFollow = DvbSdtOutputSdt' "SDT_FOLLOW"

pattern DvbSdtOutputSdtSdtFollowIfPresent :: DvbSdtOutputSdt
pattern DvbSdtOutputSdtSdtFollowIfPresent = DvbSdtOutputSdt' "SDT_FOLLOW_IF_PRESENT"

pattern DvbSdtOutputSdtSdtManual :: DvbSdtOutputSdt
pattern DvbSdtOutputSdtSdtManual = DvbSdtOutputSdt' "SDT_MANUAL"

pattern DvbSdtOutputSdtSdtNone :: DvbSdtOutputSdt
pattern DvbSdtOutputSdtSdtNone = DvbSdtOutputSdt' "SDT_NONE"

{-# COMPLETE
  DvbSdtOutputSdtSdtFollow,
  DvbSdtOutputSdtSdtFollowIfPresent,
  DvbSdtOutputSdtSdtManual,
  DvbSdtOutputSdtSdtNone,
  DvbSdtOutputSdt'
  #-}
