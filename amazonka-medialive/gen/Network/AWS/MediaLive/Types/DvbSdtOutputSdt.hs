{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.DvbSdtOutputSdt
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.DvbSdtOutputSdt
  ( DvbSdtOutputSdt
      ( ..,
        DvbSdtOutputSdt_SDT_FOLLOW,
        DvbSdtOutputSdt_SDT_FOLLOW_IF_PRESENT,
        DvbSdtOutputSdt_SDT_MANUAL,
        DvbSdtOutputSdt_SDT_NONE
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Dvb Sdt Output Sdt
newtype DvbSdtOutputSdt = DvbSdtOutputSdt'
  { fromDvbSdtOutputSdt ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern DvbSdtOutputSdt_SDT_FOLLOW :: DvbSdtOutputSdt
pattern DvbSdtOutputSdt_SDT_FOLLOW = DvbSdtOutputSdt' "SDT_FOLLOW"

pattern DvbSdtOutputSdt_SDT_FOLLOW_IF_PRESENT :: DvbSdtOutputSdt
pattern DvbSdtOutputSdt_SDT_FOLLOW_IF_PRESENT = DvbSdtOutputSdt' "SDT_FOLLOW_IF_PRESENT"

pattern DvbSdtOutputSdt_SDT_MANUAL :: DvbSdtOutputSdt
pattern DvbSdtOutputSdt_SDT_MANUAL = DvbSdtOutputSdt' "SDT_MANUAL"

pattern DvbSdtOutputSdt_SDT_NONE :: DvbSdtOutputSdt
pattern DvbSdtOutputSdt_SDT_NONE = DvbSdtOutputSdt' "SDT_NONE"

{-# COMPLETE
  DvbSdtOutputSdt_SDT_FOLLOW,
  DvbSdtOutputSdt_SDT_FOLLOW_IF_PRESENT,
  DvbSdtOutputSdt_SDT_MANUAL,
  DvbSdtOutputSdt_SDT_NONE,
  DvbSdtOutputSdt'
  #-}
