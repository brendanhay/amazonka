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
import qualified Network.AWS.Prelude as Prelude

-- | Dvb Sdt Output Sdt
newtype DvbSdtOutputSdt = DvbSdtOutputSdt'
  { fromDvbSdtOutputSdt ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
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
