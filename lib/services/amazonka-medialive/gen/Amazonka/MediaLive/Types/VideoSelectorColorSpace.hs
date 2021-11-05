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
-- Module      : Amazonka.MediaLive.Types.VideoSelectorColorSpace
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.VideoSelectorColorSpace
  ( VideoSelectorColorSpace
      ( ..,
        VideoSelectorColorSpace_FOLLOW,
        VideoSelectorColorSpace_HDR10,
        VideoSelectorColorSpace_HLG_2020,
        VideoSelectorColorSpace_REC_601,
        VideoSelectorColorSpace_REC_709
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | Video Selector Color Space
newtype VideoSelectorColorSpace = VideoSelectorColorSpace'
  { fromVideoSelectorColorSpace ::
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

pattern VideoSelectorColorSpace_FOLLOW :: VideoSelectorColorSpace
pattern VideoSelectorColorSpace_FOLLOW = VideoSelectorColorSpace' "FOLLOW"

pattern VideoSelectorColorSpace_HDR10 :: VideoSelectorColorSpace
pattern VideoSelectorColorSpace_HDR10 = VideoSelectorColorSpace' "HDR10"

pattern VideoSelectorColorSpace_HLG_2020 :: VideoSelectorColorSpace
pattern VideoSelectorColorSpace_HLG_2020 = VideoSelectorColorSpace' "HLG_2020"

pattern VideoSelectorColorSpace_REC_601 :: VideoSelectorColorSpace
pattern VideoSelectorColorSpace_REC_601 = VideoSelectorColorSpace' "REC_601"

pattern VideoSelectorColorSpace_REC_709 :: VideoSelectorColorSpace
pattern VideoSelectorColorSpace_REC_709 = VideoSelectorColorSpace' "REC_709"

{-# COMPLETE
  VideoSelectorColorSpace_FOLLOW,
  VideoSelectorColorSpace_HDR10,
  VideoSelectorColorSpace_HLG_2020,
  VideoSelectorColorSpace_REC_601,
  VideoSelectorColorSpace_REC_709,
  VideoSelectorColorSpace'
  #-}
