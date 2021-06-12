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
-- Module      : Network.AWS.MediaLive.Types.VideoSelectorColorSpace
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.VideoSelectorColorSpace
  ( VideoSelectorColorSpace
      ( ..,
        VideoSelectorColorSpace_FOLLOW,
        VideoSelectorColorSpace_REC_601,
        VideoSelectorColorSpace_REC_709
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Video Selector Color Space
newtype VideoSelectorColorSpace = VideoSelectorColorSpace'
  { fromVideoSelectorColorSpace ::
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

pattern VideoSelectorColorSpace_FOLLOW :: VideoSelectorColorSpace
pattern VideoSelectorColorSpace_FOLLOW = VideoSelectorColorSpace' "FOLLOW"

pattern VideoSelectorColorSpace_REC_601 :: VideoSelectorColorSpace
pattern VideoSelectorColorSpace_REC_601 = VideoSelectorColorSpace' "REC_601"

pattern VideoSelectorColorSpace_REC_709 :: VideoSelectorColorSpace
pattern VideoSelectorColorSpace_REC_709 = VideoSelectorColorSpace' "REC_709"

{-# COMPLETE
  VideoSelectorColorSpace_FOLLOW,
  VideoSelectorColorSpace_REC_601,
  VideoSelectorColorSpace_REC_709,
  VideoSelectorColorSpace'
  #-}
