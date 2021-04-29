{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.VideoSelectorColorSpaceUsage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.VideoSelectorColorSpaceUsage
  ( VideoSelectorColorSpaceUsage
      ( ..,
        VideoSelectorColorSpaceUsage_FALLBACK,
        VideoSelectorColorSpaceUsage_FORCE
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Video Selector Color Space Usage
newtype VideoSelectorColorSpaceUsage = VideoSelectorColorSpaceUsage'
  { fromVideoSelectorColorSpaceUsage ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern VideoSelectorColorSpaceUsage_FALLBACK :: VideoSelectorColorSpaceUsage
pattern VideoSelectorColorSpaceUsage_FALLBACK = VideoSelectorColorSpaceUsage' "FALLBACK"

pattern VideoSelectorColorSpaceUsage_FORCE :: VideoSelectorColorSpaceUsage
pattern VideoSelectorColorSpaceUsage_FORCE = VideoSelectorColorSpaceUsage' "FORCE"

{-# COMPLETE
  VideoSelectorColorSpaceUsage_FALLBACK,
  VideoSelectorColorSpaceUsage_FORCE,
  VideoSelectorColorSpaceUsage'
  #-}
