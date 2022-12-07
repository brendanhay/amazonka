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
-- Module      : Amazonka.MediaLive.Types.VideoSelectorColorSpaceUsage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.VideoSelectorColorSpaceUsage
  ( VideoSelectorColorSpaceUsage
      ( ..,
        VideoSelectorColorSpaceUsage_FALLBACK,
        VideoSelectorColorSpaceUsage_FORCE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Video Selector Color Space Usage
newtype VideoSelectorColorSpaceUsage = VideoSelectorColorSpaceUsage'
  { fromVideoSelectorColorSpaceUsage ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
