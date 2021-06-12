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
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2SceneChangeDetect
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mpeg2SceneChangeDetect
  ( Mpeg2SceneChangeDetect
      ( ..,
        Mpeg2SceneChangeDetect_DISABLED,
        Mpeg2SceneChangeDetect_ENABLED
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Enable this setting to insert I-frames at scene changes that the service
-- automatically detects. This improves video quality and is enabled by
-- default.
newtype Mpeg2SceneChangeDetect = Mpeg2SceneChangeDetect'
  { fromMpeg2SceneChangeDetect ::
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

pattern Mpeg2SceneChangeDetect_DISABLED :: Mpeg2SceneChangeDetect
pattern Mpeg2SceneChangeDetect_DISABLED = Mpeg2SceneChangeDetect' "DISABLED"

pattern Mpeg2SceneChangeDetect_ENABLED :: Mpeg2SceneChangeDetect
pattern Mpeg2SceneChangeDetect_ENABLED = Mpeg2SceneChangeDetect' "ENABLED"

{-# COMPLETE
  Mpeg2SceneChangeDetect_DISABLED,
  Mpeg2SceneChangeDetect_ENABLED,
  Mpeg2SceneChangeDetect'
  #-}
