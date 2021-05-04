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

import qualified Network.AWS.Prelude as Prelude

-- | Enable this setting to insert I-frames at scene changes that the service
-- automatically detects. This improves video quality and is enabled by
-- default.
newtype Mpeg2SceneChangeDetect = Mpeg2SceneChangeDetect'
  { fromMpeg2SceneChangeDetect ::
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

pattern Mpeg2SceneChangeDetect_DISABLED :: Mpeg2SceneChangeDetect
pattern Mpeg2SceneChangeDetect_DISABLED = Mpeg2SceneChangeDetect' "DISABLED"

pattern Mpeg2SceneChangeDetect_ENABLED :: Mpeg2SceneChangeDetect
pattern Mpeg2SceneChangeDetect_ENABLED = Mpeg2SceneChangeDetect' "ENABLED"

{-# COMPLETE
  Mpeg2SceneChangeDetect_DISABLED,
  Mpeg2SceneChangeDetect_ENABLED,
  Mpeg2SceneChangeDetect'
  #-}
