{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2SceneChangeDetect
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mpeg2SceneChangeDetect
  ( Mpeg2SceneChangeDetect
      ( Mpeg2SceneChangeDetect',
        MSCDDisabled,
        MSCDEnabled
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Enable this setting to insert I-frames at scene changes that the service automatically detects. This improves video quality and is enabled by default.
newtype Mpeg2SceneChangeDetect = Mpeg2SceneChangeDetect' Lude.Text
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

pattern MSCDDisabled :: Mpeg2SceneChangeDetect
pattern MSCDDisabled = Mpeg2SceneChangeDetect' "DISABLED"

pattern MSCDEnabled :: Mpeg2SceneChangeDetect
pattern MSCDEnabled = Mpeg2SceneChangeDetect' "ENABLED"

{-# COMPLETE
  MSCDDisabled,
  MSCDEnabled,
  Mpeg2SceneChangeDetect'
  #-}
