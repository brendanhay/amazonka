{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264SceneChangeDetect
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H264SceneChangeDetect
  ( H264SceneChangeDetect
      ( H264SceneChangeDetect',
        HDisabled,
        HEnabled
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | H264 Scene Change Detect
newtype H264SceneChangeDetect = H264SceneChangeDetect' Lude.Text
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

pattern HDisabled :: H264SceneChangeDetect
pattern HDisabled = H264SceneChangeDetect' "DISABLED"

pattern HEnabled :: H264SceneChangeDetect
pattern HEnabled = H264SceneChangeDetect' "ENABLED"

{-# COMPLETE
  HDisabled,
  HEnabled,
  H264SceneChangeDetect'
  #-}
