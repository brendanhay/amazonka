{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H265SceneChangeDetect
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H265SceneChangeDetect
  ( H265SceneChangeDetect
      ( H265SceneChangeDetect',
        HSCDDisabled,
        HSCDEnabled
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | H265 Scene Change Detect
newtype H265SceneChangeDetect = H265SceneChangeDetect' Lude.Text
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

pattern HSCDDisabled :: H265SceneChangeDetect
pattern HSCDDisabled = H265SceneChangeDetect' "DISABLED"

pattern HSCDEnabled :: H265SceneChangeDetect
pattern HSCDEnabled = H265SceneChangeDetect' "ENABLED"

{-# COMPLETE
  HSCDDisabled,
  HSCDEnabled,
  H265SceneChangeDetect'
  #-}
