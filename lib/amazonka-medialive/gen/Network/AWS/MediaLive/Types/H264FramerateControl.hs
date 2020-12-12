{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264FramerateControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H264FramerateControl
  ( H264FramerateControl
      ( H264FramerateControl',
        HFCInitializeFromSource,
        HFCSpecified
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | H264 Framerate Control
newtype H264FramerateControl = H264FramerateControl' Lude.Text
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

pattern HFCInitializeFromSource :: H264FramerateControl
pattern HFCInitializeFromSource = H264FramerateControl' "INITIALIZE_FROM_SOURCE"

pattern HFCSpecified :: H264FramerateControl
pattern HFCSpecified = H264FramerateControl' "SPECIFIED"

{-# COMPLETE
  HFCInitializeFromSource,
  HFCSpecified,
  H264FramerateControl'
  #-}
