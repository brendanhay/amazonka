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
-- Module      : Network.AWS.MediaLive.Types.H264FramerateControl
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H264FramerateControl
  ( H264FramerateControl
      ( ..,
        H264FramerateControl_INITIALIZE_FROM_SOURCE,
        H264FramerateControl_SPECIFIED
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | H264 Framerate Control
newtype H264FramerateControl = H264FramerateControl'
  { fromH264FramerateControl ::
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

pattern H264FramerateControl_INITIALIZE_FROM_SOURCE :: H264FramerateControl
pattern H264FramerateControl_INITIALIZE_FROM_SOURCE = H264FramerateControl' "INITIALIZE_FROM_SOURCE"

pattern H264FramerateControl_SPECIFIED :: H264FramerateControl
pattern H264FramerateControl_SPECIFIED = H264FramerateControl' "SPECIFIED"

{-# COMPLETE
  H264FramerateControl_INITIALIZE_FROM_SOURCE,
  H264FramerateControl_SPECIFIED,
  H264FramerateControl'
  #-}
