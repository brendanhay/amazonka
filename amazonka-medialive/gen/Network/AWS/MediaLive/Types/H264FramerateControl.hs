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

import qualified Network.AWS.Prelude as Prelude

-- | H264 Framerate Control
newtype H264FramerateControl = H264FramerateControl'
  { fromH264FramerateControl ::
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

pattern H264FramerateControl_INITIALIZE_FROM_SOURCE :: H264FramerateControl
pattern H264FramerateControl_INITIALIZE_FROM_SOURCE = H264FramerateControl' "INITIALIZE_FROM_SOURCE"

pattern H264FramerateControl_SPECIFIED :: H264FramerateControl
pattern H264FramerateControl_SPECIFIED = H264FramerateControl' "SPECIFIED"

{-# COMPLETE
  H264FramerateControl_INITIALIZE_FROM_SOURCE,
  H264FramerateControl_SPECIFIED,
  H264FramerateControl'
  #-}
