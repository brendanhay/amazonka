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
-- Module      : Network.AWS.MediaConvert.Types.TimecodeBurninPosition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.TimecodeBurninPosition
  ( TimecodeBurninPosition
      ( ..,
        TimecodeBurninPosition_BOTTOM_CENTER,
        TimecodeBurninPosition_BOTTOM_LEFT,
        TimecodeBurninPosition_BOTTOM_RIGHT,
        TimecodeBurninPosition_MIDDLE_CENTER,
        TimecodeBurninPosition_MIDDLE_LEFT,
        TimecodeBurninPosition_MIDDLE_RIGHT,
        TimecodeBurninPosition_TOP_CENTER,
        TimecodeBurninPosition_TOP_LEFT,
        TimecodeBurninPosition_TOP_RIGHT
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Use Position (Position) under under Timecode burn-in (TimecodeBurnIn) to
-- specify the location the burned-in timecode on output video.
newtype TimecodeBurninPosition = TimecodeBurninPosition'
  { fromTimecodeBurninPosition ::
      Core.Text
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

pattern TimecodeBurninPosition_BOTTOM_CENTER :: TimecodeBurninPosition
pattern TimecodeBurninPosition_BOTTOM_CENTER = TimecodeBurninPosition' "BOTTOM_CENTER"

pattern TimecodeBurninPosition_BOTTOM_LEFT :: TimecodeBurninPosition
pattern TimecodeBurninPosition_BOTTOM_LEFT = TimecodeBurninPosition' "BOTTOM_LEFT"

pattern TimecodeBurninPosition_BOTTOM_RIGHT :: TimecodeBurninPosition
pattern TimecodeBurninPosition_BOTTOM_RIGHT = TimecodeBurninPosition' "BOTTOM_RIGHT"

pattern TimecodeBurninPosition_MIDDLE_CENTER :: TimecodeBurninPosition
pattern TimecodeBurninPosition_MIDDLE_CENTER = TimecodeBurninPosition' "MIDDLE_CENTER"

pattern TimecodeBurninPosition_MIDDLE_LEFT :: TimecodeBurninPosition
pattern TimecodeBurninPosition_MIDDLE_LEFT = TimecodeBurninPosition' "MIDDLE_LEFT"

pattern TimecodeBurninPosition_MIDDLE_RIGHT :: TimecodeBurninPosition
pattern TimecodeBurninPosition_MIDDLE_RIGHT = TimecodeBurninPosition' "MIDDLE_RIGHT"

pattern TimecodeBurninPosition_TOP_CENTER :: TimecodeBurninPosition
pattern TimecodeBurninPosition_TOP_CENTER = TimecodeBurninPosition' "TOP_CENTER"

pattern TimecodeBurninPosition_TOP_LEFT :: TimecodeBurninPosition
pattern TimecodeBurninPosition_TOP_LEFT = TimecodeBurninPosition' "TOP_LEFT"

pattern TimecodeBurninPosition_TOP_RIGHT :: TimecodeBurninPosition
pattern TimecodeBurninPosition_TOP_RIGHT = TimecodeBurninPosition' "TOP_RIGHT"

{-# COMPLETE
  TimecodeBurninPosition_BOTTOM_CENTER,
  TimecodeBurninPosition_BOTTOM_LEFT,
  TimecodeBurninPosition_BOTTOM_RIGHT,
  TimecodeBurninPosition_MIDDLE_CENTER,
  TimecodeBurninPosition_MIDDLE_LEFT,
  TimecodeBurninPosition_MIDDLE_RIGHT,
  TimecodeBurninPosition_TOP_CENTER,
  TimecodeBurninPosition_TOP_LEFT,
  TimecodeBurninPosition_TOP_RIGHT,
  TimecodeBurninPosition'
  #-}
