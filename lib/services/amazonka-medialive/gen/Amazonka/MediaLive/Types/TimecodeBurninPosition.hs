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
-- Module      : Amazonka.MediaLive.Types.TimecodeBurninPosition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.TimecodeBurninPosition
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Timecode Burnin Position
newtype TimecodeBurninPosition = TimecodeBurninPosition'
  { fromTimecodeBurninPosition ::
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
