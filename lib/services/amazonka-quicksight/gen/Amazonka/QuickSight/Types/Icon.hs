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
-- Module      : Amazonka.QuickSight.Types.Icon
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.Icon
  ( Icon
      ( ..,
        Icon_ARROW_DOWN,
        Icon_ARROW_DOWN_LEFT,
        Icon_ARROW_DOWN_RIGHT,
        Icon_ARROW_LEFT,
        Icon_ARROW_RIGHT,
        Icon_ARROW_UP,
        Icon_ARROW_UP_LEFT,
        Icon_ARROW_UP_RIGHT,
        Icon_CARET_DOWN,
        Icon_CARET_UP,
        Icon_CHECKMARK,
        Icon_CIRCLE,
        Icon_FACE_DOWN,
        Icon_FACE_FLAT,
        Icon_FACE_UP,
        Icon_FLAG,
        Icon_MINUS,
        Icon_ONE_BAR,
        Icon_PLUS,
        Icon_SQUARE,
        Icon_THREE_BAR,
        Icon_THUMBS_DOWN,
        Icon_THUMBS_UP,
        Icon_TRIANGLE,
        Icon_TWO_BAR,
        Icon_X
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype Icon = Icon' {fromIcon :: Data.Text}
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

pattern Icon_ARROW_DOWN :: Icon
pattern Icon_ARROW_DOWN = Icon' "ARROW_DOWN"

pattern Icon_ARROW_DOWN_LEFT :: Icon
pattern Icon_ARROW_DOWN_LEFT = Icon' "ARROW_DOWN_LEFT"

pattern Icon_ARROW_DOWN_RIGHT :: Icon
pattern Icon_ARROW_DOWN_RIGHT = Icon' "ARROW_DOWN_RIGHT"

pattern Icon_ARROW_LEFT :: Icon
pattern Icon_ARROW_LEFT = Icon' "ARROW_LEFT"

pattern Icon_ARROW_RIGHT :: Icon
pattern Icon_ARROW_RIGHT = Icon' "ARROW_RIGHT"

pattern Icon_ARROW_UP :: Icon
pattern Icon_ARROW_UP = Icon' "ARROW_UP"

pattern Icon_ARROW_UP_LEFT :: Icon
pattern Icon_ARROW_UP_LEFT = Icon' "ARROW_UP_LEFT"

pattern Icon_ARROW_UP_RIGHT :: Icon
pattern Icon_ARROW_UP_RIGHT = Icon' "ARROW_UP_RIGHT"

pattern Icon_CARET_DOWN :: Icon
pattern Icon_CARET_DOWN = Icon' "CARET_DOWN"

pattern Icon_CARET_UP :: Icon
pattern Icon_CARET_UP = Icon' "CARET_UP"

pattern Icon_CHECKMARK :: Icon
pattern Icon_CHECKMARK = Icon' "CHECKMARK"

pattern Icon_CIRCLE :: Icon
pattern Icon_CIRCLE = Icon' "CIRCLE"

pattern Icon_FACE_DOWN :: Icon
pattern Icon_FACE_DOWN = Icon' "FACE_DOWN"

pattern Icon_FACE_FLAT :: Icon
pattern Icon_FACE_FLAT = Icon' "FACE_FLAT"

pattern Icon_FACE_UP :: Icon
pattern Icon_FACE_UP = Icon' "FACE_UP"

pattern Icon_FLAG :: Icon
pattern Icon_FLAG = Icon' "FLAG"

pattern Icon_MINUS :: Icon
pattern Icon_MINUS = Icon' "MINUS"

pattern Icon_ONE_BAR :: Icon
pattern Icon_ONE_BAR = Icon' "ONE_BAR"

pattern Icon_PLUS :: Icon
pattern Icon_PLUS = Icon' "PLUS"

pattern Icon_SQUARE :: Icon
pattern Icon_SQUARE = Icon' "SQUARE"

pattern Icon_THREE_BAR :: Icon
pattern Icon_THREE_BAR = Icon' "THREE_BAR"

pattern Icon_THUMBS_DOWN :: Icon
pattern Icon_THUMBS_DOWN = Icon' "THUMBS_DOWN"

pattern Icon_THUMBS_UP :: Icon
pattern Icon_THUMBS_UP = Icon' "THUMBS_UP"

pattern Icon_TRIANGLE :: Icon
pattern Icon_TRIANGLE = Icon' "TRIANGLE"

pattern Icon_TWO_BAR :: Icon
pattern Icon_TWO_BAR = Icon' "TWO_BAR"

pattern Icon_X :: Icon
pattern Icon_X = Icon' "X"

{-# COMPLETE
  Icon_ARROW_DOWN,
  Icon_ARROW_DOWN_LEFT,
  Icon_ARROW_DOWN_RIGHT,
  Icon_ARROW_LEFT,
  Icon_ARROW_RIGHT,
  Icon_ARROW_UP,
  Icon_ARROW_UP_LEFT,
  Icon_ARROW_UP_RIGHT,
  Icon_CARET_DOWN,
  Icon_CARET_UP,
  Icon_CHECKMARK,
  Icon_CIRCLE,
  Icon_FACE_DOWN,
  Icon_FACE_FLAT,
  Icon_FACE_UP,
  Icon_FLAG,
  Icon_MINUS,
  Icon_ONE_BAR,
  Icon_PLUS,
  Icon_SQUARE,
  Icon_THREE_BAR,
  Icon_THUMBS_DOWN,
  Icon_THUMBS_UP,
  Icon_TRIANGLE,
  Icon_TWO_BAR,
  Icon_X,
  Icon'
  #-}
