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
-- Module      : Amazonka.QuickSight.Types.ConditionalFormattingIconSetType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ConditionalFormattingIconSetType
  ( ConditionalFormattingIconSetType
      ( ..,
        ConditionalFormattingIconSetType_BARS,
        ConditionalFormattingIconSetType_CARET_UP_MINUS_DOWN,
        ConditionalFormattingIconSetType_CHECK_X,
        ConditionalFormattingIconSetType_FLAGS,
        ConditionalFormattingIconSetType_FOUR_COLOR_ARROW,
        ConditionalFormattingIconSetType_FOUR_GRAY_ARROW,
        ConditionalFormattingIconSetType_PLUS_MINUS,
        ConditionalFormattingIconSetType_THREE_CIRCLE,
        ConditionalFormattingIconSetType_THREE_COLOR_ARROW,
        ConditionalFormattingIconSetType_THREE_GRAY_ARROW,
        ConditionalFormattingIconSetType_THREE_SHAPE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ConditionalFormattingIconSetType = ConditionalFormattingIconSetType'
  { fromConditionalFormattingIconSetType ::
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

pattern ConditionalFormattingIconSetType_BARS :: ConditionalFormattingIconSetType
pattern ConditionalFormattingIconSetType_BARS = ConditionalFormattingIconSetType' "BARS"

pattern ConditionalFormattingIconSetType_CARET_UP_MINUS_DOWN :: ConditionalFormattingIconSetType
pattern ConditionalFormattingIconSetType_CARET_UP_MINUS_DOWN = ConditionalFormattingIconSetType' "CARET_UP_MINUS_DOWN"

pattern ConditionalFormattingIconSetType_CHECK_X :: ConditionalFormattingIconSetType
pattern ConditionalFormattingIconSetType_CHECK_X = ConditionalFormattingIconSetType' "CHECK_X"

pattern ConditionalFormattingIconSetType_FLAGS :: ConditionalFormattingIconSetType
pattern ConditionalFormattingIconSetType_FLAGS = ConditionalFormattingIconSetType' "FLAGS"

pattern ConditionalFormattingIconSetType_FOUR_COLOR_ARROW :: ConditionalFormattingIconSetType
pattern ConditionalFormattingIconSetType_FOUR_COLOR_ARROW = ConditionalFormattingIconSetType' "FOUR_COLOR_ARROW"

pattern ConditionalFormattingIconSetType_FOUR_GRAY_ARROW :: ConditionalFormattingIconSetType
pattern ConditionalFormattingIconSetType_FOUR_GRAY_ARROW = ConditionalFormattingIconSetType' "FOUR_GRAY_ARROW"

pattern ConditionalFormattingIconSetType_PLUS_MINUS :: ConditionalFormattingIconSetType
pattern ConditionalFormattingIconSetType_PLUS_MINUS = ConditionalFormattingIconSetType' "PLUS_MINUS"

pattern ConditionalFormattingIconSetType_THREE_CIRCLE :: ConditionalFormattingIconSetType
pattern ConditionalFormattingIconSetType_THREE_CIRCLE = ConditionalFormattingIconSetType' "THREE_CIRCLE"

pattern ConditionalFormattingIconSetType_THREE_COLOR_ARROW :: ConditionalFormattingIconSetType
pattern ConditionalFormattingIconSetType_THREE_COLOR_ARROW = ConditionalFormattingIconSetType' "THREE_COLOR_ARROW"

pattern ConditionalFormattingIconSetType_THREE_GRAY_ARROW :: ConditionalFormattingIconSetType
pattern ConditionalFormattingIconSetType_THREE_GRAY_ARROW = ConditionalFormattingIconSetType' "THREE_GRAY_ARROW"

pattern ConditionalFormattingIconSetType_THREE_SHAPE :: ConditionalFormattingIconSetType
pattern ConditionalFormattingIconSetType_THREE_SHAPE = ConditionalFormattingIconSetType' "THREE_SHAPE"

{-# COMPLETE
  ConditionalFormattingIconSetType_BARS,
  ConditionalFormattingIconSetType_CARET_UP_MINUS_DOWN,
  ConditionalFormattingIconSetType_CHECK_X,
  ConditionalFormattingIconSetType_FLAGS,
  ConditionalFormattingIconSetType_FOUR_COLOR_ARROW,
  ConditionalFormattingIconSetType_FOUR_GRAY_ARROW,
  ConditionalFormattingIconSetType_PLUS_MINUS,
  ConditionalFormattingIconSetType_THREE_CIRCLE,
  ConditionalFormattingIconSetType_THREE_COLOR_ARROW,
  ConditionalFormattingIconSetType_THREE_GRAY_ARROW,
  ConditionalFormattingIconSetType_THREE_SHAPE,
  ConditionalFormattingIconSetType'
  #-}
