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
-- Module      : Amazonka.MediaConvert.Types.DvbSubtitleBackgroundColor
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.DvbSubtitleBackgroundColor
  ( DvbSubtitleBackgroundColor
      ( ..,
        DvbSubtitleBackgroundColor_AUTO,
        DvbSubtitleBackgroundColor_BLACK,
        DvbSubtitleBackgroundColor_NONE,
        DvbSubtitleBackgroundColor_WHITE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specify the color of the rectangle behind the captions. Leave background
-- color (BackgroundColor) blank and set Style passthrough
-- (StylePassthrough) to enabled to use the background color data from your
-- input captions, if present.
newtype DvbSubtitleBackgroundColor = DvbSubtitleBackgroundColor'
  { fromDvbSubtitleBackgroundColor ::
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

pattern DvbSubtitleBackgroundColor_AUTO :: DvbSubtitleBackgroundColor
pattern DvbSubtitleBackgroundColor_AUTO = DvbSubtitleBackgroundColor' "AUTO"

pattern DvbSubtitleBackgroundColor_BLACK :: DvbSubtitleBackgroundColor
pattern DvbSubtitleBackgroundColor_BLACK = DvbSubtitleBackgroundColor' "BLACK"

pattern DvbSubtitleBackgroundColor_NONE :: DvbSubtitleBackgroundColor
pattern DvbSubtitleBackgroundColor_NONE = DvbSubtitleBackgroundColor' "NONE"

pattern DvbSubtitleBackgroundColor_WHITE :: DvbSubtitleBackgroundColor
pattern DvbSubtitleBackgroundColor_WHITE = DvbSubtitleBackgroundColor' "WHITE"

{-# COMPLETE
  DvbSubtitleBackgroundColor_AUTO,
  DvbSubtitleBackgroundColor_BLACK,
  DvbSubtitleBackgroundColor_NONE,
  DvbSubtitleBackgroundColor_WHITE,
  DvbSubtitleBackgroundColor'
  #-}
