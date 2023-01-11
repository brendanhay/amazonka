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
-- Module      : Amazonka.MediaConvert.Types.BurninSubtitleTeletextSpacing
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.BurninSubtitleTeletextSpacing
  ( BurninSubtitleTeletextSpacing
      ( ..,
        BurninSubtitleTeletextSpacing_AUTO,
        BurninSubtitleTeletextSpacing_FIXED_GRID,
        BurninSubtitleTeletextSpacing_PROPORTIONAL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specify whether the text spacing (TeletextSpacing) in your captions is
-- set by the captions grid, or varies depending on letter width. Choose
-- fixed grid (FIXED_GRID) to conform to the spacing specified in the
-- captions file more accurately. Choose proportional (PROPORTIONAL) to
-- make the text easier to read for closed captions.
newtype BurninSubtitleTeletextSpacing = BurninSubtitleTeletextSpacing'
  { fromBurninSubtitleTeletextSpacing ::
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

pattern BurninSubtitleTeletextSpacing_AUTO :: BurninSubtitleTeletextSpacing
pattern BurninSubtitleTeletextSpacing_AUTO = BurninSubtitleTeletextSpacing' "AUTO"

pattern BurninSubtitleTeletextSpacing_FIXED_GRID :: BurninSubtitleTeletextSpacing
pattern BurninSubtitleTeletextSpacing_FIXED_GRID = BurninSubtitleTeletextSpacing' "FIXED_GRID"

pattern BurninSubtitleTeletextSpacing_PROPORTIONAL :: BurninSubtitleTeletextSpacing
pattern BurninSubtitleTeletextSpacing_PROPORTIONAL = BurninSubtitleTeletextSpacing' "PROPORTIONAL"

{-# COMPLETE
  BurninSubtitleTeletextSpacing_AUTO,
  BurninSubtitleTeletextSpacing_FIXED_GRID,
  BurninSubtitleTeletextSpacing_PROPORTIONAL,
  BurninSubtitleTeletextSpacing'
  #-}
