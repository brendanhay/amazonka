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
-- Module      : Amazonka.MediaLive.Types.TimecodeBurninFontSize
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.TimecodeBurninFontSize
  ( TimecodeBurninFontSize
      ( ..,
        TimecodeBurninFontSize_EXTRA_SMALL_10,
        TimecodeBurninFontSize_LARGE_48,
        TimecodeBurninFontSize_MEDIUM_32,
        TimecodeBurninFontSize_SMALL_16
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Timecode Burnin Font Size
newtype TimecodeBurninFontSize = TimecodeBurninFontSize'
  { fromTimecodeBurninFontSize ::
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

pattern TimecodeBurninFontSize_EXTRA_SMALL_10 :: TimecodeBurninFontSize
pattern TimecodeBurninFontSize_EXTRA_SMALL_10 = TimecodeBurninFontSize' "EXTRA_SMALL_10"

pattern TimecodeBurninFontSize_LARGE_48 :: TimecodeBurninFontSize
pattern TimecodeBurninFontSize_LARGE_48 = TimecodeBurninFontSize' "LARGE_48"

pattern TimecodeBurninFontSize_MEDIUM_32 :: TimecodeBurninFontSize
pattern TimecodeBurninFontSize_MEDIUM_32 = TimecodeBurninFontSize' "MEDIUM_32"

pattern TimecodeBurninFontSize_SMALL_16 :: TimecodeBurninFontSize
pattern TimecodeBurninFontSize_SMALL_16 = TimecodeBurninFontSize' "SMALL_16"

{-# COMPLETE
  TimecodeBurninFontSize_EXTRA_SMALL_10,
  TimecodeBurninFontSize_LARGE_48,
  TimecodeBurninFontSize_MEDIUM_32,
  TimecodeBurninFontSize_SMALL_16,
  TimecodeBurninFontSize'
  #-}
