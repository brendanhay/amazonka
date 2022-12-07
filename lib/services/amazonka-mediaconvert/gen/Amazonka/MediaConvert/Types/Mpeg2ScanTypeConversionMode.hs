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
-- Module      : Amazonka.MediaConvert.Types.Mpeg2ScanTypeConversionMode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Mpeg2ScanTypeConversionMode
  ( Mpeg2ScanTypeConversionMode
      ( ..,
        Mpeg2ScanTypeConversionMode_INTERLACED,
        Mpeg2ScanTypeConversionMode_INTERLACED_OPTIMIZE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Use this setting for interlaced outputs, when your output frame rate is
-- half of your input frame rate. In this situation, choose Optimized
-- interlacing (INTERLACED_OPTIMIZE) to create a better quality interlaced
-- output. In this case, each progressive frame from the input corresponds
-- to an interlaced field in the output. Keep the default value, Basic
-- interlacing (INTERLACED), for all other output frame rates. With basic
-- interlacing, MediaConvert performs any frame rate conversion first and
-- then interlaces the frames. When you choose Optimized interlacing and
-- you set your output frame rate to a value that isn\'t suitable for
-- optimized interlacing, MediaConvert automatically falls back to basic
-- interlacing. Required settings: To use optimized interlacing, you must
-- set Telecine (telecine) to None (NONE) or Soft (SOFT). You can\'t use
-- optimized interlacing for hard telecine outputs. You must also set
-- Interlace mode (interlaceMode) to a value other than Progressive
-- (PROGRESSIVE).
newtype Mpeg2ScanTypeConversionMode = Mpeg2ScanTypeConversionMode'
  { fromMpeg2ScanTypeConversionMode ::
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

pattern Mpeg2ScanTypeConversionMode_INTERLACED :: Mpeg2ScanTypeConversionMode
pattern Mpeg2ScanTypeConversionMode_INTERLACED = Mpeg2ScanTypeConversionMode' "INTERLACED"

pattern Mpeg2ScanTypeConversionMode_INTERLACED_OPTIMIZE :: Mpeg2ScanTypeConversionMode
pattern Mpeg2ScanTypeConversionMode_INTERLACED_OPTIMIZE = Mpeg2ScanTypeConversionMode' "INTERLACED_OPTIMIZE"

{-# COMPLETE
  Mpeg2ScanTypeConversionMode_INTERLACED,
  Mpeg2ScanTypeConversionMode_INTERLACED_OPTIMIZE,
  Mpeg2ScanTypeConversionMode'
  #-}
