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
-- Module      : Amazonka.MediaConvert.Types.H264InterlaceMode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.H264InterlaceMode
  ( H264InterlaceMode
      ( ..,
        H264InterlaceMode_BOTTOM_FIELD,
        H264InterlaceMode_FOLLOW_BOTTOM_FIELD,
        H264InterlaceMode_FOLLOW_TOP_FIELD,
        H264InterlaceMode_PROGRESSIVE,
        H264InterlaceMode_TOP_FIELD
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Choose the scan line type for the output. Keep the default value,
-- Progressive (PROGRESSIVE) to create a progressive output, regardless of
-- the scan type of your input. Use Top field first (TOP_FIELD) or Bottom
-- field first (BOTTOM_FIELD) to create an output that\'s interlaced with
-- the same field polarity throughout. Use Follow, default top
-- (FOLLOW_TOP_FIELD) or Follow, default bottom (FOLLOW_BOTTOM_FIELD) to
-- produce outputs with the same field polarity as the source. For jobs
-- that have multiple inputs, the output field polarity might change over
-- the course of the output. Follow behavior depends on the input scan
-- type. If the source is interlaced, the output will be interlaced with
-- the same polarity as the source. If the source is progressive, the
-- output will be interlaced with top field bottom field first, depending
-- on which of the Follow options you choose.
newtype H264InterlaceMode = H264InterlaceMode'
  { fromH264InterlaceMode ::
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

pattern H264InterlaceMode_BOTTOM_FIELD :: H264InterlaceMode
pattern H264InterlaceMode_BOTTOM_FIELD = H264InterlaceMode' "BOTTOM_FIELD"

pattern H264InterlaceMode_FOLLOW_BOTTOM_FIELD :: H264InterlaceMode
pattern H264InterlaceMode_FOLLOW_BOTTOM_FIELD = H264InterlaceMode' "FOLLOW_BOTTOM_FIELD"

pattern H264InterlaceMode_FOLLOW_TOP_FIELD :: H264InterlaceMode
pattern H264InterlaceMode_FOLLOW_TOP_FIELD = H264InterlaceMode' "FOLLOW_TOP_FIELD"

pattern H264InterlaceMode_PROGRESSIVE :: H264InterlaceMode
pattern H264InterlaceMode_PROGRESSIVE = H264InterlaceMode' "PROGRESSIVE"

pattern H264InterlaceMode_TOP_FIELD :: H264InterlaceMode
pattern H264InterlaceMode_TOP_FIELD = H264InterlaceMode' "TOP_FIELD"

{-# COMPLETE
  H264InterlaceMode_BOTTOM_FIELD,
  H264InterlaceMode_FOLLOW_BOTTOM_FIELD,
  H264InterlaceMode_FOLLOW_TOP_FIELD,
  H264InterlaceMode_PROGRESSIVE,
  H264InterlaceMode_TOP_FIELD,
  H264InterlaceMode'
  #-}
