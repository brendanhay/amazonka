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
-- Module      : Network.AWS.MediaConvert.Types.ProresInterlaceMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ProresInterlaceMode
  ( ProresInterlaceMode
      ( ..,
        ProresInterlaceMode_BOTTOM_FIELD,
        ProresInterlaceMode_FOLLOW_BOTTOM_FIELD,
        ProresInterlaceMode_FOLLOW_TOP_FIELD,
        ProresInterlaceMode_PROGRESSIVE,
        ProresInterlaceMode_TOP_FIELD
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

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
newtype ProresInterlaceMode = ProresInterlaceMode'
  { fromProresInterlaceMode ::
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

pattern ProresInterlaceMode_BOTTOM_FIELD :: ProresInterlaceMode
pattern ProresInterlaceMode_BOTTOM_FIELD = ProresInterlaceMode' "BOTTOM_FIELD"

pattern ProresInterlaceMode_FOLLOW_BOTTOM_FIELD :: ProresInterlaceMode
pattern ProresInterlaceMode_FOLLOW_BOTTOM_FIELD = ProresInterlaceMode' "FOLLOW_BOTTOM_FIELD"

pattern ProresInterlaceMode_FOLLOW_TOP_FIELD :: ProresInterlaceMode
pattern ProresInterlaceMode_FOLLOW_TOP_FIELD = ProresInterlaceMode' "FOLLOW_TOP_FIELD"

pattern ProresInterlaceMode_PROGRESSIVE :: ProresInterlaceMode
pattern ProresInterlaceMode_PROGRESSIVE = ProresInterlaceMode' "PROGRESSIVE"

pattern ProresInterlaceMode_TOP_FIELD :: ProresInterlaceMode
pattern ProresInterlaceMode_TOP_FIELD = ProresInterlaceMode' "TOP_FIELD"

{-# COMPLETE
  ProresInterlaceMode_BOTTOM_FIELD,
  ProresInterlaceMode_FOLLOW_BOTTOM_FIELD,
  ProresInterlaceMode_FOLLOW_TOP_FIELD,
  ProresInterlaceMode_PROGRESSIVE,
  ProresInterlaceMode_TOP_FIELD,
  ProresInterlaceMode'
  #-}
