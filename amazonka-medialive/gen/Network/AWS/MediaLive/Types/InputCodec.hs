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
-- Module      : Network.AWS.MediaLive.Types.InputCodec
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputCodec
  ( InputCodec
      ( ..,
        InputCodec_AVC,
        InputCodec_HEVC,
        InputCodec_MPEG2
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | codec in increasing order of complexity
newtype InputCodec = InputCodec'
  { fromInputCodec ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern InputCodec_AVC :: InputCodec
pattern InputCodec_AVC = InputCodec' "AVC"

pattern InputCodec_HEVC :: InputCodec
pattern InputCodec_HEVC = InputCodec' "HEVC"

pattern InputCodec_MPEG2 :: InputCodec
pattern InputCodec_MPEG2 = InputCodec' "MPEG2"

{-# COMPLETE
  InputCodec_AVC,
  InputCodec_HEVC,
  InputCodec_MPEG2,
  InputCodec'
  #-}
