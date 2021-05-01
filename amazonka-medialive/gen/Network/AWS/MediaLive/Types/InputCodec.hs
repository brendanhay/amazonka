{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

-- | codec in increasing order of complexity
newtype InputCodec = InputCodec'
  { fromInputCodec ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
