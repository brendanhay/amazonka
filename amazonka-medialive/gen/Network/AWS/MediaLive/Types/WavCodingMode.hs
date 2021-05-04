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
-- Module      : Network.AWS.MediaLive.Types.WavCodingMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.WavCodingMode
  ( WavCodingMode
      ( ..,
        WavCodingMode_CODING_MODE_1_0,
        WavCodingMode_CODING_MODE_2_0,
        WavCodingMode_CODING_MODE_4_0,
        WavCodingMode_CODING_MODE_8_0
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Wav Coding Mode
newtype WavCodingMode = WavCodingMode'
  { fromWavCodingMode ::
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

pattern WavCodingMode_CODING_MODE_1_0 :: WavCodingMode
pattern WavCodingMode_CODING_MODE_1_0 = WavCodingMode' "CODING_MODE_1_0"

pattern WavCodingMode_CODING_MODE_2_0 :: WavCodingMode
pattern WavCodingMode_CODING_MODE_2_0 = WavCodingMode' "CODING_MODE_2_0"

pattern WavCodingMode_CODING_MODE_4_0 :: WavCodingMode
pattern WavCodingMode_CODING_MODE_4_0 = WavCodingMode' "CODING_MODE_4_0"

pattern WavCodingMode_CODING_MODE_8_0 :: WavCodingMode
pattern WavCodingMode_CODING_MODE_8_0 = WavCodingMode' "CODING_MODE_8_0"

{-# COMPLETE
  WavCodingMode_CODING_MODE_1_0,
  WavCodingMode_CODING_MODE_2_0,
  WavCodingMode_CODING_MODE_4_0,
  WavCodingMode_CODING_MODE_8_0,
  WavCodingMode'
  #-}
