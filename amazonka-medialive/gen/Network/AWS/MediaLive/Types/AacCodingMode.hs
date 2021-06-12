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
-- Module      : Network.AWS.MediaLive.Types.AacCodingMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AacCodingMode
  ( AacCodingMode
      ( ..,
        AacCodingMode_AD_RECEIVER_MIX,
        AacCodingMode_CODING_MODE_1_0,
        AacCodingMode_CODING_MODE_1_1,
        AacCodingMode_CODING_MODE_2_0,
        AacCodingMode_CODING_MODE_5_1
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Aac Coding Mode
newtype AacCodingMode = AacCodingMode'
  { fromAacCodingMode ::
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

pattern AacCodingMode_AD_RECEIVER_MIX :: AacCodingMode
pattern AacCodingMode_AD_RECEIVER_MIX = AacCodingMode' "AD_RECEIVER_MIX"

pattern AacCodingMode_CODING_MODE_1_0 :: AacCodingMode
pattern AacCodingMode_CODING_MODE_1_0 = AacCodingMode' "CODING_MODE_1_0"

pattern AacCodingMode_CODING_MODE_1_1 :: AacCodingMode
pattern AacCodingMode_CODING_MODE_1_1 = AacCodingMode' "CODING_MODE_1_1"

pattern AacCodingMode_CODING_MODE_2_0 :: AacCodingMode
pattern AacCodingMode_CODING_MODE_2_0 = AacCodingMode' "CODING_MODE_2_0"

pattern AacCodingMode_CODING_MODE_5_1 :: AacCodingMode
pattern AacCodingMode_CODING_MODE_5_1 = AacCodingMode' "CODING_MODE_5_1"

{-# COMPLETE
  AacCodingMode_AD_RECEIVER_MIX,
  AacCodingMode_CODING_MODE_1_0,
  AacCodingMode_CODING_MODE_1_1,
  AacCodingMode_CODING_MODE_2_0,
  AacCodingMode_CODING_MODE_5_1,
  AacCodingMode'
  #-}
