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
-- Module      : Amazonka.MediaLive.Types.AacCodingMode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.AacCodingMode
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Aac Coding Mode
newtype AacCodingMode = AacCodingMode'
  { fromAacCodingMode ::
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
