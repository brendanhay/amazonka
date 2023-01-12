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
-- Module      : Amazonka.MediaConvert.Types.H265CodecProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.H265CodecProfile
  ( H265CodecProfile
      ( ..,
        H265CodecProfile_MAIN10_HIGH,
        H265CodecProfile_MAIN10_MAIN,
        H265CodecProfile_MAIN_422_10BIT_HIGH,
        H265CodecProfile_MAIN_422_10BIT_MAIN,
        H265CodecProfile_MAIN_422_8BIT_HIGH,
        H265CodecProfile_MAIN_422_8BIT_MAIN,
        H265CodecProfile_MAIN_HIGH,
        H265CodecProfile_MAIN_MAIN
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents the Profile and Tier, per the HEVC (H.265) specification.
-- Selections are grouped as [Profile] \/ [Tier], so \"Main\/High\"
-- represents Main Profile with High Tier. 4:2:2 profiles are only
-- available with the HEVC 4:2:2 License.
newtype H265CodecProfile = H265CodecProfile'
  { fromH265CodecProfile ::
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

pattern H265CodecProfile_MAIN10_HIGH :: H265CodecProfile
pattern H265CodecProfile_MAIN10_HIGH = H265CodecProfile' "MAIN10_HIGH"

pattern H265CodecProfile_MAIN10_MAIN :: H265CodecProfile
pattern H265CodecProfile_MAIN10_MAIN = H265CodecProfile' "MAIN10_MAIN"

pattern H265CodecProfile_MAIN_422_10BIT_HIGH :: H265CodecProfile
pattern H265CodecProfile_MAIN_422_10BIT_HIGH = H265CodecProfile' "MAIN_422_10BIT_HIGH"

pattern H265CodecProfile_MAIN_422_10BIT_MAIN :: H265CodecProfile
pattern H265CodecProfile_MAIN_422_10BIT_MAIN = H265CodecProfile' "MAIN_422_10BIT_MAIN"

pattern H265CodecProfile_MAIN_422_8BIT_HIGH :: H265CodecProfile
pattern H265CodecProfile_MAIN_422_8BIT_HIGH = H265CodecProfile' "MAIN_422_8BIT_HIGH"

pattern H265CodecProfile_MAIN_422_8BIT_MAIN :: H265CodecProfile
pattern H265CodecProfile_MAIN_422_8BIT_MAIN = H265CodecProfile' "MAIN_422_8BIT_MAIN"

pattern H265CodecProfile_MAIN_HIGH :: H265CodecProfile
pattern H265CodecProfile_MAIN_HIGH = H265CodecProfile' "MAIN_HIGH"

pattern H265CodecProfile_MAIN_MAIN :: H265CodecProfile
pattern H265CodecProfile_MAIN_MAIN = H265CodecProfile' "MAIN_MAIN"

{-# COMPLETE
  H265CodecProfile_MAIN10_HIGH,
  H265CodecProfile_MAIN10_MAIN,
  H265CodecProfile_MAIN_422_10BIT_HIGH,
  H265CodecProfile_MAIN_422_10BIT_MAIN,
  H265CodecProfile_MAIN_422_8BIT_HIGH,
  H265CodecProfile_MAIN_422_8BIT_MAIN,
  H265CodecProfile_MAIN_HIGH,
  H265CodecProfile_MAIN_MAIN,
  H265CodecProfile'
  #-}
