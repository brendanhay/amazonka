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
-- Module      : Amazonka.MediaConvert.Types.DolbyVisionProfile
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.DolbyVisionProfile
  ( DolbyVisionProfile
      ( ..,
        DolbyVisionProfile_PROFILE_5,
        DolbyVisionProfile_PROFILE_8_1
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Required when you enable Dolby Vision. Use Profile 5 to include
-- frame-interleaved Dolby Vision metadata in your output. Your input must
-- include Dolby Vision metadata or an HDR10 YUV color space. Use Profile
-- 8.1 to include frame-interleaved Dolby Vision metadata and HDR10
-- metadata in your output. Your input must include Dolby Vision metadata.
newtype DolbyVisionProfile = DolbyVisionProfile'
  { fromDolbyVisionProfile ::
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

pattern DolbyVisionProfile_PROFILE_5 :: DolbyVisionProfile
pattern DolbyVisionProfile_PROFILE_5 = DolbyVisionProfile' "PROFILE_5"

pattern DolbyVisionProfile_PROFILE_8_1 :: DolbyVisionProfile
pattern DolbyVisionProfile_PROFILE_8_1 = DolbyVisionProfile' "PROFILE_8_1"

{-# COMPLETE
  DolbyVisionProfile_PROFILE_5,
  DolbyVisionProfile_PROFILE_8_1,
  DolbyVisionProfile'
  #-}
