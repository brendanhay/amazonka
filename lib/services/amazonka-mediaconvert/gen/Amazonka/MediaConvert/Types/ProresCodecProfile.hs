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
-- Module      : Amazonka.MediaConvert.Types.ProresCodecProfile
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.ProresCodecProfile
  ( ProresCodecProfile
      ( ..,
        ProresCodecProfile_APPLE_PRORES_422,
        ProresCodecProfile_APPLE_PRORES_422_HQ,
        ProresCodecProfile_APPLE_PRORES_422_LT,
        ProresCodecProfile_APPLE_PRORES_422_PROXY,
        ProresCodecProfile_APPLE_PRORES_4444,
        ProresCodecProfile_APPLE_PRORES_4444_XQ
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Use Profile (ProResCodecProfile) to specify the type of Apple ProRes
-- codec to use for this output.
newtype ProresCodecProfile = ProresCodecProfile'
  { fromProresCodecProfile ::
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

pattern ProresCodecProfile_APPLE_PRORES_422 :: ProresCodecProfile
pattern ProresCodecProfile_APPLE_PRORES_422 = ProresCodecProfile' "APPLE_PRORES_422"

pattern ProresCodecProfile_APPLE_PRORES_422_HQ :: ProresCodecProfile
pattern ProresCodecProfile_APPLE_PRORES_422_HQ = ProresCodecProfile' "APPLE_PRORES_422_HQ"

pattern ProresCodecProfile_APPLE_PRORES_422_LT :: ProresCodecProfile
pattern ProresCodecProfile_APPLE_PRORES_422_LT = ProresCodecProfile' "APPLE_PRORES_422_LT"

pattern ProresCodecProfile_APPLE_PRORES_422_PROXY :: ProresCodecProfile
pattern ProresCodecProfile_APPLE_PRORES_422_PROXY = ProresCodecProfile' "APPLE_PRORES_422_PROXY"

pattern ProresCodecProfile_APPLE_PRORES_4444 :: ProresCodecProfile
pattern ProresCodecProfile_APPLE_PRORES_4444 = ProresCodecProfile' "APPLE_PRORES_4444"

pattern ProresCodecProfile_APPLE_PRORES_4444_XQ :: ProresCodecProfile
pattern ProresCodecProfile_APPLE_PRORES_4444_XQ = ProresCodecProfile' "APPLE_PRORES_4444_XQ"

{-# COMPLETE
  ProresCodecProfile_APPLE_PRORES_422,
  ProresCodecProfile_APPLE_PRORES_422_HQ,
  ProresCodecProfile_APPLE_PRORES_422_LT,
  ProresCodecProfile_APPLE_PRORES_422_PROXY,
  ProresCodecProfile_APPLE_PRORES_4444,
  ProresCodecProfile_APPLE_PRORES_4444_XQ,
  ProresCodecProfile'
  #-}
