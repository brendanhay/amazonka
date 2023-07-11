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
-- Module      : Amazonka.MediaLive.Types.H264Profile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.H264Profile
  ( H264Profile
      ( ..,
        H264Profile_BASELINE,
        H264Profile_HIGH,
        H264Profile_HIGH_10BIT,
        H264Profile_HIGH_422,
        H264Profile_HIGH_422_10BIT,
        H264Profile_MAIN
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | H264 Profile
newtype H264Profile = H264Profile'
  { fromH264Profile ::
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

pattern H264Profile_BASELINE :: H264Profile
pattern H264Profile_BASELINE = H264Profile' "BASELINE"

pattern H264Profile_HIGH :: H264Profile
pattern H264Profile_HIGH = H264Profile' "HIGH"

pattern H264Profile_HIGH_10BIT :: H264Profile
pattern H264Profile_HIGH_10BIT = H264Profile' "HIGH_10BIT"

pattern H264Profile_HIGH_422 :: H264Profile
pattern H264Profile_HIGH_422 = H264Profile' "HIGH_422"

pattern H264Profile_HIGH_422_10BIT :: H264Profile
pattern H264Profile_HIGH_422_10BIT = H264Profile' "HIGH_422_10BIT"

pattern H264Profile_MAIN :: H264Profile
pattern H264Profile_MAIN = H264Profile' "MAIN"

{-# COMPLETE
  H264Profile_BASELINE,
  H264Profile_HIGH,
  H264Profile_HIGH_10BIT,
  H264Profile_HIGH_422,
  H264Profile_HIGH_422_10BIT,
  H264Profile_MAIN,
  H264Profile'
  #-}
