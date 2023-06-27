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
-- Module      : Amazonka.MediaLive.Types.NielsenWatermarkTimezones
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.NielsenWatermarkTimezones
  ( NielsenWatermarkTimezones
      ( ..,
        NielsenWatermarkTimezones_AMERICA_PUERTO_RICO,
        NielsenWatermarkTimezones_US_ALASKA,
        NielsenWatermarkTimezones_US_ARIZONA,
        NielsenWatermarkTimezones_US_CENTRAL,
        NielsenWatermarkTimezones_US_EASTERN,
        NielsenWatermarkTimezones_US_HAWAII,
        NielsenWatermarkTimezones_US_MOUNTAIN,
        NielsenWatermarkTimezones_US_PACIFIC,
        NielsenWatermarkTimezones_US_SAMOA,
        NielsenWatermarkTimezones_UTC
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Nielsen Watermark Timezones
newtype NielsenWatermarkTimezones = NielsenWatermarkTimezones'
  { fromNielsenWatermarkTimezones ::
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

pattern NielsenWatermarkTimezones_AMERICA_PUERTO_RICO :: NielsenWatermarkTimezones
pattern NielsenWatermarkTimezones_AMERICA_PUERTO_RICO = NielsenWatermarkTimezones' "AMERICA_PUERTO_RICO"

pattern NielsenWatermarkTimezones_US_ALASKA :: NielsenWatermarkTimezones
pattern NielsenWatermarkTimezones_US_ALASKA = NielsenWatermarkTimezones' "US_ALASKA"

pattern NielsenWatermarkTimezones_US_ARIZONA :: NielsenWatermarkTimezones
pattern NielsenWatermarkTimezones_US_ARIZONA = NielsenWatermarkTimezones' "US_ARIZONA"

pattern NielsenWatermarkTimezones_US_CENTRAL :: NielsenWatermarkTimezones
pattern NielsenWatermarkTimezones_US_CENTRAL = NielsenWatermarkTimezones' "US_CENTRAL"

pattern NielsenWatermarkTimezones_US_EASTERN :: NielsenWatermarkTimezones
pattern NielsenWatermarkTimezones_US_EASTERN = NielsenWatermarkTimezones' "US_EASTERN"

pattern NielsenWatermarkTimezones_US_HAWAII :: NielsenWatermarkTimezones
pattern NielsenWatermarkTimezones_US_HAWAII = NielsenWatermarkTimezones' "US_HAWAII"

pattern NielsenWatermarkTimezones_US_MOUNTAIN :: NielsenWatermarkTimezones
pattern NielsenWatermarkTimezones_US_MOUNTAIN = NielsenWatermarkTimezones' "US_MOUNTAIN"

pattern NielsenWatermarkTimezones_US_PACIFIC :: NielsenWatermarkTimezones
pattern NielsenWatermarkTimezones_US_PACIFIC = NielsenWatermarkTimezones' "US_PACIFIC"

pattern NielsenWatermarkTimezones_US_SAMOA :: NielsenWatermarkTimezones
pattern NielsenWatermarkTimezones_US_SAMOA = NielsenWatermarkTimezones' "US_SAMOA"

pattern NielsenWatermarkTimezones_UTC :: NielsenWatermarkTimezones
pattern NielsenWatermarkTimezones_UTC = NielsenWatermarkTimezones' "UTC"

{-# COMPLETE
  NielsenWatermarkTimezones_AMERICA_PUERTO_RICO,
  NielsenWatermarkTimezones_US_ALASKA,
  NielsenWatermarkTimezones_US_ARIZONA,
  NielsenWatermarkTimezones_US_CENTRAL,
  NielsenWatermarkTimezones_US_EASTERN,
  NielsenWatermarkTimezones_US_HAWAII,
  NielsenWatermarkTimezones_US_MOUNTAIN,
  NielsenWatermarkTimezones_US_PACIFIC,
  NielsenWatermarkTimezones_US_SAMOA,
  NielsenWatermarkTimezones_UTC,
  NielsenWatermarkTimezones'
  #-}
