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
-- Module      : Amazonka.MediaConvert.Types.HlsIntervalCadence
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.HlsIntervalCadence
  ( HlsIntervalCadence
      ( ..,
        HlsIntervalCadence_FOLLOW_CUSTOM,
        HlsIntervalCadence_FOLLOW_IFRAME
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The cadence MediaConvert follows for generating thumbnails. If set to
-- FOLLOW_IFRAME, MediaConvert generates thumbnails for each IDR frame in
-- the output (matching the GOP cadence). If set to FOLLOW_CUSTOM,
-- MediaConvert generates thumbnails according to the interval you specify
-- in thumbnailInterval.
newtype HlsIntervalCadence = HlsIntervalCadence'
  { fromHlsIntervalCadence ::
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

pattern HlsIntervalCadence_FOLLOW_CUSTOM :: HlsIntervalCadence
pattern HlsIntervalCadence_FOLLOW_CUSTOM = HlsIntervalCadence' "FOLLOW_CUSTOM"

pattern HlsIntervalCadence_FOLLOW_IFRAME :: HlsIntervalCadence
pattern HlsIntervalCadence_FOLLOW_IFRAME = HlsIntervalCadence' "FOLLOW_IFRAME"

{-# COMPLETE
  HlsIntervalCadence_FOLLOW_CUSTOM,
  HlsIntervalCadence_FOLLOW_IFRAME,
  HlsIntervalCadence'
  #-}
