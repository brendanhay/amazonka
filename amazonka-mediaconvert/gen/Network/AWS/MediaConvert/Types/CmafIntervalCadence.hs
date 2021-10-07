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
-- Module      : Network.AWS.MediaConvert.Types.CmafIntervalCadence
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CmafIntervalCadence
  ( CmafIntervalCadence
      ( ..,
        CmafIntervalCadence_FOLLOW_CUSTOM,
        CmafIntervalCadence_FOLLOW_IFRAME
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | The cadence MediaConvert follows for generating thumbnails. If set to
-- FOLLOW_IFRAME, MediaConvert generates thumbnails for each IDR frame in
-- the output (matching the GOP cadence). If set to FOLLOW_CUSTOM,
-- MediaConvert generates thumbnails according to the interval you specify
-- in thumbnailInterval.
newtype CmafIntervalCadence = CmafIntervalCadence'
  { fromCmafIntervalCadence ::
      Core.Text
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

pattern CmafIntervalCadence_FOLLOW_CUSTOM :: CmafIntervalCadence
pattern CmafIntervalCadence_FOLLOW_CUSTOM = CmafIntervalCadence' "FOLLOW_CUSTOM"

pattern CmafIntervalCadence_FOLLOW_IFRAME :: CmafIntervalCadence
pattern CmafIntervalCadence_FOLLOW_IFRAME = CmafIntervalCadence' "FOLLOW_IFRAME"

{-# COMPLETE
  CmafIntervalCadence_FOLLOW_CUSTOM,
  CmafIntervalCadence_FOLLOW_IFRAME,
  CmafIntervalCadence'
  #-}
