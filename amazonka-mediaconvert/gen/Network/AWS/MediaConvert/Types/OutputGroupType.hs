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
-- Module      : Network.AWS.MediaConvert.Types.OutputGroupType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.OutputGroupType
  ( OutputGroupType
      ( ..,
        OutputGroupType_CMAF_GROUP_SETTINGS,
        OutputGroupType_DASH_ISO_GROUP_SETTINGS,
        OutputGroupType_FILE_GROUP_SETTINGS,
        OutputGroupType_HLS_GROUP_SETTINGS,
        OutputGroupType_MS_SMOOTH_GROUP_SETTINGS
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Type of output group (File group, Apple HLS, DASH ISO, Microsoft Smooth
-- Streaming, CMAF)
newtype OutputGroupType = OutputGroupType'
  { fromOutputGroupType ::
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

pattern OutputGroupType_CMAF_GROUP_SETTINGS :: OutputGroupType
pattern OutputGroupType_CMAF_GROUP_SETTINGS = OutputGroupType' "CMAF_GROUP_SETTINGS"

pattern OutputGroupType_DASH_ISO_GROUP_SETTINGS :: OutputGroupType
pattern OutputGroupType_DASH_ISO_GROUP_SETTINGS = OutputGroupType' "DASH_ISO_GROUP_SETTINGS"

pattern OutputGroupType_FILE_GROUP_SETTINGS :: OutputGroupType
pattern OutputGroupType_FILE_GROUP_SETTINGS = OutputGroupType' "FILE_GROUP_SETTINGS"

pattern OutputGroupType_HLS_GROUP_SETTINGS :: OutputGroupType
pattern OutputGroupType_HLS_GROUP_SETTINGS = OutputGroupType' "HLS_GROUP_SETTINGS"

pattern OutputGroupType_MS_SMOOTH_GROUP_SETTINGS :: OutputGroupType
pattern OutputGroupType_MS_SMOOTH_GROUP_SETTINGS = OutputGroupType' "MS_SMOOTH_GROUP_SETTINGS"

{-# COMPLETE
  OutputGroupType_CMAF_GROUP_SETTINGS,
  OutputGroupType_DASH_ISO_GROUP_SETTINGS,
  OutputGroupType_FILE_GROUP_SETTINGS,
  OutputGroupType_HLS_GROUP_SETTINGS,
  OutputGroupType_MS_SMOOTH_GROUP_SETTINGS,
  OutputGroupType'
  #-}
