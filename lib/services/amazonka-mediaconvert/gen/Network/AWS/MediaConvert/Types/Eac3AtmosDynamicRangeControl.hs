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
-- Module      : Network.AWS.MediaConvert.Types.Eac3AtmosDynamicRangeControl
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3AtmosDynamicRangeControl
  ( Eac3AtmosDynamicRangeControl
      ( ..,
        Eac3AtmosDynamicRangeControl_INITIALIZE_FROM_SOURCE,
        Eac3AtmosDynamicRangeControl_SPECIFIED
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Specify whether MediaConvert should use any dynamic range control
-- metadata from your input file. Keep the default value, Custom
-- (SPECIFIED), to provide dynamic range control values in your job
-- settings. Choose Follow source (INITIALIZE_FROM_SOURCE) to use the
-- metadata from your input. Related settings--Use these settings to
-- specify your dynamic range control values: Dynamic range compression
-- line (DynamicRangeCompressionLine) and Dynamic range compression RF
-- (DynamicRangeCompressionRf). When you keep the value Custom (SPECIFIED)
-- for Dynamic range control (DynamicRangeControl) and you don\'t specify
-- values for the related settings, MediaConvert uses default values for
-- those settings.
newtype Eac3AtmosDynamicRangeControl = Eac3AtmosDynamicRangeControl'
  { fromEac3AtmosDynamicRangeControl ::
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

pattern Eac3AtmosDynamicRangeControl_INITIALIZE_FROM_SOURCE :: Eac3AtmosDynamicRangeControl
pattern Eac3AtmosDynamicRangeControl_INITIALIZE_FROM_SOURCE = Eac3AtmosDynamicRangeControl' "INITIALIZE_FROM_SOURCE"

pattern Eac3AtmosDynamicRangeControl_SPECIFIED :: Eac3AtmosDynamicRangeControl
pattern Eac3AtmosDynamicRangeControl_SPECIFIED = Eac3AtmosDynamicRangeControl' "SPECIFIED"

{-# COMPLETE
  Eac3AtmosDynamicRangeControl_INITIALIZE_FROM_SOURCE,
  Eac3AtmosDynamicRangeControl_SPECIFIED,
  Eac3AtmosDynamicRangeControl'
  #-}
