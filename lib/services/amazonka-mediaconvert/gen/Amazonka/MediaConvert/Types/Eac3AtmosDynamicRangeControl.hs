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
-- Module      : Amazonka.MediaConvert.Types.Eac3AtmosDynamicRangeControl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Eac3AtmosDynamicRangeControl
  ( Eac3AtmosDynamicRangeControl
      ( ..,
        Eac3AtmosDynamicRangeControl_INITIALIZE_FROM_SOURCE,
        Eac3AtmosDynamicRangeControl_SPECIFIED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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

pattern Eac3AtmosDynamicRangeControl_INITIALIZE_FROM_SOURCE :: Eac3AtmosDynamicRangeControl
pattern Eac3AtmosDynamicRangeControl_INITIALIZE_FROM_SOURCE = Eac3AtmosDynamicRangeControl' "INITIALIZE_FROM_SOURCE"

pattern Eac3AtmosDynamicRangeControl_SPECIFIED :: Eac3AtmosDynamicRangeControl
pattern Eac3AtmosDynamicRangeControl_SPECIFIED = Eac3AtmosDynamicRangeControl' "SPECIFIED"

{-# COMPLETE
  Eac3AtmosDynamicRangeControl_INITIALIZE_FROM_SOURCE,
  Eac3AtmosDynamicRangeControl_SPECIFIED,
  Eac3AtmosDynamicRangeControl'
  #-}
