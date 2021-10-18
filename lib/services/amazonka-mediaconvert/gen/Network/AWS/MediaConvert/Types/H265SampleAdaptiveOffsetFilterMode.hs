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
-- Module      : Network.AWS.MediaConvert.Types.H265SampleAdaptiveOffsetFilterMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H265SampleAdaptiveOffsetFilterMode
  ( H265SampleAdaptiveOffsetFilterMode
      ( ..,
        H265SampleAdaptiveOffsetFilterMode_ADAPTIVE,
        H265SampleAdaptiveOffsetFilterMode_DEFAULT,
        H265SampleAdaptiveOffsetFilterMode_OFF
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Specify Sample Adaptive Offset (SAO) filter strength. Adaptive mode
-- dynamically selects best strength based on content
newtype H265SampleAdaptiveOffsetFilterMode = H265SampleAdaptiveOffsetFilterMode'
  { fromH265SampleAdaptiveOffsetFilterMode ::
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

pattern H265SampleAdaptiveOffsetFilterMode_ADAPTIVE :: H265SampleAdaptiveOffsetFilterMode
pattern H265SampleAdaptiveOffsetFilterMode_ADAPTIVE = H265SampleAdaptiveOffsetFilterMode' "ADAPTIVE"

pattern H265SampleAdaptiveOffsetFilterMode_DEFAULT :: H265SampleAdaptiveOffsetFilterMode
pattern H265SampleAdaptiveOffsetFilterMode_DEFAULT = H265SampleAdaptiveOffsetFilterMode' "DEFAULT"

pattern H265SampleAdaptiveOffsetFilterMode_OFF :: H265SampleAdaptiveOffsetFilterMode
pattern H265SampleAdaptiveOffsetFilterMode_OFF = H265SampleAdaptiveOffsetFilterMode' "OFF"

{-# COMPLETE
  H265SampleAdaptiveOffsetFilterMode_ADAPTIVE,
  H265SampleAdaptiveOffsetFilterMode_DEFAULT,
  H265SampleAdaptiveOffsetFilterMode_OFF,
  H265SampleAdaptiveOffsetFilterMode'
  #-}
