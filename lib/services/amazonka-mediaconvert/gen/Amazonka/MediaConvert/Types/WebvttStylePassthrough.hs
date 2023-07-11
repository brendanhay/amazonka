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
-- Module      : Amazonka.MediaConvert.Types.WebvttStylePassthrough
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.WebvttStylePassthrough
  ( WebvttStylePassthrough
      ( ..,
        WebvttStylePassthrough_DISABLED,
        WebvttStylePassthrough_ENABLED,
        WebvttStylePassthrough_STRICT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | To use the available style, color, and position information from your
-- input captions: Set Style passthrough (stylePassthrough) to Enabled
-- (ENABLED). MediaConvert uses default settings when style and position
-- information is missing from your input captions. To recreate the input
-- captions exactly: Set Style passthrough to Strict (STRICT). MediaConvert
-- automatically applies timing adjustments, including adjustments for
-- frame rate conversion, ad avails, and input clipping. Your input
-- captions format must be WebVTT. To ignore the style and position
-- information from your input captions and use simplified output captions:
-- Set Style passthrough to Disabled (DISABLED), or leave blank.
newtype WebvttStylePassthrough = WebvttStylePassthrough'
  { fromWebvttStylePassthrough ::
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

pattern WebvttStylePassthrough_DISABLED :: WebvttStylePassthrough
pattern WebvttStylePassthrough_DISABLED = WebvttStylePassthrough' "DISABLED"

pattern WebvttStylePassthrough_ENABLED :: WebvttStylePassthrough
pattern WebvttStylePassthrough_ENABLED = WebvttStylePassthrough' "ENABLED"

pattern WebvttStylePassthrough_STRICT :: WebvttStylePassthrough
pattern WebvttStylePassthrough_STRICT = WebvttStylePassthrough' "STRICT"

{-# COMPLETE
  WebvttStylePassthrough_DISABLED,
  WebvttStylePassthrough_ENABLED,
  WebvttStylePassthrough_STRICT,
  WebvttStylePassthrough'
  #-}
