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
-- Module      : Network.AWS.MediaConvert.Types.SrtStylePassthrough
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.SrtStylePassthrough
  ( SrtStylePassthrough
      ( ..,
        SrtStylePassthrough_DISABLED,
        SrtStylePassthrough_ENABLED
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Choose Enabled (ENABLED) to have MediaConvert use the font style, color,
-- and position information from the captions source in the input. Keep the
-- default value, Disabled (DISABLED), for simplified output captions.
newtype SrtStylePassthrough = SrtStylePassthrough'
  { fromSrtStylePassthrough ::
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

pattern SrtStylePassthrough_DISABLED :: SrtStylePassthrough
pattern SrtStylePassthrough_DISABLED = SrtStylePassthrough' "DISABLED"

pattern SrtStylePassthrough_ENABLED :: SrtStylePassthrough
pattern SrtStylePassthrough_ENABLED = SrtStylePassthrough' "ENABLED"

{-# COMPLETE
  SrtStylePassthrough_DISABLED,
  SrtStylePassthrough_ENABLED,
  SrtStylePassthrough'
  #-}
