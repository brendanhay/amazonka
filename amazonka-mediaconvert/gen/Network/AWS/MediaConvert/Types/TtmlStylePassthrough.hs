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
-- Module      : Network.AWS.MediaConvert.Types.TtmlStylePassthrough
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.TtmlStylePassthrough
  ( TtmlStylePassthrough
      ( ..,
        TtmlStylePassthrough_DISABLED,
        TtmlStylePassthrough_ENABLED
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Pass through style and position information from a TTML-like input
-- source (TTML, SMPTE-TT) to the TTML output.
newtype TtmlStylePassthrough = TtmlStylePassthrough'
  { fromTtmlStylePassthrough ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern TtmlStylePassthrough_DISABLED :: TtmlStylePassthrough
pattern TtmlStylePassthrough_DISABLED = TtmlStylePassthrough' "DISABLED"

pattern TtmlStylePassthrough_ENABLED :: TtmlStylePassthrough
pattern TtmlStylePassthrough_ENABLED = TtmlStylePassthrough' "ENABLED"

{-# COMPLETE
  TtmlStylePassthrough_DISABLED,
  TtmlStylePassthrough_ENABLED,
  TtmlStylePassthrough'
  #-}
