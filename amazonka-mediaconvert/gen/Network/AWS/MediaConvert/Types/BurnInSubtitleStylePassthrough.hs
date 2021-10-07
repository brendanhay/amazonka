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
-- Module      : Network.AWS.MediaConvert.Types.BurnInSubtitleStylePassthrough
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.BurnInSubtitleStylePassthrough
  ( BurnInSubtitleStylePassthrough
      ( ..,
        BurnInSubtitleStylePassthrough_DISABLED,
        BurnInSubtitleStylePassthrough_ENABLED
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Ignore this setting unless your output captions are burned in. Choose
-- which set of style and position values the service applies to your
-- output captions. When you choose ENABLED, the service uses the input
-- style and position information from your input. When you choose
-- DISABLED, the service uses any style values that you specify in your
-- output settings. If you don\'t specify values, the service uses default
-- style and position values. When you choose DISABLED, the service ignores
-- all style and position values from your input.
newtype BurnInSubtitleStylePassthrough = BurnInSubtitleStylePassthrough'
  { fromBurnInSubtitleStylePassthrough ::
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

pattern BurnInSubtitleStylePassthrough_DISABLED :: BurnInSubtitleStylePassthrough
pattern BurnInSubtitleStylePassthrough_DISABLED = BurnInSubtitleStylePassthrough' "DISABLED"

pattern BurnInSubtitleStylePassthrough_ENABLED :: BurnInSubtitleStylePassthrough
pattern BurnInSubtitleStylePassthrough_ENABLED = BurnInSubtitleStylePassthrough' "ENABLED"

{-# COMPLETE
  BurnInSubtitleStylePassthrough_DISABLED,
  BurnInSubtitleStylePassthrough_ENABLED,
  BurnInSubtitleStylePassthrough'
  #-}
