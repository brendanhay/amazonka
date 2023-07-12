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
-- Module      : Amazonka.MediaLive.Types.TemporalFilterPostFilterSharpening
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.TemporalFilterPostFilterSharpening
  ( TemporalFilterPostFilterSharpening
      ( ..,
        TemporalFilterPostFilterSharpening_AUTO,
        TemporalFilterPostFilterSharpening_DISABLED,
        TemporalFilterPostFilterSharpening_ENABLED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Temporal Filter Post Filter Sharpening
newtype TemporalFilterPostFilterSharpening = TemporalFilterPostFilterSharpening'
  { fromTemporalFilterPostFilterSharpening ::
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

pattern TemporalFilterPostFilterSharpening_AUTO :: TemporalFilterPostFilterSharpening
pattern TemporalFilterPostFilterSharpening_AUTO = TemporalFilterPostFilterSharpening' "AUTO"

pattern TemporalFilterPostFilterSharpening_DISABLED :: TemporalFilterPostFilterSharpening
pattern TemporalFilterPostFilterSharpening_DISABLED = TemporalFilterPostFilterSharpening' "DISABLED"

pattern TemporalFilterPostFilterSharpening_ENABLED :: TemporalFilterPostFilterSharpening
pattern TemporalFilterPostFilterSharpening_ENABLED = TemporalFilterPostFilterSharpening' "ENABLED"

{-# COMPLETE
  TemporalFilterPostFilterSharpening_AUTO,
  TemporalFilterPostFilterSharpening_DISABLED,
  TemporalFilterPostFilterSharpening_ENABLED,
  TemporalFilterPostFilterSharpening'
  #-}
