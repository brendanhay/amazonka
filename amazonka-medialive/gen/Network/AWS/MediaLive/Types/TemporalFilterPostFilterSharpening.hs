{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.TemporalFilterPostFilterSharpening
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.TemporalFilterPostFilterSharpening
  ( TemporalFilterPostFilterSharpening
      ( ..,
        TemporalFilterPostFilterSharpening_AUTO,
        TemporalFilterPostFilterSharpening_DISABLED,
        TemporalFilterPostFilterSharpening_ENABLED
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Temporal Filter Post Filter Sharpening
newtype TemporalFilterPostFilterSharpening = TemporalFilterPostFilterSharpening'
  { fromTemporalFilterPostFilterSharpening ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
