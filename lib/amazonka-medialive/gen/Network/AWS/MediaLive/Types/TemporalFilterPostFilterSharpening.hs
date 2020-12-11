-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.TemporalFilterPostFilterSharpening
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.TemporalFilterPostFilterSharpening
  ( TemporalFilterPostFilterSharpening
      ( TemporalFilterPostFilterSharpening',
        TFPFSAuto,
        TFPFSDisabled,
        TFPFSEnabled
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Temporal Filter Post Filter Sharpening
newtype TemporalFilterPostFilterSharpening = TemporalFilterPostFilterSharpening' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern TFPFSAuto :: TemporalFilterPostFilterSharpening
pattern TFPFSAuto = TemporalFilterPostFilterSharpening' "AUTO"

pattern TFPFSDisabled :: TemporalFilterPostFilterSharpening
pattern TFPFSDisabled = TemporalFilterPostFilterSharpening' "DISABLED"

pattern TFPFSEnabled :: TemporalFilterPostFilterSharpening
pattern TFPFSEnabled = TemporalFilterPostFilterSharpening' "ENABLED"

{-# COMPLETE
  TFPFSAuto,
  TFPFSDisabled,
  TFPFSEnabled,
  TemporalFilterPostFilterSharpening'
  #-}
