{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265TemporalIds
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H265TemporalIds
  ( H265TemporalIds
      ( H265TemporalIds',
        H265TemporalIdsDisabled,
        H265TemporalIdsEnabled,
        fromH265TemporalIds
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Enables temporal layer identifiers in the encoded bitstream. Up to 3 layers are supported depending on GOP structure: I- and P-frames form one layer, reference B-frames can form a second layer and non-reference b-frames can form a third layer. Decoders can optionally decode only the lower temporal layers to generate a lower frame rate output. For example, given a bitstream with temporal IDs and with b-frames = 1 (i.e. IbPbPb display order), a decoder could decode all the frames for full frame rate output or only the I and P frames (lowest temporal layer) for a half frame rate output.
newtype H265TemporalIds = H265TemporalIds'
  { fromH265TemporalIds ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern H265TemporalIdsDisabled :: H265TemporalIds
pattern H265TemporalIdsDisabled = H265TemporalIds' "DISABLED"

pattern H265TemporalIdsEnabled :: H265TemporalIds
pattern H265TemporalIdsEnabled = H265TemporalIds' "ENABLED"

{-# COMPLETE
  H265TemporalIdsDisabled,
  H265TemporalIdsEnabled,
  H265TemporalIds'
  #-}
