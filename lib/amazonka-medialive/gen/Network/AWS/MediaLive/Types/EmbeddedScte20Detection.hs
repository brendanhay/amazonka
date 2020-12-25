{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.EmbeddedScte20Detection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.EmbeddedScte20Detection
  ( EmbeddedScte20Detection
      ( EmbeddedScte20Detection',
        EmbeddedScte20DetectionAuto,
        EmbeddedScte20DetectionOff,
        fromEmbeddedScte20Detection
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Embedded Scte20 Detection
newtype EmbeddedScte20Detection = EmbeddedScte20Detection'
  { fromEmbeddedScte20Detection ::
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

pattern EmbeddedScte20DetectionAuto :: EmbeddedScte20Detection
pattern EmbeddedScte20DetectionAuto = EmbeddedScte20Detection' "AUTO"

pattern EmbeddedScte20DetectionOff :: EmbeddedScte20Detection
pattern EmbeddedScte20DetectionOff = EmbeddedScte20Detection' "OFF"

{-# COMPLETE
  EmbeddedScte20DetectionAuto,
  EmbeddedScte20DetectionOff,
  EmbeddedScte20Detection'
  #-}
