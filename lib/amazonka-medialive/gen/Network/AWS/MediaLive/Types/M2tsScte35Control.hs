{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M2tsScte35Control
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.M2tsScte35Control
  ( M2tsScte35Control
      ( M2tsScte35Control',
        M2tsScte35ControlNone,
        M2tsScte35ControlPassthrough,
        fromM2tsScte35Control
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | M2ts Scte35 Control
newtype M2tsScte35Control = M2tsScte35Control'
  { fromM2tsScte35Control ::
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

pattern M2tsScte35ControlNone :: M2tsScte35Control
pattern M2tsScte35ControlNone = M2tsScte35Control' "NONE"

pattern M2tsScte35ControlPassthrough :: M2tsScte35Control
pattern M2tsScte35ControlPassthrough = M2tsScte35Control' "PASSTHROUGH"

{-# COMPLETE
  M2tsScte35ControlNone,
  M2tsScte35ControlPassthrough,
  M2tsScte35Control'
  #-}
