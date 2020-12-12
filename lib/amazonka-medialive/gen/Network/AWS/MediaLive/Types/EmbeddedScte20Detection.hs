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
        ESDAuto,
        ESDOff
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Embedded Scte20 Detection
newtype EmbeddedScte20Detection = EmbeddedScte20Detection' Lude.Text
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

pattern ESDAuto :: EmbeddedScte20Detection
pattern ESDAuto = EmbeddedScte20Detection' "AUTO"

pattern ESDOff :: EmbeddedScte20Detection
pattern ESDOff = EmbeddedScte20Detection' "OFF"

{-# COMPLETE
  ESDAuto,
  ESDOff,
  EmbeddedScte20Detection'
  #-}
